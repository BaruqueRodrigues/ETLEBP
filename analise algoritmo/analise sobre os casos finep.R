library(ETLEBP)
library(tidyverse)
finep <- cria_base_intermediaria_finep()

sqlite <- here::here("C:/Users/quemu/Desktop/EIP_20210415.db")

finep <- finep %>% dplyr::mutate(titulo_projeto2 = abjutils::rm_accent(titulo_projeto))

finep <- dplyr::left_join(finep, consulta[,c ("título", "cat2") ],
                         by = c("titulo_projeto2"= "título"))  %>%
  unique() %>%
  dplyr::rename(categoria_sqlite = cat2)



tbl_dm_projeto <- tbl_dm_projeto %>% mutate(título = stringr::str_trim(título))

finep <- finep %>%
  dplyr::mutate(existe = ifelse(titulo_projeto %in% tbl_dm_projeto$'título',
                                "sim",
                                "não"))
finep %>% filter(existe == "sim") %>% janitor::tabyl(categorias )

finep_acerto <- finep %>% filter(existe == "sim") %>% rowwise() %>%
  mutate(categorias_split = str_split(categorias, pattern = ",")) %>%
  ungroup() %>% unnest(categorias_split_un =categorias_split) %>%
  mutate(categorias_split_un = str_remove_all(categorias_split_un, '\\"')) %>%
  select(id,categoria_sqlite,  categorias_split_un)

finep_acerto_res <- finep_acerto %>%
  mutate(correto = categorias_split_un == categoria_sqlite ) %>% group_by(id) %>%
  summarise(correto = mean(correto))

mean(finep_acerto_res$correto,  na.rm = T)

finep_ebp1 <- readr::read_delim("analise algoritmo/bases 1 versao ebp/2.FINEP.csv",
                                delim = ";", escape_double = FALSE, trim_ws = TRUE,
                                skip = 5) %>%
  janitor::clean_names() %>%
  rename_with(
    .fn = ~stringr::str_glue("{.x}_2019")
  ) %>%
  dplyr::mutate(
    prazo_utilizacao_2019 = lubridate::ymd(lubridate::dmy(prazo_utilizacao_2019))
    ) %>%
  dplyr::filter(instrumento_2019 == "Reembolsável",
    prazo_utilizacao_2019 >= "2013-01-01") %>%
  tidyr::drop_na(valor_finep_2019) %>%
  mutate(origem = "base_bruta_2019")

finep_brutos <- rio::import(here::here("data/FINEP/14_09_2021_Liberacoes.ods"),
                            skip = 5,
                            sheet = 1) %>%
  janitor::clean_names() %>%
  mutate(
    data_liberacao   = lubridate::ymd(lubridate::dmy(data_liberacao)),
    data_assinatura  = lubridate::ymd(lubridate::dmy(data_assinatura)),
    prazo_utilizacao = lubridate::ymd(lubridate::dmy(prazo_utilizacao))
  ) %>%
  rename_with(
    .fn = ~str_glue("{.x}_2021")
  )

finep_brutos <- finep_brutos %>%  dplyr::filter(
  prazo_utilizacao_2021 >= "2013-01-01")

finep_brutos <- finep_brutos %>%
  tidyr::drop_na(valor_finep_2021)

finep_sobra_2021 <- finep_brutos %>% tidylog::anti_join(
  finep_ebp1,
  by = c("contrato_2021" = "contrato_2019")
)

finep_sobra_2021 <- finep_sobra_2021 %>% mutate(
  motor            = tolower(stringi::stri_trans_general(titulo_2021, "Latin-ASCII")),
  contrato2 = paste(contrato_2021, 1:nrow(finep_sobra_2021)))

finep_sobra_2021 <- dtc_categorias(finep_sobra_2021, processo = contrato2, motor = motor)

finep_sobra_2021$categorias %>% table()

#criando um objeto com informações sobre as categorias de IEA
consulta <- dplyr::select(tbl_ft_dispendio, id_item, id_cat2, id_formnt)

#puxando as descrições das categorias
consulta <- dplyr::left_join(consulta, tbl_dm_categoria[,c("id","cat2")],
                             by = c("id_cat2" = "id"))
#trazendo as informações de fomentador
consulta <- dplyr::left_join(consulta, tbl_dm_formentador[,c("id_formentador","nme_form")],
                             by = c("id_formnt"= "id_formentador"))

#trazendo as informações de titulo
consulta <- dplyr::left_join(consulta, tbl_dm_projeto[,c("título", "id_item")])

consulta <- consulta %>% mutate(título = stringr::str_trim(título),
                                título = abjutils::rm_accent(título))

#

finep_analise <- tidylog::full_join(finep_brutos, finep_ebp1,
                                    by =c("contrato_2021" = "contrato_2019"),
                                    suffix = c("_2021", "_2019")) %>% unique()

consulta <- consulta %>% filter(id_formnt == 7)

finep_analise <- finep_analise %>%
  tidylog::left_join(consulta[,c ("título", "cat2") ],
       by= c("titulo_2021" = "título")) %>%
  unique() %>%
  dplyr::rename(categoria_sqlite = cat2) %>% unique()



finep_analise <- finep_analise %>% mutate(
  motor            = tolower(stringi::stri_trans_general(titulo_2021, "Latin-ASCII")),
  contrato2 = paste(contrato_2021, 1:nrow(finep_analise)))

finep_analise <- ETLEBP::dtc_categorias(finep_analise, processo = contrato2, motor = motor) %>%
                  select(titulo_2021, contrato_2021,categoria_sqlite,
                          categorias, origem_2021, origem_2019,
                          instrumento_2019, instrumento_2021)

finep_analise <- finep_analise %>% tidyr::gather(origem, valor, -titulo_2021, -contrato_2021,
                         -categorias, -categoria_sqlite, -instrumento_2019, -instrumento_2021) %>%
  select(-origem) %>% rename(origem = valor) %>%
  tidyr::gather(instrumento, tipo_instrumento, -titulo_2021, -contrato_2021,
         -categorias, -categoria_sqlite, -origem)

finep_analise <- finep_analise %>% select(-instrumento)

write.csv(finep_sobra_2021, "finep_sobra_2021.csv")
write.csv(finep_analise, "finep_analise.csv")
write.csv(finep_brutos, "finep_brutos.csv")

finep_analise


finep_brutos <- finep_brutos %>%
  mutate(origem_2021 = "base_bruta_2021")
