library(tidyverse)
library(ETLEBP)

bndes <- cria_base_intermediaria_bndes()


sqlite <- here::here("C:/Users/quemu/Desktop/EIP_20210415.db")

bndes <- executa_tratamento_completo(bndes, sqlite)
bndes <- executa_tratamento_incremental(bndes, sqlite)

bndes %>% filter(existe == "sim") %>% janitor::tabyl(categorias )


bndes_acerto <- bndes %>% filter(existe == "sim") %>% rowwise() %>%
  mutate(categorias_split = str_split(categorias, pattern = ",")) %>%
  ungroup() %>% unnest(categorias_split_un =categorias_split) %>%
  mutate(categorias_split_un = str_remove_all(categorias_split_un, '\\"')) %>%
  select(id,categoria_sqlite,  categorias_split_un)

bndes_acerto_res <- bndes_acerto %>%
  mutate(correto = categorias_split_un == categoria_sqlite ) %>% group_by(id) %>%
  summarise(correto = mean(correto))

 mean(bndes_acerto_res$correto)

bndes_ebp1 <- readr::read_delim(here::here("analise algoritmo/bases 1 versao ebp/6.bndes.csv"),
                                delim = ";", escape_double = FALSE, locale = readr::locale(),
                                trim_ws = TRUE, skip = 4) %>%
  janitor::clean_names() %>%
  mutate(
    prazo_execucao_meses  = as.numeric(prazo_carencia_meses) + as.numeric(prazo_amortizacao_meses),
    data_da_contratacao   = str_replace_all(data_da_contratacao, "\\/", "-"),
    data_da_contratacao   = lubridate::dmy(data_da_contratacao),
    prazo_utilizacao      = lubridate::ymd(data_da_contratacao) %m+% months(prazo_execucao_meses)
  ) %>%
  rename_with(
    .fn = ~str_glue("{.x}_2019")
  ) %>%
  mutate(numero_do_contrato_2019 = as.character(numero_do_contrato_2019),
         descricao_do_projeto_2019 = abjutils::rm_accent(descricao_do_projeto_2019),
         descricao_do_projeto_2019 = stringr::str_replace_all(descricao_do_projeto_2019, '""', '"')) %>%
  dplyr::filter(prazo_utilizacao_2019 >= "2013-01-01",
                inovacao_2019         == "SIM") %>%
  tidyr::drop_na(valor_contratado_r_2019) %>%
  unique()%>%
  mutate(origem = "base_bruta_2019")

bndes_brutos <- readxl::read_excel(here::here("data/BNDES/naoautomaticas.xlsx"),
                                   skip = 4) %>%
  janitor::clean_names() %>%
  mutate(
    prazo_execucao_meses  = as.numeric(prazo_carencia_meses) + as.numeric(prazo_amortizacao_meses),
    data_da_contratacao   = lubridate::ymd(data_da_contratacao),
    prazo_utilizacao      = lubridate::ymd(data_da_contratacao) %m+% months(prazo_execucao_meses)
  ) %>%
  rename_with(
    .fn = ~str_glue("{.x}_2021")
  ) %>%
  mutate(
    descricao_do_projeto_2021 = abjutils::rm_accent(descricao_do_projeto_2021)
  ) %>%
  dplyr::filter(prazo_utilizacao_2021 >= "2013-01-01",
                 inovacao_2021         == "SIM") %>%
  tidyr::drop_na(valor_contratado_r_2021) %>%
  unique() %>%
  mutate(origem = "base_bruta_2021")


bndes_brutos_2022 <- readxl::read_excel("C:/Users/quemu/Downloads/naoautomaticas.xlsx",
                                skip = 4)%>%
  janitor::clean_names() %>%
  mutate(
    prazo_execucao_meses  = as.numeric(prazo_carencia_meses) + as.numeric(prazo_amortizacao_meses),
    data_da_contratacao   = lubridate::ymd(data_da_contratacao),
    prazo_utilizacao      = lubridate::ymd(data_da_contratacao) %m+% months(prazo_execucao_meses)
  )
#
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

# Análise: dos projetos que estão na base bruta 2021 e não estão na base bruta 2019,
#quantos estão classificados em alguma categoria?
bndes_sobra_2021 <- bndes_brutos %>% tidylog::anti_join(
  bndes_ebp1,
  by = c("numero_do_contrato_2021" = "numero_do_contrato_2019")
)


bndes_brutos %>% tidylog::anti_join(
  bndes_brutos_2022,
  by = c("numero_do_contrato" = "numero_do_contrato"))


bndes_brutos_2022 %>% tidylog::anti_join(
  bndes_brutos,
  by = c("numero_do_contrato" = "numero_do_contrato"))

bndes_sobra_2021 <- bndes_sobra_2021 %>%
  dplyr::mutate(
    prazo_execucao_meses  = as.numeric(prazo_carencia_meses_2021) + as.numeric(prazo_amortizacao_meses_2021),
    data_da_contratacao   = lubridate::ymd(data_da_contratacao_2021),
    motor = tolower(stringi::stri_trans_general(descricao_do_projeto_2021, "Latin-ASCII")),
    prazo_utilizacao      = lubridate::ymd(data_da_contratacao_2021) %m+% months(prazo_execucao_meses),
    prazo_decorrido_anos  = as.integer(lubridate::time_length(prazo_utilizacao - data_da_contratacao, "years")),
    prazo_decorrido_dias  = lubridate::time_length(prazo_utilizacao- data_da_contratacao, "days"),
    numero_do_contrato2    = paste(numero_do_contrato_2021, 1:nrow(bndes_sobra_2021))

  )%>%
  dplyr::filter(prazo_utilizacao >= "2013-01-01",
                inovacao_2021         == "SIM") %>%
  tidyr::drop_na(valor_contratado_r_2021) %>%
  unique() %>%
  func_a(
         processo = numero_do_contrato2,
         data_inicio = data_da_contratacao,
         prazo_utilizacao = prazo_utilizacao,
         valor_projeto = valor_contratado_r_2021) %>%
  dtc_categorias(numero_do_contrato2, motor) %>%
  dplyr::mutate(regiao_ag_executor = dplyr::recode(
    uf_2021,
    "AC" = "N",
    "AL" = "NE",
    "AM" = "N",
    "BA" = "NE",
    "CE" = "NE",
    "DF" = "CO",
    "ES" = "SE",
    "GO" = "CO",
    "MA" = "NE",
    "MG" = "SE",
    "MS" = "CO",
    "MT" = "CO",
    "PA" = "N",
    "PB" = "NE",
    "PE" = "NE",
    "PI" = "NE",
    "PR" = "S",
    "RJ" = "SE",
    "RN" = "NE",
    "RO" = "N",
    "RS" = "S",
    "SC" = "S",
    "SE" = "NE",
    "SP" = "SE",
    "TO" = "N"))

bndes_sobra_2021 <- bndes_sobra_2021 %>%
  dplyr::mutate(
    id                           = paste("BNDES",
                                         numero_do_contrato_2021, sep = "-"),
    titulo_projeto = descricao_do_projeto_2021,
    fonte_de_dados                 = "BNDES",
    data_assinatura                = data_da_contratacao,
    data_limite                    = prazo_utilizacao,
    duracao_dias                   = prazo_decorrido_dias,
    status_projeto                 = situacao_do_contrato_2021,
    duracao_meses                  = prazo_execucao_meses,
    duracao_anos                   = prazo_decorrido_anos,
    valor_contratado               = valor_contratado_r_2021,
    valor_executado_2013_2020      = gasto_2013_2020,
    nome_agente_financiador     = "Bndes",
    natureza_agente_financiador = "empresa pública",
    natureza_financiamento      = "pública",
    modalidade_financiamento    = modalidade_de_apoio_2021,
    nome_agente_executor        = cliente_2021,
    natureza_agente_executor    = natureza_do_cliente_2021,
    'p&d_ou_demonstracao'          = NA ,
    uf_ag_executor                  = uf_2021,
    valor_executado_2013            = gasto_2013,
    valor_executado_2014            = gasto_2014,
    valor_executado_2015            = gasto_2015,
    valor_executado_2016            = gasto_2016,
    valor_executado_2017            = gasto_2017,
    valor_executado_2018            = gasto_2018,
    valor_executado_2019            = gasto_2019,
    valor_executado_2020            = gasto_2020) %>%
  dplyr::select(
    id,
    fonte_de_dados,
    data_assinatura,data_limite,
    duracao_dias,
    titulo_projeto,
    status_projeto,
    valor_contratado,
    valor_executado_2013_2020,
    nome_agente_financiador,
    natureza_agente_financiador,
    modalidade_financiamento,
    nome_agente_executor,
    natureza_agente_executor,
    uf_ag_executor,
    regiao_ag_executor,
    natureza_agente_executor,
    natureza_financiamento,
    'p&d_ou_demonstracao',
    modalidade_financiamento,
    valor_executado_2013,
    valor_executado_2014,
    valor_executado_2015,
    valor_executado_2016,
    valor_executado_2017,
    valor_executado_2018,
    valor_executado_2019,
    valor_executado_2020,
    motor,
    categorias
  ) %>% tibble()

bndes_sobra_2021$categorias %>% table()


bndes_sobra_2021 <- executa_tratamento_completo(bndes_sobra_2021, sqlite)

bndes_sobra_2021 <- executa_tratamento_incremental(bndes_sobra_2021, sqlite)

writexl::write_xlsx(bndes_sobra_2021, "bndes_intermediaria_novos.xlsx")
#

bndes_analise <- tidylog::full_join(bndes_brutos, bndes_ebp1,
          by = c("descricao_do_projeto_2021"= "descricao_do_projeto_2019"),
          suffix = c("_2021", "_2019"))

bndes_analise <- bndes_analise %>% tidylog::left_join(consulta[,c ("título", "cat2") ],
                            by= c("descricao_do_projeto_2021" = "título")) %>% unique() %>%
  dplyr::rename(categoria_sqlite = cat2)

bndes_analise <- bndes_analise %>%
    mutate(numero_do_contrato2    = paste(numero_do_contrato_2021, 1:nrow(bndes_analise)),
           motor = tolower(stringi::stri_trans_general(descricao_do_projeto_2021, "Latin-ASCII"))
    )

bndes_analise <- dtc_categorias(bndes_analise, numero_do_contrato2, motor)%>%
  select(descricao_do_projeto_2021, numero_do_contrato_2021,
         categoria_sqlite, categorias, origem_2021, origem_2019)

bndes_analise <- bndes_analise %>% gather(origem, valor, -descricao_do_projeto_2021,
                         -numero_do_contrato_2021,-categoria_sqlite,-categorias) %>% select(-valor)
write.csv(bndes_sobra_2021, "bndes_sobra_2021.csv")
write.csv(bndes_analise, "bndes_analise.csv")
