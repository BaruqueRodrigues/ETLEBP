library(ETLEBP)
library(tidyverse)
# Importando os dados sqlite
bndes_ebp1 <- readr::read_delim(here::here("analise algoritmo/bases 1 versao ebp/6.bndes.csv"),
                                delim = ";", escape_double = FALSE, locale = readr::locale(),
                                trim_ws = TRUE, skip = 4) %>%
  janitor::clean_names() %>%
  rename_with(
    .fn = ~str_glue("{.x}_2019")
  ) %>%
  mutate(numero_do_contrato_2019 = as.character(numero_do_contrato_2019),
         descricao_do_projeto_2019 = abjutils::rm_accent(descricao_do_projeto_2019),
         descricao_do_projeto_2019 = stringr::str_replace_all(descricao_do_projeto_2019, '""', '"'))

bndes_brutos <- readxl::read_excel(here::here("data/BNDES/naoautomaticas.xlsx"),
                                   skip = 4) %>%
  janitor::clean_names() %>%
  rename_with(
    .fn = ~str_glue("{.x}_2021")
  ) %>%
  mutate(
    descricao_do_projeto_2021 = abjutils::rm_accent(descricao_do_projeto_2021)
  )

filename <- here::here("data/DB_EIP/EIP_20210415.db")

con <- DBI::dbConnect(RSQLite::SQLite(),
                      ":memory:",
                      dbname = filename)

# Importando as tabelas do SQlite da INOVA-E
tbl_dm_agente_empresa <- DBI::dbReadTable(con,"dm_agente_empresa")
tbl_dm_categoria <- DBI::dbReadTable(con,"dm_categoria")
tbl_dm_formentador <- DBI::dbReadTable(con,"dm_formentador")
tbl_dm_mod_finan <- DBI::dbReadTable(con,"dm_mod_finan")
tbl_dm_nat_disp <- DBI::dbReadTable(con,"dm_nat_disp")
tbl_dm_projeto <- DBI::dbReadTable(con,"dm_projeto")
tbl_ft_dispendio <- DBI::dbReadTable(con,"ft_dispendio")

#projetos do bndes dentro do sqlite
dm_projeto_sel <- left_join(tbl_dm_projeto, tbl_ft_dispendio[,c("id_item","id_formnt")]) %>%
  filter(id_formnt == 6) %>%
  mutate(título = abjutils::rm_accent(título),
         título = stringr::str_trim(título),
         existe = match(título, bndes_ebp1$descricao_do_projeto_2019)) %>%
  unique()

#projetos do bndes que foram identificados no sqlite
# bndes_energia_sqlite <- tidylog::semi_join(bndes_ebp1, dm_projeto_sel,
#           by =c("descricao_do_projeto_2019" = "título" )) %>% unique()

 bndes_energia_sqlite <- tidylog::semi_join(dm_projeto_sel, bndes_ebp1,
                                            by =c("título" = "descricao_do_projeto_2019"  ))%>% unique()



# comparacao <- tidylog::full_join(bndes_energia_sqlite, bndes_brutos,
#                                  by = c("numero_do_contrato_2019" = "numero_do_contrato_2021"),
#                                  keep = TRUE)

comparacao <- tidylog::full_join(bndes_energia_sqlite, bndes_brutos,
                                 by = c("título" = "descricao_do_projeto_2021"),
                                 keep = TRUE)


projetos_inexistentes <- bndes_energia_sqlite %>%
  tidylog::anti_join(bndes_brutos,
                     by = c("título" = "descricao_do_projeto_2021"),
                     keep = TRUE)



# projetos_inexistentes <- bndes_energia_sqlite %>%
#   tidylog::anti_join(bndes_brutos,
#                      by = c("numero_do_contrato_2019" = "numero_do_contrato_2021"),
#                      keep = TRUE) %>%
#   select(numero_do_contrato_2019,
#          descricao_do_projeto_2019,
#          situacao_do_contrato_2019,
#          valor_contratado_r_2019,
#          everything()
#   )

# projetos_sem_valor_em_2021 <- comparacao %>%
#   filter(
#     !is.na(numero_do_contrato_2019) & !is.na(numero_do_contrato_2021)
#   ) %>%
#   filter(!is.na(valor_contratado_r_2019) & !is.na(valor_desembolsado_r_2019),
#          !is.na(valor_contratado_r_2021) & !is.na(valor_desembolsado_r_2021),
#   ) %>%
#   select(numero_do_contrato_2019, numero_do_contrato_2021,
#          descricao_do_projeto_2019, descricao_do_projeto_2021,
#          situacao_do_contrato_2019, situacao_do_contrato_2021,
#          valor_contratado_r_2019, valor_contratado_r_2021,
#          valor_desembolsado_r_2019, valor_desembolsado_r_2021,
#          everything())

write.csv(projetos_inexistentes, "projetos_inexistentes_selecionados.csv")
