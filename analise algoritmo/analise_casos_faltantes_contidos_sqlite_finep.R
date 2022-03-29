library(tidyverse)
library(janitor)
library(writexl)

finep_ebp1 <- readr::read_delim("analise algoritmo/bases 1 versao ebp/2.FINEP.csv",
                                delim = ";", escape_double = FALSE, trim_ws = TRUE,
                                skip = 5) %>%
  janitor::clean_names() %>%
  rename_with(
    .fn = ~str_glue("{.x}_2019")
  )

finep_brutos <- rio::import(here::here("data/FINEP/14_09_2021_Liberacoes.ods"),
                            skip = 5,
                            sheet = 1) %>%
  janitor::clean_names() %>%
  rename_with(
    .fn = ~str_glue("{.x}_2021")
  )

# Importando as tabelas do SQlite da INOVA-E
tbl_dm_agente_empresa <- DBI::dbReadTable(con,"dm_agente_empresa")
tbl_dm_projeto <- DBI::dbReadTable(con,"dm_projeto")
tbl_dm_formentador <- DBI::dbReadTable(con,"dm_formentador")
tbl_ft_dispendio <- DBI::dbReadTable(con,"ft_dispendio")


#projetos do bndes dentro do sqlite
dm_projeto_sel <- left_join(tbl_dm_projeto, tbl_ft_dispendio[,c("id_item","id_formnt")]) %>%
  filter(id_formnt == 7) %>%
  unique()

finep_energia_sqlite <- semi_join(finep_ebp1,dm_projeto_sel,
                                  by = c("contrato_2019" = "id_item"))

comparacao <- tidylog::full_join(finep_energia_sqlite, finep_brutos,
                                 by = c("contrato_2019" = "contrato_2021"),
                                 keep = TRUE)

projetos_inexistentes <- finep_ebp1 %>%
  tidylog::anti_join(finep_brutos,
                     by = c("contrato_2019" = "contrato_2021"),
                     keep = TRUE) %>%
  select(contrato_2019,
         titulo_2019,
         status_2019,
         valor_finep_2019,
         valor_liberado_2019,
         everything()
  )

#Não existem projetos sem valor


