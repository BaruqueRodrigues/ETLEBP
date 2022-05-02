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

finep_ebp1 %>% mutate(prazo_utilizacao_2019 = lubridate::ymd(lubridate::dmy(prazo_utilizacao_2019))) %>%
    dplyr::filter(
    prazo_utilizacao_2019 >= "2013-01-01")

finep_brutos <- rio::import(here::here("data/FINEP/14_09_2021_Liberacoes.ods"),
                            skip = 5,
                            sheet = 1) %>%
  janitor::clean_names() %>%
  rename_with(
    .fn = ~str_glue("{.x}_2021")
  )

comparacao <- tidylog::full_join(finep_ebp1, finep_brutos,
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

comparacao %>%
  filter(
    !is.na(contrato_2019) & !is.na(contrato_2021)
  ) %>%
  select(
    contrato_2019,
    contrato_2021,
    titulo_2019,
    titulo_2021,
    status_2019,
    status_2021,
    data_assinatura_2019,
    data_assinatura_2021,
    data_liberacao_2019,
    data_liberacao_2021,
    valor_finep_2019,
    valor_finep_2021,
    valor_liberado_2019,
    valor_liberado_2021,
    everything()
  )


