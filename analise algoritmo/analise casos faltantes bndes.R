library(tidyverse)
library(janitor)
library(writexl)

bndes_ebp1 <- readr::read_delim(here::here("analise algoritmo/bases 1 versao ebp/6.bndes.csv"),
                                delim = ";", escape_double = FALSE, locale = readr::locale(),
                                trim_ws = TRUE, skip = 4) %>%
                                janitor::clean_names() %>%
                                rename_with(
                                .fn = ~str_glue("{.x}_2019")
                                ) %>%
                                mutate(numero_do_contrato_2019 = as.character(numero_do_contrato_2019))

bndes_brutos <- readxl::read_excel(here::here("data/BNDES/naoautomaticas.xlsx"),
                           skip = 4) %>%
                        janitor::clean_names() %>%
                        rename_with(
                        .fn = ~str_glue("{.x}_2021")
                        )

comparacao <- tidylog::full_join(bndes_ebp1, bndes_brutos,
                        by = c("numero_do_contrato_2019" = "numero_do_contrato_2021"),
                        keep = TRUE)

projetos_inexistentes <- bndes_ebp1 %>%
                         tidylog::anti_join(bndes_brutos,
                                   by = c("numero_do_contrato_2019" = "numero_do_contrato_2021"),
                                   keep = TRUE) %>%
                         select(numero_do_contrato_2019,
                                descricao_do_projeto_2019,
                                situacao_do_contrato_2019,
                                valor_contratado_r_2019,
                                everything()
                                )

projetos_sem_valor_em_2021 <- comparacao %>%
  filter(
  !is.na(numero_do_contrato_2019) & !is.na(numero_do_contrato_2021)
) %>%
  filter(!is.na(valor_contratado_r_2019) & !is.na(valor_desembolsado_r_2019),
           !is.na(valor_contratado_r_2021) & !is.na(valor_desembolsado_r_2021),
         ) %>%
  select(numero_do_contrato_2019, numero_do_contrato_2021,
         descricao_do_projeto_2019, descricao_do_projeto_2021,
         situacao_do_contrato_2019, situacao_do_contrato_2021,
         valor_contratado_r_2019, valor_contratado_r_2021,
         valor_desembolsado_r_2019, valor_desembolsado_r_2021,
         everything())

write_xlsx(
  list(
    projetos_inexistentes = projetos_inexistentes,
    projetos_sem_valor_em_2021 = projetos_sem_valor_em_2021
  )

)
