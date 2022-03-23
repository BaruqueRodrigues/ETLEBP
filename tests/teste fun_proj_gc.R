library(testthat)
library(ETLEBP)

# Teste projeto guarda-chuva

test_that("Checa o teste com um subconjunto do dataset ANP",{

  filename <- here::here("data/DB_EIP/EIP_20210415.db")

  con <- DBI::dbConnect(RSQLite::SQLite(),
                        ":memory:",
                        dbname = filename)
  tbl_ft_dispendio <- DBI::dbReadTable(con,"ft_dispendio")

  ft_guarda_chuva <- tbl_ft_dispendio%>% dplyr::filter(id_formnt == 11)

  anp <- cria_base_intermediaria_anp() %>%
    dplyr::slice(1:5)

  anp_test <- anp %>%
    dplyr::select(id,valor_executado_2013:valor_executado_2020) %>%
    tidyr::gather(key = ano, value = valor, -id) %>%
    dplyr::mutate(ano = stringr::str_remove(ano, "valor_executado_")) %>%
    dplyr::group_by(ano) %>%
    dplyr::mutate(valor_altera_projeto_guarda_chuva = ifelse(ano %in% c(2013,2014,2015,
                                                                 2016, 2017,2018), "sim", "não")) %>%
    dplyr::group_by(ano, valor_altera_projeto_guarda_chuva) %>%
    dplyr::summarise(valor = sum(valor))


  expect_equal(fun_proj_gc(base = anp, ft_guarda_chuva = ft_guarda_chuva)$vlr_alterar,
               anp_test$valor)
}
)
