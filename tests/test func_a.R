library(testthat)
library(ETLEBP)

# Teste func_a

#A função abaixo calcula o valor gasto pelo projeto por dia do ano.
#A função retorna adiciona ao dataset alvo colunas com o valor total investido por dia do ano.

test_that("testando função com dataframe simulando um projeto",{

  base <- data.frame(processo = "0001",
                     data_inicio = lubridate::ymd("2013-01-01"),
                     prazo_utilizacao = lubridate::ymd("2020-12-31"),
                     valor_projeto  = 80000) %>%
    dplyr::mutate(tempo_dias = lubridate::time_length(data_inicio- prazo_utilizacao, "days"),
                  tempo_anos = lubridate::time_length(data_inicio- prazo_utilizacao, "year"))

  df_gabarito <- data.frame(valor_projeto = 80000,
                            gasto_2013 = 9996.577,
                            gasto_2014 = 9996.577,
                            gasto_2015 = 9996.577,
                            gasto_2016 = 10023.96,
                            gasto_2017 = 9996.577,
                            gasto_2018 = 9996.577,
                            gasto_2019 = 9996.577,
                            gasto_2020 = 10023.96,
                            gasto_2013_2020 = 80000)

  res_func_a <- func_a(base, processo, data_inicio, prazo_utilizacao, valor_projeto) %>%
    dplyr::select(valor_projeto, gasto_2013:gasto_2013_2020)
  expect_equal(res_func_a,
               df_gabarito,
               tolerance = 0.001
  )
})
