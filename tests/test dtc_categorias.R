library(testthat)
library(ETLEBP)

## Teste função detecta categorias
#Abaixo testaremos a função que detecta em qual categoria de IEA o projeto pode ser classificado.
#Para tal utilizaremos o dataseet com os termos de detecção abaixo.

test_that("Testando a função com 12 termos em dataset simulado",{

  base <- data.frame(id = seq(1:12),
                     motor = c("eficiencia no sistema energetico",
                               "só eficiencia",
                               "eficiencia qualquer palavra e carro",
                               "aqui temos energia solar",
                               "captura de CO2",
                               "aqui não temos nada",
                               "energia de fonte solar",
                               "biocombustivel",
                               "fazenda verde",
                               "fazenda",
                               "só bateria",
                               "batera")
  )

  df_gabarito <- data.frame(id = seq(1:12),
                            motor = c("eficiencia no sistema energetico",
                                      "só eficiencia",
                                      "eficiencia qualquer palavra e carro",
                                      "aqui temos energia solar",
                                      "captura de CO2",
                                      "aqui não temos nada",
                                      "energia de fonte solar",
                                      "biocombustivel",
                                      "fazenda verde",
                                      "fazenda",
                                      "só bateria",
                                      "batera"),
                            categorias = c("1.1", "nenhuma categoria encontrada", "1.3",
                                           "3.1", "2.3", "nenhuma categoria encontrada",
                                           "3.1", "3.4", "nenhuma categoria encontrada",
                                           "nenhuma categoria encontrada", "6.3", "nenhuma categoria encontrada")
  )
  expect_equal(
    dtc_categorias(base, id, motor),
    df_gabarito
  )


})
