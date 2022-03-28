library(ETLEBP)
library(tidyverse)

#faz o tratamento da base intermedirária

df <- cria_base_intermediaria_bndes()

#visualização do produto da função

df

#diretório do sqlite como argumento

sqlite <- here::here("C:/Users/quemu/Desktop/EIP_20210415.db")

#faz o tratamento para carga completa

executa_tratamento_completo(df, sqlite)

#faz o tratamento para a carga incremental

executa_tratamento_incremental(df, sqlite)

#executa a carga completa

executa_carga_completa(df, sqlite)

#executa a carga incremental

executa_carga_incremental(df, sqlite)
