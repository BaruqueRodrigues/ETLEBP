test_that("executa a validação da carga completa", {
  filename <- here::here("inst/EIP_20210415.db")

  con <- DBI::dbConnect(RSQLite::SQLite(),
                        ":memory:",
                        dbname = filename)

  # Importando as tabelas do SQlite da INOVA-E
  {tbl_dm_agente_empresa <- DBI::dbReadTable(con,"dm_agente_empresa")
    tbl_dm_categoria <- DBI::dbReadTable(con,"dm_categoria")
    tbl_dm_formentador <- DBI::dbReadTable(con,"dm_formentador")
    tbl_dm_mod_finan <- DBI::dbReadTable(con,"dm_mod_finan")
    tbl_dm_nat_disp <- DBI::dbReadTable(con,"dm_nat_disp")
    tbl_dm_projeto <- DBI::dbReadTable(con,"dm_projeto")
    tbl_ft_dispendio <- DBI::dbReadTable(con,"ft_dispendio")
  }
  tbl_ft_dispendio <- tbl_ft_dispendio %>% filter(id_item == "ANP")

  #base simulada
  dm_agente_empresa <- data.frame(id_agente = 1, #id sqlite
                                  nme_agente = "Companhia de Dados Simulados para Teste Automático",#nome_agente_executor
                                  ntz_agente = NA,#natureza_agente_executor
                                  uf = NA,#uf
                                  municipio = NA,#
                                  cnpj = NA)

  dm_projeto <- data.frame(id_projeto = 1,#id no sqlite
                           dta_inicio = "2013-01-01",#data_inicio
                           dta_limite = "2019-12-31",#data_limite
                           título = "Dados Simulados para o teste automático",#titulo_projeto
                           id_item = "0001",#id
                           situação = NA #status
  )

  ft_dispendio <-data.frame(id_disp = 1:7, #id sqlite
                            chamada = NA, #
                            vlr = rep(10000, 7), #valor_executado
                            dta_inicio = "2013-01-01",
                            id_item = "ANEEL", #id
                            ntz_finan = 1, #natureza_financiamento
                            id_cat2 = 13, #categoria slqite
                            id_exec = 1, #
                            id_finan = NA,
                            id_prop = NA,
                            id_formnt = 5, #fonte_de_dados
                            mod_finan = 2, #
                            ano = 2013:2019)

  #Removendo as tabelas dos SQlite e inserindo a tabela de dados simulados
  DBI::dbRemoveTable(con, "dm_agente_empresa")
  DBI::dbRemoveTable(con, "dm_projeto")
  DBI::dbRemoveTable(con, "ft_dispendio")
  DBI::dbWriteTable(con, "dm_agente_empresa", dm_agente_empresa)
  DBI::dbWriteTable(con, "dm_projeto", dm_projeto)
  DBI::dbWriteTable(con, "ft_dispendio", ft_dispendio)


  # Dataframe pra inserir a info na função
  df_test <- data.frame(
    id = as.character(0001),
    fonte_de_dados = "ANEEL",
    data_assinatura = lubridate::ymd("2013-01-01"),
    data_limite = lubridate::ymd("2020-12-31"),
    duracao_dias = 2921,
    titulo_projeto = "Dados Simulados para o teste automático",
    status_projeto = NA,
    valor_contratado = 100000,
    valor_executado_2013_2020 = 100000,
    nome_agente_financiador = "",
    natureza_agente_financiador = "Empresa Privada",
    modalidade_financiamento = "Reembolsável",
    nome_agente_executor = "Companhia de Dados Simulados para Teste Automático",
    natureza_agente_executor = "",
    uf_ag_executor = "",
    regiao_ag_executor = "",
    natureza_financiamento = NA,
    "p&d_ou_demonstracao" = "P&D",
    valor_executado_2013 = 100000,
    valor_executado_2014 = 100000,
    valor_executado_2015 = 100000,
    valor_executado_2016 = 100000,
    valor_executado_2017= 100000,
    valor_executado_2018 = 100000,
    valor_executado_2019 = 100000,
    valor_executado_2020 = 100000,
    motor = "projeto a é de origem b e tem objetivo c",
    categorias = 3.4
  )

  ETLEBP::executa_carga_completa(df_test, filename)

  #GABARITO
  gabarito <-data.frame(id_disp = 8, #id sqlite
                        chamada = NA, #
                        vlr = rep(100000, 8), #valor_executado
                        dta_inicio = NA,
                        id_item = "1", #id
                        ntz_finan = 0, #natureza_financiamento
                        id_cat2 = 13, #categoria slqite
                        id_exec = 2, #
                        id_finan = NA,
                        id_prop = NA,
                        id_formnt = 5, #fonte_de_dados
                        mod_finan = 1, #modalidade de financiamento
                        ano = 2013:2020)

  gabarito <- gabarito %>%
    dplyr::mutate(
    chamada = as.integer(chamada),
    dta_inicio = as.character(dta_inicio),
    id_finan = as.integer(id_finan),
    id_prop = as.integer(id_prop))

  tbl_ft_dispendio <- DBI::dbReadTable(con,"ft_dispendio")

  expect_equal(tbl_ft_dispendio, gabarito)


})
