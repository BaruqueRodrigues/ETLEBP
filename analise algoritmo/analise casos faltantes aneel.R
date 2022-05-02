library(ETLEBP)
library(tidyverse)
aneel_ebp1 <- readr::read_delim("analise algoritmo/bases 1 versao ebp/5.ANEELpd_rev.csv",
                         delim = ";", escape_double = FALSE, trim_ws = TRUE) %>% janitor::clean_names()

aneel_brutos <-  readr::read_delim("data/SGPED_BI/PD Busca Textual.csv",
                                   ";", escape_double = FALSE, locale = readr::locale(encoding = "latin1"),
                                   trim_ws = TRUE) %>%
  janitor::clean_names()
#
aneel_projetos_continuados_2019 <- aneel_brutos%>%
  dplyr::mutate(
    data_de_carregamento   = lubridate::as_date(lubridate::dmy_hms(data_de_carregamento)),
    data_de_conclusao             = lubridate::dmy(data_de_conclusao),
    duracao_prevista              = lubridate::date(data_de_carregamento+lubridate::dmonths(duracao_prevista_meses)),
    data_de_conclusao             = dplyr::case_when(is.na(data_de_conclusao) ~ duracao_prevista,
                                                     TRUE~data_de_conclusao ),
    custo_total_previsto          = as.numeric(stringr::str_replace_all(
      stringr::str_replace_all(custo_total_previsto, "[.$]", ""), "[,]", "." )),
    custo_total_realizado         = as.numeric(stringr::str_replace_all(
      stringr::str_replace_all(custo_total_realizado, "[.$]", ""), "[,]", "." )),
    custo_total_realizado         = dplyr::case_when(is.na(custo_total_realizado) ~ custo_total_previsto,
                                                     TRUE~custo_total_realizado),
    duracao_dias                  = lubridate::time_length(data_de_conclusao - data_de_carregamento, "days"),
    #duracao_dias           = interval(data_de_carregamento, data_de_conclusao)/ddays(),
    duracao_anos                  = as.integer(lubridate::interval(data_de_carregamento, data_de_conclusao)/lubridate::dyears()),
    motor                         = stringi::stri_trans_general(paste(titulo,segmento,tema),
                                                                "Latin-ASCII"),
    motor                         = tolower(motor)) %>%
  dplyr::filter(duracao_prevista >= "2013-01-01") %>%
  tidyr::drop_na(custo_total_previsto) %>%
  func_a(cod_proj, data_de_carregamento,
         data_de_conclusao,custo_total_previsto)%>%
  filter(data_de_conclusao >= "2019-01-01")%>%
  dtc_categorias(cod_proj, motor)

aneel_sobra <- aneel_brutos %>% tidylog::anti_join(aneel_ebp1,
                                                   by = c("cod_proj" = "cod_proj"))


aneel_sobra <-   aneel_sobra  %>%
  dplyr::mutate(data_de_carregamento   = lubridate::as_date(lubridate::dmy_hms(data_de_carregamento)),
                data_de_conclusao             = lubridate::dmy(data_de_conclusao),
                duracao_prevista              = lubridate::date(data_de_carregamento+lubridate::dmonths(duracao_prevista_meses)),
                data_de_conclusao             = dplyr::case_when(is.na(data_de_conclusao) ~ duracao_prevista,
                                                                 TRUE~data_de_conclusao ),
                custo_total_previsto          = as.numeric(stringr::str_replace_all(
                  stringr::str_replace_all(custo_total_previsto, "[.$]", ""), "[,]", "." )),
                custo_total_realizado         = as.numeric(stringr::str_replace_all(
                  stringr::str_replace_all(custo_total_realizado, "[.$]", ""), "[,]", "." )),
                custo_total_realizado         = dplyr::case_when(is.na(custo_total_realizado) ~ custo_total_previsto,
                                                                 TRUE~custo_total_realizado),
                duracao_dias                  = lubridate::time_length(data_de_conclusao - data_de_carregamento, "days"),
                #duracao_dias           = interval(data_de_carregamento, data_de_conclusao)/ddays(),
                duracao_anos                  = as.integer(lubridate::interval(data_de_carregamento, data_de_conclusao)/lubridate::dyears()),
                motor                         = stringi::stri_trans_general(paste(titulo,segmento,tema),
                                                                            "Latin-ASCII"),
                motor                         = tolower(motor)) %>%
  dplyr::filter(duracao_prevista >= "2013-01-01") %>%
  tidyr::drop_na(custo_total_previsto) %>%
    func_a(cod_proj, data_de_carregamento,
           data_de_conclusao,custo_total_previsto) %>%
    dtc_categorias(cod_proj, motor)

aneel_time <- readr::read_csv2("data/SGPED_BI/5.PD RF EQUIPE.csv")%>%
  dplyr::filter(`Tipo de Entidade` == "Proponente")  %>%
  dplyr::select(CodProj,`Entidade Vinculada`,`Unidade Federativa`) %>%
  dplyr::distinct() %>%
  janitor::clean_names()

#

aneel_sobra <- dplyr::left_join(aneel_sobra, aneel_time, by = "cod_proj")

aneel_sobra <- aneel_sobra %>%
  dplyr::mutate(regiao_ag_executor = dplyr::recode(unidade_federativa,
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
                                                   "TO" = "N"
  )) %>% dplyr::mutate(
    id                          = paste("ANEEL", cod_proj, sep = "-"),
    fonte_de_dados                 = "ANEEL",
    data_assinatura             = data_de_carregamento,
    data_limite                 = data_de_conclusao,
    duracao_dias                = duracao_dias,
    duracao_meses               = duracao_prevista,
    valor_contratado            = custo_total_previsto,
    valor_executado_2013_2020   = gasto_2013_2020,
    nome_agente_financiador     = proponente,
    natureza_agente_financiador = "Empresa Privada", # confirmar
    natureza_financiamento    = "Publicamente Orientado",
    modalidade_financiamento    = "Não se Aplica",
    nome_agente_executor        = entidade_vinculada,
    natureza_agente_executor    = "Empresa Privada", # confirmar
    'p&d_ou_demonstracao'       = NA,
    titulo_projeto              = titulo,
    status_projeto              = situacao,
    uf_ag_executor              = unidade_federativa,
    valor_executado_2013        = gasto_2013,
    valor_executado_2014        = gasto_2014,
    valor_executado_2015        = gasto_2015,
    valor_executado_2016        = gasto_2016,
    valor_executado_2017        = gasto_2017,
    valor_executado_2018        = gasto_2018,
    valor_executado_2019        = gasto_2019,
    valor_executado_2020        = gasto_2020,
    motor
  ) %>%
  dplyr::select(
    id,
    fonte_de_dados,
    data_assinatura,
    data_limite,
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
    natureza_financiamento,
    `p&d_ou_demonstracao`,
    valor_executado_2013,valor_executado_2014,
    valor_executado_2015,valor_executado_2016,
    valor_executado_2017,valor_executado_2018,
    valor_executado_2019,valor_executado_2020,
    motor,
    categorias) %>% tibble()

aneel_sobra <- executa_tratamento_completo(aneel_sobra, sqlite)

aneel_sobra <- executa_tratamento_incremental(aneel_sobra, sqlite)

aneel_sobra$categorias %>% janitor::tabyl()

writexl::write_xlsx(aneel_sobra, "aneel_intermediario_novos.xlsx")


#juntando os projetos novos com os projetos continuados

aneel_sobra <- rbind(aneel_sobra,aneel_projetos_continuados_2019) %>% unique()

aneel_time <- readr::read_csv2("data/SGPED_BI/5.PD RF EQUIPE.csv")%>%
  dplyr::filter(`Tipo de Entidade` == "Proponente")  %>%
  dplyr::select(CodProj,`Entidade Vinculada`,`Unidade Federativa`) %>%
  dplyr::distinct() %>%
  janitor::clean_names()

aneel_sobra <- dplyr::left_join(aneel_sobra, aneel_time, by = "cod_proj")

aneel_sobra <- aneel_sobra %>%
  dplyr::mutate(regiao_ag_executor = dplyr::recode(unidade_federativa,
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
                                                   "TO" = "N"
  )) %>% dplyr::mutate(
    id                          = paste("ANEEL", cod_proj, sep = "-"),
    fonte_de_dados                 = "ANEEL",
    data_assinatura             = data_de_carregamento,
    data_limite                 = data_de_conclusao,
    duracao_dias                = duracao_dias,
    duracao_meses               = duracao_prevista,
    valor_contratado            = custo_total_previsto,
    valor_executado_2013_2020   = gasto_2013_2020,
    nome_agente_financiador     = proponente,
    natureza_agente_financiador = "Empresa Privada", # confirmar
    natureza_financiamento    = "Publicamente Orientado",
    modalidade_financiamento    = "Não se Aplica",
    nome_agente_executor        = entidade_vinculada,
    natureza_agente_executor    = "Empresa Privada", # confirmar
    'p&d_ou_demonstracao'       = NA,
    titulo_projeto              = titulo,
    status_projeto              = situacao,
    uf_ag_executor              = unidade_federativa,
    valor_executado_2013        = gasto_2013,
    valor_executado_2014        = gasto_2014,
    valor_executado_2015        = gasto_2015,
    valor_executado_2016        = gasto_2016,
    valor_executado_2017        = gasto_2017,
    valor_executado_2018        = gasto_2018,
    valor_executado_2019        = gasto_2019,
    valor_executado_2020        = gasto_2020,
    motor
  ) %>%
  dplyr::select(
    id,
    fonte_de_dados,
    data_assinatura,
    data_limite,
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
    natureza_financiamento,
    `p&d_ou_demonstracao`,
    valor_executado_2013,valor_executado_2014,
    valor_executado_2015,valor_executado_2016,
    valor_executado_2017,valor_executado_2018,
    valor_executado_2019,valor_executado_2020,
    motor,
    categorias) %>% tibble()

aneel_sobra <- executa_tratamento_completo(aneel_sobra, sqlite)

aneel_sobra <- executa_tratamento_incremental(aneel_sobra, sqlite)

aneel_sobra$categorias %>% janitor::tabyl()

writexl::write_xlsx(aneel_sobra, "aneel_intermediario_continuados_2019_e_novos.xlsx")
