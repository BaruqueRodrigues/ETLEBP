
#' Cria a base intemediária para a aneel
#'
#'
#' @return
#' @export
#'
#' @examples
cria_base_intermediaria_aneel <- function(
  origem_processos = here::here("data/SGPED_BI/PD Busca Textual.csv"),
  origem_equipes = here::here("data/SGPED_BI/5.PD RF EQUIPE.csv")
){


  options(scipen=999)
  ##get the data ##

  #importando o dataset
  anel_pd <- readr::read_delim(origem_processos,
                        ";", escape_double = FALSE, locale = locale(encoding = "WINDOWS-1252"),
                        trim_ws = TRUE) %>%
    janitor::clean_names()

  anel_pd <- anel_pd%>%
    dplyr::mutate(data_de_carregamento   = lubridate::as_date(lubridate::dmy_hms(data_de_carregamento)),
           data_de_conclusao             = lubridate::dmy(data_de_conclusao),
           duracao_prevista              = date(data_de_carregamento+dmonths(anel_pd$duracao_prevista_meses)),
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
    tidyr::drop_na(custo_total_previsto)


  anel_pd <- func_a(anel_pd,
                                data_assinatura = anel_pd$data_de_carregamento,
                                data_limite = anel_pd$data_de_conclusao,
                                duracao_dias = anel_pd$duracao_dias,
                                valor_contratado = anel_pd$custo_total_previsto)


  aneel_time <- read_csv2(origem_equipes)%>%
    dplyr::filter(`Tipo de Entidade` == "Proponente")  %>%
    dplyr::select(CodProj,`Entidade Vinculada`,`Unidade Federativa`) %>%
    dplyr::distinct() %>%
    janitor::clean_names()

  anel_pd <- dplyr::left_join(anel_pd, aneel_time, by = "cod_proj")

  anel_pd<- anel_pd %>% dplyr::mutate(regiao_ag_executor = dplyr::recode(unidade_federativa,
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
  ))
  anel_pd <- anel_pd %>%
    dplyr::mutate(
           id                          = paste("Aneel", cod_proj, sep = "-"),
           fonte_dados                 = "Aneel",
           data_assinatura             = data_de_carregamento,
           data_limite                 = data_de_conclusao,
           duracao_dias                = duracao_dias,
           duracao_meses               = duracao_prevista,
           valor_contratado            = custo_total_previsto,
           valor_executado_2013_2020   = media_gasto,
           nome_agente_financiador     = proponente,
           natureza_agente_financiador = "Empresa Privada", # confirmar
           modalidade_financiamento    = NA,
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
    )

  anel_pd <- anel_pd %>%
    dplyr::select(
      id,
      fonte_dados,
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
      `p&d_ou_demonstracao`,
      valor_executado_2013,valor_executado_2014,
      valor_executado_2015,valor_executado_2016,
      valor_executado_2017,valor_executado_2018,
      valor_executado_2019,valor_executado_2020,
      motor)

  anel_pd <- anel_pd %>%
    dplyr::mutate(
           iea1_1 = stringr::str_detect(motor, iea1_1),
           iea1_2 = stringr::str_detect(motor, iea1_2),
           iea1_3 = stringr::str_detect(motor, iea1_3),
           iea1_4 = stringr::str_detect(motor, iea1_4),
           iea2_1 = stringr::str_detect(motor, iea2_1),
           iea2_2 = stringr::str_detect(motor, iea2_2),
           iea2_3 = stringr::str_detect(motor, iea2_3),
           iea3_1 = stringr::str_detect(motor, iea3_1),
           iea3_2 = stringr::str_detect(motor, iea3_2),
           iea3_3 = stringr::str_detect(motor, iea3_3),
           iea3_4 = stringr::str_detect(motor, iea3_4),
           iea3_5 = stringr::str_detect(motor, iea3_5),
           iea3_6 = stringr::str_detect(motor, iea3_6),
           iea3_7 = stringr::str_detect(motor, iea3_7),
           iea4_1 = stringr::str_detect(motor, iea4_1),
           iea4_2 = stringr::str_detect(motor, iea4_2),
           iea5_1 = stringr::str_detect(motor, iea5_1),
           iea5_2 = stringr::str_detect(motor, iea5_2),
           iea6_1 = stringr::str_detect(motor, iea6_1),
           iea6_2 = stringr::str_detect(motor, iea6_2),
           iea6_3 = stringr::str_detect(motor, iea6_3),
           iea7_1 = stringr::str_detect(motor, iea7_1),
           iea7_2 = stringr::str_detect(motor, iea7_2)
    )



  #anel_pd <- anel_pd %>% select(-motor)

  write.csv(anel_pd, here::here("inst/intermediarias/aneel_interm_06_10_2021.csv"))



  anel_pd

}


