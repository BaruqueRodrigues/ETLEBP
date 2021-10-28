#' Cria a base intemediária para a anp
#'
#' @return
#' @export
#'
#' @examples
cria_base_intermediaria_anp <- function(origem_processos = here::here("data/ANP/projetos-rt-3-2015.csv")) {

  anp_2015 <- readr::read_delim(origem_processos,
                         ";", escape_double = FALSE, trim_ws = TRUE) %>%
              janitor::clean_names()
  anp_2015 <- anp_2015 %>%
    dplyr::mutate(
      valor_clausula       = as.numeric(stringr::str_replace_all(
                             stringr::str_remove_all(valor_clausula, "[.R$ ]"), "[,]", ".")),
      data_inicio          = lubridate::dmy(data_inicio),
      prazo_utilizacao     = data_inicio %m+% months(prazo),
      prazo_decorrido_dias = lubridate::time_length(prazo_utilizacao - data_inicio, "days"),
      prazo_decorrido_anos = as.integer(prazo_decorrido_dias/365),
      motor                = stringi::stri_trans_general(paste(titulo, objetivo, tema, subtema),
                                                         "Latin-ASCII"),
      motor                = tolower(motor)
    ) %>%
    dplyr::filter(prazo_utilizacao >= "2013-01-01") %>%
    tidyr::drop_na(valor_clausula)

  anp_2015 <- func_a(anp_2015,
                     data_assinatura = anp_2015$data_inicio,
                     data_limite = anp_2015$prazo_utilizacao,
                     duracao_dias = anp_2015$prazo_decorrido_dias,
                     valor_contratado = anp_2015$valor_clausula)


  anp_2015<-anp_2015 %>%
            dplyr::mutate(
            id                = paste("Anp", no_anp, sep = "-"),
            fonte_de_dados              = "Anp",
            titulo_projeto              = titulo,
            data_assinatura             = data_inicio,
            data_limite                 = prazo_utilizacao,
            duracao_meses               = prazo,
            duracao_dias                = prazo_decorrido_dias,
            duracao_anos                = prazo_decorrido_anos,
            valor_contratado            = valor_clausula,
            valor_executado_2013_2020   = media_gasto,
            nome_agente_financiador     = empresa_responsavel,
            natureza_agente_financiador = "Empresa Privada", # confirmar
            natureza_financiamneto      = "publicamente orientado",
            modalidade_financiamento    = NA,
            nome_agente_executor        = executor_1,
            natureza_agente_executor    = 'Empresa Privada', # confirmar
            'p&d_ou_demonstracao'       = NA,
            uf_ag_executor              = NA,
            regiao_ag_executor          = NA,
            status_projeto              = NA,
            valor_executado_2013        = gasto_2013,
            valor_executado_2014        = gasto_2014,
            valor_executado_2015        = gasto_2015,
            valor_executado_2016        = gasto_2016,
            valor_executado_2017        = gasto_2017,
            valor_executado_2018        = gasto_2018,
            valor_executado_2019        = gasto_2019,
            valor_executado_2020        = gasto_2020)

  anp_2015<-anp_2015 %>%
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
            natureza_financiamneto,
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

  anp_2015 <- anp_2015 %>%
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

  anp_2015
}
