#' Cria a base intemediária para a finep
#'
#' @import dplyr
#' @import readr
#' @import janitor
#' @import tidyr
#' @import lubridate
#' @import stringr
#' @import readODS
#' @return
#' @export
#'
#' @examples
cria_base_intermediaria_finep <- function(){

  finep <- readODS::read_ods(path = "data/FINEP/14_09_2021_Liberacoes.ods",
                    skip = 4,
                    sheet = 1)

  finep <- finep %>% clean_names()%>% slice(-1)


  finep <- finep %>% mutate(
    titulo2          = tolower(stringi::stri_trans_general(titulo, "Latin-ASCII")),
    valor_finep      = as.numeric(valor_finep),
    valor_liberado   = as.numeric(valor_liberado),
    data_liberacao   = ymd(dmy(data_liberacao)),
    data_assinatura  = ymd(dmy(data_assinatura)),
    prazo_utilizacao = ymd(dmy(prazo_utilizacao)),
    periodo_meses    = time_length(prazo_utilizacao- data_assinatura, "months"),
    periodo_dias     = time_length(prazo_utilizacao - data_assinatura, "days"),
    periodo_anos     = as.integer(time_length(prazo_utilizacao - data_assinatura, "years") )
  ) %>%
    filter(prazo_utilizacao >= "2013-01-01",#,
           instrumento == "Reembolsável") %>%
    drop_na(valor_finep)



  finep <- func_a(finep,
                  data_assinatura = finep$data_assinatura ,
                  data_limite = finep$prazo_utilizacao,
                  duracao_dias = finep$periodo_dias,
                  valor_contratado = finep$valor_liberado)


  finep <- finep %>% mutate(regiao_ag_executor = recode(uf,
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

    finep <- finep %>%
      mutate(
        id                           = paste("Finep",
                                             contrato, sep = "-"),
        fonte_de_dados                 = "Finep",
        data_limite                    = prazo_utilizacao,
        duracao_dias                   = periodo_dias,
        duracao_meses                  = periodo_meses,
        duracao_anos                   = periodo_anos,
        valor_contratado               = valor_finep,
        titulo_projeto                 = titulo,
        status_projeto                 = status,
        nome_agente_financiador        = "Finep",
        natureza_agente_financiador    = "Empresa Pública",
        natureza_financiamento         = "pública",
        modalidade_financiamento       = instrumento,
        nome_agente_executor           = proponente,
        natureza_agente_executor       = "Empresa Privada", #confirmar natureza juridica proponente
        'p&d_ou_demonstracao'          = "Demonstração",
        uf_ag_executor                 = uf,
        valor_executado_2013_2020      = media_gasto,
        valor_executado_2013           = gasto_2013,
        valor_executado_2014           = gasto_2014,
        valor_executado_2015           = gasto_2015,
        valor_executado_2016           = gasto_2016,
        valor_executado_2017           = gasto_2017,
        valor_executado_2018           = gasto_2018,
        valor_executado_2019           = gasto_2019,
        valor_executado_2020           = gasto_2020)



  finep <- finep %>%
    select(id,
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
           `p&d_ou_demonstracao`,
           valor_executado_2013,
           valor_executado_2014,
           valor_executado_2015,
           valor_executado_2016,
           valor_executado_2017,
           valor_executado_2018,
           valor_executado_2019,
           valor_executado_2020,
           titulo2
    )

  finep <- finep %>%
    mutate(iea1_1 = str_detect(titulo2, iea1_1),
           iea1_2 = str_detect(titulo2, iea1_2),
           iea1_3 = str_detect(titulo2, iea1_3),
           iea1_4 = str_detect(titulo2, iea1_4),
           iea2_1 = str_detect(titulo2, iea2_1),
           iea2_2 = str_detect(titulo2, iea2_2),
           iea2_3 = str_detect(titulo2, iea2_3),
           iea3_1 = str_detect(titulo2, iea3_1),
           iea3_2 = str_detect(titulo2, iea3_2),
           iea3_3 = str_detect(titulo2, iea3_3),
           iea3_4 = str_detect(titulo2, iea3_4),
           iea3_5 = str_detect(titulo2, iea3_5),
           iea3_6 = str_detect(titulo2, iea3_6),
           iea3_7 = str_detect(titulo2, iea3_7),
           iea4_1 = str_detect(titulo2, iea4_1),
           iea4_2 = str_detect(titulo2, iea4_2),
           iea5_1 = str_detect(titulo2, iea5_1),
           iea5_2 = str_detect(titulo2, iea5_2),
           iea6_1 = str_detect(titulo2, iea6_1),
           iea6_2 = str_detect(titulo2, iea6_2),
           iea6_3 = str_detect(titulo2, iea6_3),
           iea7_1 = str_detect(titulo2, iea7_1),
           iea7_2 = str_detect(titulo2, iea7_2)
    )
finep
}
