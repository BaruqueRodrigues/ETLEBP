#' Cria a base intemediária para a finep  criando um dataframe
#' @import dplyr
#' @import tidyr
#' @import lubridate
#' @import stringr
#' @import readODS
#' @return
#' @export
#'
#' @examples
cria_base_intermediaria_finep <- function(origem_processos = here::here("data/FINEP/14_09_2021_Liberacoes.ods")){

  finep <- readODS::read_ods(path = origem_processos,
                    skip = 4,
                    sheet = 1)

  names(finep)<-finep[1,]

  finep <- finep %>% janitor::clean_names()%>% dplyr::slice(-1)


  finep <- finep %>% dplyr::mutate(
    motor            = tolower(stringi::stri_trans_general(titulo, "Latin-ASCII")),
    valor_finep      = as.numeric(valor_finep),
    valor_liberado   = as.numeric(valor_liberado),
    data_liberacao   = lubridate::ymd(lubridate::dmy(data_liberacao)),
    data_assinatura  = lubridate::ymd(lubridate::dmy(data_assinatura)),
    prazo_utilizacao = lubridate::ymd(lubridate::dmy(prazo_utilizacao)),
    periodo_meses    = lubridate::time_length(prazo_utilizacao- data_assinatura, "months"),
    periodo_dias     = lubridate::time_length(prazo_utilizacao - data_assinatura, "days"),
    periodo_anos     = as.integer(lubridate::time_length(prazo_utilizacao - data_assinatura, "years") ),
    contrato2 = paste(contrato, 1:nrow(finep) )
  ) %>%
    dplyr::filter(
    prazo_utilizacao >= "2013-01-01") %>%
    tidyr::drop_na(valor_finep)


   #Old func
#  finep <- func_a(finep,
#                  data_assinatura = finep$data_assinatura ,
#                  data_limite = finep$prazo_utilizacao,
#                  duracao_dias = finep$periodo_dias,
#                  valor_contratado = finep$valor_liberado)

  finep <- func_a(df = finep,
                processo = contrato2,
                data_inicio = data_assinatura,
                prazo_utilizacao = prazo_utilizacao,
                valor_projeto = valor_finep)
  finep <- dtc_categorias(finep,processo = contrato2, motor = motor)
  finep <- finep %>% dplyr::mutate(
    categorias = dplyr::recode(categorias,
                               "character(0" = "nenhuma categoria encontrada"))
  finep <- finep %>% dplyr::mutate(regiao_ag_executor = dplyr::recode(uf,
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
      dplyr::mutate(
        id                           = paste("FINEP",
                                             contrato, sep = "-"),
        fonte_de_dados                 = "FINEP",
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
        valor_executado_2013_2020      = gasto_2013_2020,
        valor_executado_2013           = gasto_2013,
        valor_executado_2014           = gasto_2014,
        valor_executado_2015           = gasto_2015,
        valor_executado_2016           = gasto_2016,
        valor_executado_2017           = gasto_2017,
        valor_executado_2018           = gasto_2018,
        valor_executado_2019           = gasto_2019,
        valor_executado_2020           = gasto_2020)



  finep <- finep %>%
    dplyr::select(id,
           fonte_de_dados,
           data_assinatura,
           data_limite,
           duracao_dias,
           titulo_projeto,
           status_projeto,
           valor_contratado,
           valor_executado_2013_2020,
           nome_agente_financiador,
           natureza_financiamento,
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
           motor,
           categorias
    )



finep
}
