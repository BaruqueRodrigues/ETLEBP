#' Cria a base intemediária para o CNEN criando um dataframe
#'
#' @param origem_processos dataset que contém os projetos da fonte cnen
#'
#' @import dplyr
#' @import tidyr
#' @import lubridate
#' @import stringr
#' @return
#' @export
#'
#' @examples
#' cria_base_intermediaria_cnen()


cria_base_intermediaria_cnen <- function(origem_processos = here::here("data/CNEN/Projeto CNEN_Plataforma Inova-E.xlsx") ){

  cnen <- readxl::read_excel(origem_processos,
                     col_types = c("text", "text", "text",
                                   "text", "date", "date", "text", "numeric",
                                   "text", "text", "text", "text", "text",
                                   "text", "text", "text", "text", "text")) %>%
    janitor::clean_names() %>%
    dplyr::slice(-c(1,2,3)) %>%
    dplyr::mutate(data_assinatura = lubridate::ymd(data_assinatura),
           data_limite     = lubridate::ymd(data_limite),
           duracao_dias    = lubridate::time_length(data_limite - data_assinatura, "days"))

#  cnen <-func_a(cnen,
#                data_assinatura = cnen$data_assinatura,
#                data_limite = cnen$data_limite,
#                duracao_dias = cnen$duracao_dias,
#                valor_contratado = cnen$valor_contratado)

  cnen <- func_a(df = cnen,
               processo = id,
               data_inicio = data_assinatura,
               prazo_utilizacao = data_limite,
               valor_projeto = valor_contratado)
  cnen<-cnen %>%
    dplyr::mutate(
    titulo_projeto                  = titulo,
    status_projeto                  = NA,
    nome_agente_financiador         = nome_do_agente_financiador,
    nome_agente_executor            = nome_do_agente_executor,
    natureza_agente_financiador     = natureza_do_agente_financiador,
    natureza_financiamento          = natureza_do_financiamento,
    natureza_agente_executor        = natureza_do_agente_executor,
    modalidade_financiamento        = modalidade_do_financiamento,
    uf_ag_executor                  = uf_execucao,
    valor_executado_2013_2020       = gasto_2013_2020,
    valor_executado_2013            = gasto_2013,
    valor_executado_2014            = gasto_2014,
    valor_executado_2015            = gasto_2015,
    valor_executado_2016            = gasto_2016,
    valor_executado_2017            = gasto_2017,
    valor_executado_2018            = gasto_2018,
    valor_executado_2019            = gasto_2019,
    valor_executado_2020            = gasto_2020
  ) %>%
    dplyr::mutate(regiao_ag_executor = dplyr::recode(uf_ag_executor,
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
                                       "TO" = "N"))%>%
    dplyr::select(
    id,
    fonte_de_dados,
    data_assinatura,data_limite,
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
    natureza_agente_executor,
    natureza_financiamento,
    modalidade_financiamento,
    valor_executado_2013,
    valor_executado_2014,
    valor_executado_2015,
    valor_executado_2016,
    valor_executado_2017,
    valor_executado_2018,
    valor_executado_2019,
    valor_executado_2020)

    cnen
}
