#' Cria a base intemediária para a anp criando um dataframe
#' @import dplyr
#' @import tidyr
#' @import lubridate
#' @import stringr
#' @import readr
#' @return
#' @export
#'
#' @examples
cria_base_intermediaria_anp <- function(origem_processos = here::here("data/ANP/projetos-rt-3-2015.csv"),
                                        origem_enriquecimento = here::here("data/ANP/anp_agregados_declarados.xlsx - Plan1.csv")) {

  anp_2015 <- readr::read_delim(origem_processos,
                         ";", escape_double = FALSE, trim_ws = TRUE) %>%
              janitor::clean_names()
  anp_2015 <- anp_2015 %>%
    dplyr::mutate(
      valor_clausula       = as.numeric(stringr::str_replace_all(
                             stringr::str_remove_all(valor_clausula, "[.R$ ]"), "[,]", ".")),
      data_inicio          = lubridate::dmy(data_inicio),
      prazo_utilizacao     = data_inicio + months(prazo),
      prazo_decorrido_dias = lubridate::time_length(prazo_utilizacao - data_inicio, "days"),
      prazo_decorrido_anos = as.integer(prazo_decorrido_dias/365),
      motor                = stringi::stri_trans_general(paste(titulo, objetivo, tema, subtema),
                                                         "Latin-ASCII"),
      motor                = tolower(motor)
    ) %>%
    dplyr::filter(prazo_utilizacao >= "2013-01-01") %>%
    tidyr::drop_na(valor_clausula) %>%
    func_a(#df = anp_2015,
           processo = no_anp,
           data_inicio = data_inicio,
           prazo_utilizacao = prazo_utilizacao,
           valor_projeto = valor_clausula)

  df<-anp_2015%>%dplyr::summarise(gasto_2013 = sum(gasto_2013),
                                  gasto_2014 = sum(gasto_2014, na.rm = T),
                                  gasto_2015 = sum(gasto_2015, na.rm = T),
                                  gasto_2016 = sum(gasto_2016, na.rm = T),
                                  gasto_2017 = sum(gasto_2017, na.rm = T),
                                  gasto_2018 = sum(gasto_2018, na.rm = T))

  anp_agregados <- read.csv(origem_enriquecimento, dec=",") %>%
    dplyr::slice(-2,-3)

  anp_2015 <- rbind(anp_2015, anp_agregados)
  anp_2015 <- anp_2015 %>% dplyr::mutate(gasto_2013 = 0,
                      gasto_2014 = 0,
                      gasto_2015 = 0,
                      gasto_2016 = 0,
                      gasto_2017 = 0,
                      gasto_2018 = 0,
                      gasto_2013_2020 = gasto_2013 + gasto_2014 + gasto_2015 +
                                        gasto_2016 + gasto_2017 + gasto_2018 +
                                        gasto_2019 + gasto_2020
                      )


anp_2015 <-dtc_categorias(anp_2015,no_anp, motor)
anp_2015 <- anp_2015 %>% dplyr::mutate(categorias = dplyr::recode(categorias,
                                                                 "character(0" = "nenhuma categoria encontrada"))
  #Old func
#  anp_2015 <- func_a(anp_2015,
#                     data_assinatura = anp_2015$data_inicio,
#                     data_limite = anp_2015$prazo_utilizacao,
#                     duracao_dias = anp_2015$prazo_decorrido_dias,
#                     valor_contratado = anp_2015$valor_clausula)

#anp_2015 <-

  anp_2015<-anp_2015 %>%
            dplyr::mutate(
            id                = paste("ANP", no_anp, sep = "-"),
            fonte_de_dados              = "ANP",
            titulo_projeto              = titulo,
            data_assinatura             = data_inicio,
            data_limite                 = prazo_utilizacao,
            duracao_meses               = prazo,
            duracao_dias                = prazo_decorrido_dias,
            duracao_anos                = prazo_decorrido_anos,
            valor_contratado            = valor_clausula,
            valor_executado_2013_2020   = gasto_2013_2020,
            nome_agente_financiador     = empresa_responsavel,
            natureza_agente_financiador = "Empresa Privada", # confirmar
            natureza_financiamento      = "publicamente orientado",
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
            natureza_financiamento,
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
            motor,
            categorias)

  anp_2015
}
