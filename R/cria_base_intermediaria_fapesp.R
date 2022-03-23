#' Cria Base Intermediária da Fapesp
#'
#' @param origem_processos dataset que contém os projetos da fonte fapesp
#'
#' @return
#' @export
#'
#' @examples
#' cria_base_intermediaria_fapesp()
cria_base_intermediaria_fapesp <- function(origem_processos = here::here("data/FAPESP/PROJETOS FAPESP SELECIONADOS INOVA-E - VALORES - 13 dez 2021.xlsx")){


  fapesp2 <- readxl::read_excel(origem_processos) %>%
    janitor::clean_names()

  fapesp2 <- fapesp2 %>% dplyr::mutate(n_processo = paste("FAPESP", n_processo, sep = "-"),
                                       data_inicio = lubridate::ymd(data_de_inicio),
                                       data_termino = lubridate::ymd(data_de_termino),
                                       duracao_dias = lubridate::time_length(data_termino- data_inicio, "days"),
                                       motor = stringi::stri_trans_general(paste(titulo_portugues),
                                                                           "Latin-ASCII"),
                                       motor = tolower(motor))
  fapesp2 <- fapesp2 %>%
    dplyr::slice(1:7488) %>%
    func_a(n_processo, data_inicio,
           data_termino,valor_desembolsado)

  fapesp2 <- fapesp2 %>% dtc_categorias(n_processo,motor)

  fapesp2 <- fapesp2 %>% dplyr::mutate(categorias = dplyr::recode(categorias,
                                                                  "character(0" = "nenhuma categoria encontrada"))

  fapesp2 <- fapesp2 %>%
    dplyr::mutate(
      id                          = n_processo,
      fonte_de_dados              = "FAPESP",
      data_assinatura             = data_inicio,
      data_limite                 = data_termino,
      duracao_dias                = duracao_dias,
      valor_contratado            = valor_desembolsado,
      valor_executado_2013_2020   = gasto_2013_2020,
      nome_agente_financiador     = "FAPESP",
      natureza_agente_financiador = "Empresa Pública", # confirmar
      natureza_financiamento    = "Público",
      modalidade_financiamento    = NA,
      nome_agente_executor        = beneficiario,
      natureza_agente_executor    = "Instituição Pública", # confirmar
      'p&d_ou_demonstracao'       = NA,
      titulo_projeto              = titulo_portugues,
      status_projeto              = NA,
      uf_ag_executor              = "SP",
      regiao_ag_executor          = "SUDESTE",
      valor_executado_2013        = gasto_2013,
      valor_executado_2014        = gasto_2014,
      valor_executado_2015        = gasto_2015,
      valor_executado_2016        = gasto_2016,
      valor_executado_2017        = gasto_2017,
      valor_executado_2018        = gasto_2018,
      valor_executado_2019        = gasto_2019,
      valor_executado_2020        = gasto_2020)

  fapesp2 <- fapesp2 %>%
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
                  categorias)


  fapesp2 <- dplyr::tibble(fapesp2)

  fapesp2
}
