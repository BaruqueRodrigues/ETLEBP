#' faz o calculo do projeto guarda chuva.
#'
#' @param base dataframe que contem o projeto de energia
#' @param ft_guarda_chuva subconjunto dos projeto que agregam os valores de financiamento do projeto
#'
#' @return
#' @export
#'
#' @examples
#' fun_proj_gc(dataset, projeto_guarda_chuva)
fun_proj_gc <- function(base, ft_guarda_chuva){

  ft_guarda_chuva <- {{ft_guarda_chuva}}
  data <- {{base}}

soma_anp <-  data %>%
  dplyr::filter(categorias %in% c(1.1, 1.2, 1.3, 1.4,
                                  2.1, 2.2, 2.3,
                                  3.1, 3.2, 3.3, 3.4, 3.5, 3.6, 3.7,
                                  4.1, 4.2,
                                  5.1, 5.2,
                                  6.1, 6.2, 6.3,
                                  7.1, 7.2)) %>%
  dplyr::select(id, valor_executado_2013:valor_executado_2020) %>%
  tidyr::gather(ano, vlr, -id) %>%
  dplyr::mutate(ano = dplyr::recode(ano,
                                    "valor_executado_2013" = 2013,
                                    "valor_executado_2014" = 2014,
                                    "valor_executado_2015" = 2015,
                                    "valor_executado_2016" = 2016,
                                    "valor_executado_2017" = 2017,
                                    "valor_executado_2018" = 2018,
                                    "valor_executado_2019" = 2019,
                                    "valor_executado_2020" = 2020),
                ano = as.character(ano)) %>%
  dplyr::rename(id_item = id) %>%
  dplyr::group_by(ano) %>%
  dplyr::summarise(vlr = sum(vlr))

att_guarda_chuva <- dplyr::left_join(soma_anp, ft_guarda_chuva[,c(3,13)],
                                     by = c("ano" ="ano"),
                                     suffix = c("_alterar", "_original")) %>%
  dplyr::mutate(vlr =  vlr_original- vlr_alterar,
                id_item = "ANP") %>%
  tidyr::replace_na(list(vlr_original = 0, vlr =0 ))

att_guarda_chuva
}

#fun_proj_gc(base = anp, ft_guarda_chuva = ft_guarda_chuva)

