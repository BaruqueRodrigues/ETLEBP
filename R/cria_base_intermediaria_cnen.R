#' Cria a base intemediária para o CNEN
#'
#' @import dplyr
#' @import readr
#' @import janitor
#' @import tidyr
#' @import lubridate
#' @import stringr
#' @return
#' @export
#'
#' @examples


cria_base_intermediaria_cnen <- function(){

  cnen <- readxl::read_excel("data/CNEN/Projeto CNEN_Plataforma Inova-E.xlsx",
                     col_types = c("text", "text", "text",
                                   "text", "date", "date", "text", "numeric",
                                   "text", "text", "text", "text", "text",
                                   "text", "text", "text", "text", "text")) %>% clean_names() %>% slice(-c(1,2,3)) %>%
    mutate(data_assinatura = ymd(data_assinatura),
           data_limite     = ymd(data_limite),
           duracao_dias    = time_length(data_limite - data_assinatura, "days"))

  cnen <-func_a(cnen,
                data_assinatura = cnen$data_assinatura,
                data_limite = cnen$data_limite,
                duracao_dias = cnen$duracao_dias,
                valor_contratado = cnen$valor_contratado)

    cnen
}
