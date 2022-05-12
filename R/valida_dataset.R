#' Função que insere as alterações feitas na estação de validação da interface EBPInterface
#'
#' @param dataset_enriquecido dataset que receberá os atributos validados pelo analista
#' @param dataset_insercao dataset que contém os atributos validados pelo analista
#'
#' @return
#' @export
#'
#' @examples valida_dataset(anp_intermediario, anp_intermediario_validado)
valida_dataset<- function(dataset_enriquecido,dataset_insercao){

  data_enriq <- dataset_enriquecido
  data_inser <- dataset_insercao

  data_enriq <- data_enriq %>% dplyr::mutate(id = as.character(id))

  data_inser <- data_inser %>% dplyr::mutate(id = as.character(id))

  data <- dplyr::left_join(data_enriq, data_inser,
                           by = c("id" = "id"),
                           suffix = c("_original", "_inserido")) %>%
    unique()

  data <- data %>%
    dplyr::select(-contains("_original"))%>%
    dplyr::rename_at(dplyr::vars(dplyr::contains("_inserido")), list(~stringr::str_remove(., "_inserido")))
}
