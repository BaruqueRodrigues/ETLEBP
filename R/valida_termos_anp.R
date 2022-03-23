#' reclassifica em cima de alguns parametros a classificação da função dtc_categorias
#'
#' @param df dataframe que contem os projetos de energia
#' @param categorias atributo que mede as classificações das categorias de IEA
#'
#' @return
#' @export
#'
#' @examples
#' valida_termos_anp(dataset, categorias)
valida_termos_anp <- function(df, categorias){

  #caso >1 categorias & categorias %in% 2.1
  df <- df %>%
    #p_cat é um vetor que indica se o caso deve ou não ser reclassificado
    #aqui indicamos se ele contém mais de uma categoria de IEA ou não foi classificado em nenhuma categoria
    dplyr::mutate(p_cat = ifelse(!categorias %in% c("1.1", "1.2","1.3",
                                             "1.4",
                                             "2.1","2.2","2.3",
                                             "3.1","3.2","3.3",
                                             "3.4","3.5","3.6",
                                             "3.7",
                                             "4.1","4.2",
                                             "5.1","5.2",
                                             "6.1","6.2","6.3",
                                             "7.1","7.2"), "reclassificar", "não reclassificar")) %>%
    #Para aqueles que tem mais de uma categoria de IEA removemos a categoria 2.1  e mantemos as demais
    dplyr::mutate(categorias = ifelse(p_cat == "reclassificar", stringr::str_remove_all(categorias,'"2.1", '), categorias ),
           categorias = ifelse(p_cat == "reclassificar", stringr::str_remove_all(categorias,', "2.1"'), categorias ))

  # lidando com casos que ainda contém mais de uma categoria ou não tiveram categorias encontradas
  df <- df %>%
    dplyr::mutate(
      categorias = stringr::str_remove_all(categorias, '"'),
      p_cat = ifelse(!categorias %in% c("1.1", "1.2","1.3",
                                        "1.4",
                                        "2.1","2.2","2.3",
                                        "3.1","3.2","3.3",
                                        "3.4","3.5","3.6",
                                        "3.7",
                                        "4.1","4.2",
                                        "5.1","5.2",
                                        "6.1","6.2","6.3",
                                        "7.1","7.2"), "reclassificar", "não reclassificar")) %>%
    dplyr::mutate(rec_class_anp = ifelse(p_cat == "reclassificar" & area %in% c("TEMAS TRANSVERSAIS",
                                                                         "OUTRAS FONTES DE ENERGIA"), "sim", "não" )) %>%
    #Quando está classificado na área temas transversais ou Outras fontes de energia
    #Recorrer a coluna Tema da ANP e reclassificar
    dplyr::mutate(categorias = dplyr::case_when(rec_class_anp == "sim" & tema == "ENERGIA SOLAR" ~ "3.1",
                                  rec_class_anp == "sim" & tema == "HIDROGÊNIO" ~ "5.1",
                                  rec_class_anp == "sim" & tema == "OUTRAS FONTES ALTERNATIVAS" ~ "3.9",
                                  rec_class_anp == "sim" & tema == "SEGURANÇA E MEIO AMBIENTE" ~ "7.1",
                                  rec_class_anp == "sim" & tema == "AVALIAÇÃO DA CONFORMIDADE, MONITORAMENTO E CONTROLE" ~ "7.1",
                                  rec_class_anp == "sim" & tema == "DISTRIBUIÇÃO, LOGÍSTICA E TRANSPORTE" ~ "2.1",
                                  rec_class_anp == "sim" & tema == "MATERIAIS" ~ "2.1",
                                  TRUE ~ categorias)) %>%
    #Quando está classificado na área temas transversais ou Outras fontes de energia
    #Recorrer a coluna area anp e reclassificar
    dplyr::mutate(categorias = dplyr::case_when(p_cat == "reclassificar" & rec_class_anp == "não" & area =="ABASTECIMENTO" ~ "2.1",
                                  p_cat == "reclassificar" & rec_class_anp == "não" & area =="BIOCOMBUSTÍVEIS" ~ "3.4",
                                  p_cat == "reclassificar" & rec_class_anp == "não" & area =="EXPLORAÇÃO E PRODUÇÃO DE PETRÓLEO E GÁS NATURAL - ONSHORE E OFFSHORE" ~ "2.1",
                                  p_cat == "reclassificar" & rec_class_anp == "não" & area =="GÁS NATURAL" ~ "2.1",
                                  p_cat == "reclassificar" & rec_class_anp == "não" & area =="REGULAÇÃO DO SETOR DE PETRÓLEO, GÁS NATURAL E BIOCOMBUSTÍVEIS" ~ "7.1",
                                  TRUE ~ categorias))

  df
}





