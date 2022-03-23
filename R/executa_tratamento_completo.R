#' executa o tratamento que gera um dataset para a carga completa do sqlite
#'
#' @param df dataset tratado
#' @param sqlite diretório do .db do sqlite
#'
#' @return
#' @export
#'
#' @examples
#' executa_tratamento_completo(df_intermediario_aneel, diretorio_sqlite)
#' executa_tratamento_completo(df_intermediario_anp, diretorio_sqlite)
#' executa_tratamento_completo(df_intermediario_bndes, diretorio_sqlite)
#' executa_tratamento_completo(df_intermediario_cnen, diretorio_sqlite)
executa_tratamento_completo <- function(df, sqlite){

  data <- {{df}}
  fonte <- {{sqlite}}

  con <- DBI::dbConnect(RSQLite::SQLite(),
                        ":memory:",
                        dbname = fonte)
  mytbl2 <- DBI::dbReadTable(con,"dm_categoria")
  mytbl3 <- DBI::dbReadTable(con,"dm_formentador")
  mytbl6 <- DBI::dbReadTable(con,"dm_projeto")
  mytbl7 <- DBI::dbReadTable(con,"ft_dispendio")

  #inserir categorias IEA do sqlite na base

  #criando um objeto com informações sobre as categorias de IEA
  consulta <- dplyr::select(mytbl7, id_item, id_cat2, id_formnt)

  #puxando as descrições das categorias
  consulta <- dplyr::left_join(consulta, mytbl2[,c("id","cat2")],
                               by = c("id_cat2" = "id"))
  #trazendo as informações de fomentador
  consulta <- dplyr::left_join(consulta, mytbl3[,c("id_formentador","nme_form")],
                               by = c("id_formnt"= "id_formentador"))

  #trazendo as informações de titulo
  consulta <- dplyr::left_join(consulta, mytbl6[,c("título", "id_item")])

  #fazendo o merge
  data <- dplyr::left_join(data, consulta[,c ("título", "cat2") ],
                           by = c("titulo_projeto"= "título")) %>% unique() %>%
    rename(categoria_sqlite = cat2)

  # criar uma coluna que diz se o caso já existe no sqlite
  #
  # data <- dplyr::left_join(data, consulta, by = c("id"= "id_item")) %>% unique() %>%
  #   select(-cat2.x, -id_cat2, -id_formnt, -cat2.y,
  #          -nme_form, -título)

  data
}
