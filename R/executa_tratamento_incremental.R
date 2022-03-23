#' executa o tratamento que gera um dataset para a carga incremental do sqlite
#'
#' @param df dataset intermediario
#' @param sqlite diretório do sqlite
#'
#' @return
#' @export
#'
#' @examples
#' executa_tratamento_incremental(df_intermediario_aneel, diretorio_sqlite)
#' executa_tratamento_incremental(df_intermediario_anp, diretorio_sqlite)
#' executa_tratamento_incremental(df_intermediario_bndes, diretorio_sqlite)
#' executa_tratamento_incremental(df_intermediario_cnen, diretorio_sqlite)
executa_tratamento_incremental <- function(df, sqlite){
  data <- {{df}}
  fonte <- {{sqlite}}

  con <- DBI::dbConnect(RSQLite::SQLite(),
                        ":memory:",
                        dbname = fonte)

  #importando tabela com os titulos de projeto
  mytbl6 <- DBI::dbReadTable(con,"dm_projeto")

  data <- data %>%
    dplyr::mutate(existe = ifelse(titulo_projeto %in% mytbl6$'título',
                                  "sim",
                                  "não"))

  data
}
