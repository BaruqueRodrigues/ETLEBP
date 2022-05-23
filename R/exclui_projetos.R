#' Exclui projetos do sqlite
#'
#' @param fonte string de caracteres da  fonte dos projetos que desejam ser removidos
#' @param sqlite diretório do sqlite
#'
#' @return
#' @export
#'
#' @examples
exclui_projetos<- function(fonte, sqlite){

#Remove da tabela dm_agente_empresa

  diretorio_sqlite <- {{sqlite}}

  con <- DBI::dbConnect(RSQLite::SQLite(),
                        ":memory:",
                        dbname = diretorio_sqlite)

  tbl_dm_agente_empresa <- DBI::dbReadTable(con,"dm_agente_empresa")
  mytbl6 <- DBI::dbReadTable(con,"dm_projeto")
  mytbl7 <- DBI::dbReadTable(con,"ft_dispendio")
  tbl_ft_dispendio <- DBI::dbReadTable(con,"ft_dispendio")

  data <- data.frame(fonte_de_dados = fonte)

  tbl_res <- tbl_ft_dispendio %>%
    select(id_exec, id_formnt) %>%
    left_join(tbl_dm_agente_empresa,
              by = c("id_exec" = "id_agente")) %>% unique()

  fmt <- dplyr::case_when(
    data$fonte_de_dados[1] == "ANEEL" ~ 5,
    data$fonte_de_dados[1] == "BNDES" ~ 6,
    data$fonte_de_dados[1] == "FINEP" ~ 7,
    data$fonte_de_dados[1] == "FNDCT" ~ 8,
    data$fonte_de_dados[1] == "CNPq" ~ 9,
    data$fonte_de_dados[1] == "CNPQ" ~ 9,
    data$fonte_de_dados[1] == "FAPESP" ~ 10,
    data$fonte_de_dados[1] == "ANP" ~ 11,
    data$fonte_de_dados[1] == "CNEM" ~ 12,
    data$fonte_de_dados[1] == "CNEN" ~ 12)

  obj <- tbl_res %>% filter(id_formnt == fmt) %>% select(id_exec) %>%
    mutate(id_item = stringr::str_glue("'{id_exec}'"))

  ex <- obj %>% pull(id_item)

  ex <-  paste0(ex, collapse = ", ")

  txt <- sprintf("DELETE FROM dm_agente_empresa WHERE id_agente IN (%s)",
                 ex)

  RSQLite::dbExecute(con, txt)

  tbl_res <- mytbl7 %>%
    select(id_item, id_formnt) %>%
    left_join(mytbl6,
              by = c("id_item" = "id_item")) %>%
    unique()

# Remove da tabela dm_projeto

  mytbl6 <- DBI::dbReadTable(con,"dm_projeto")
  mytbl7 <- DBI::dbReadTable(con,"ft_dispendio")

  obj <- tbl_res %>% filter(id_formnt == fmt) %>% select(id_item) %>%
    mutate(id_item = stringr::str_glue("'{id_item}'"))

  ex <- obj %>% pull(id_item)

  ex <-  paste0(ex, collapse = ", ")

  txt <- sprintf("DELETE FROM dm_projeto WHERE id_item IN (%s)",
                 ex)

  RSQLite::dbExecute(con, txt)

  # Remove da tabela ft dispendio

  txt <-sprintf(
    "DELETE FROM ft_dispendio WHERE id_formnt = %s",
    fmt)
  RSQLite::dbExecute(con, txt)
}
