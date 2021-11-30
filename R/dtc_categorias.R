#' detecta categorias de iea
#'
#' @param df
#' @param processo
#' @param motor
#' @import dplyr
#' @import stringr
#' @import purrr
#' @return
#' @export
#'
#' @examples
dtc_categorias <- function(df,processo, motor){
  detecta_categorias <- function(texto){

    expressoes_capitulos <- tibble(
      categoria = c("1.1", "1.2","1.3",
                    "1.4",
                    "2.1","2.2","2.3",
                    "3.1","3.2","3.3",
                    "3.4","3.5","3.6",
                    "3.7",
                    "4.1","4.2",
                    "5.1","5.2",
                    "6.1","6.2","6.3",
                    "7.1","7.2"),
      expressoes =c(
        str_flatten(
          str_glue("\\b{iea1_1}\\b"),
          collapse = "|"
        ),

        str_flatten(
          str_glue("\\b{iea1_2}\\b"),
          collapse = "|"
        ),
        str_flatten(
          str_glue("\\b{iea1_3}\\b"),
          collapse = "|"
        ),
        str_flatten(
          str_glue("\\b{iea1_4}\\b"),
          collapse = "|"
        ),
        str_flatten(
          str_glue("\\b{iea2_1}\\b"),
          collapse = "|"
        ),
        str_flatten(
          str_glue("\\b{iea2_2}\\b"),
          collapse = "|"
        ),
        str_flatten(
          str_glue("\\b{iea2_3}\\b"),
          collapse = "|"
        ),
        str_flatten(
          str_glue("\\b{iea3_1}\\b"),
          collapse = "|"
        ),
        str_flatten(
          str_glue("\\b{iea3_2}\\b"),
          collapse = "|"
        ),
        str_flatten(
          str_glue("\\b{iea3_3}\\b"),
          collapse = "|"
        ),
        str_flatten(
          str_glue("\\b{iea3_4}\\b"),
          collapse = "|"
        ),
        str_flatten(
          str_glue("\\b{iea3_5}\\b"),
          collapse = "|"
        ),
        str_flatten(
          str_glue("\\b{iea3_6}\\b"),
          collapse = "|"
        ),
        str_flatten(
          str_glue("\\b{iea3_7}\\b"),
          collapse = "|"
        ),
        str_flatten(
          str_glue("\\b{iea4_1}\\b"),
          collapse = "|"
        ),
        str_flatten(
          str_glue("\\b{iea4_2}\\b"),
          collapse = "|"
        ),
        str_flatten(
          str_glue("\\b{iea5_1}\\b"),
          collapse = "|"
        ),
        str_flatten(
          str_glue("\\b{iea5_2}\\b"),
          collapse = "|"
        ),
        str_flatten(
          str_glue("\\b{iea6_1}\\b"),
          collapse = "|"
        ),
        str_flatten(
          str_glue("\\b{iea6_2}\\b"),
          collapse = "|"
        ),
        str_flatten(
          str_glue("\\b{iea6_3}\\b"),
          collapse = "|"
        ),
        str_flatten(
          str_glue("\\b{iea7_1}\\b"),
          collapse = "|"
        ),
        str_flatten(
          str_glue("\\b{iea7_2}\\b"),
          collapse = "|"
        )
      )
    )

    deteccao <- map_lgl(
      .x = expressoes_capitulos$expressoes,
      .f = ~str_detect(string = texto, pattern = .x)
    )

    categorias_encontradas <- expressoes_capitulos %>%
      select(categoria) %>%
      filter(
        deteccao
      )

    saida <- case_when(
      length(categorias_encontradas$categoria) == 0 ~ "Não encontrado",
      length(categorias_encontradas$categoria) == 1 ~ categorias_encontradas$categoria,
      length(categorias_encontradas$categoria) > 1 ~ categorias_encontradas$categoria
    )


  }


  resultado <- df %>%
    select(
      {{processo}},
      {{motor}}
    ) %>%
    rowwise() %>%
    mutate(
      categorias = list(detecta_categorias({{motor}}))
    )

  resultado <- resultado %>% as_tibble() %>%
    mutate(categorias = str_remove_all(str_remove_all(categorias, 'c\\('), "\\)")) %>%
    select(-motor)

  df<-left_join(df, resultado)

  df
}


usethis::use_data(dtc_categorias, overwrite = T)


