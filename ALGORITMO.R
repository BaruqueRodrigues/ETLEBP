

detecta_categorias <- function(texto){

  expressoes_capitulos <- tibble(
    categoria = c("1.1", "1.2"),
    expressoes =c(
      str_flatten(
        str_glue("\\b{iea1_1}\\b"),
        collapse = "|"
      ),

      str_flatten(
        str_glue("\\b{iea1_2}\\b"),
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
    length(categorias_encontradas$categoria) > 1 ~ "Várias"
  )

  saida[1]
}


resultado <- anp_2015 %>%
  select(
    id,
    motor
  ) %>%
  rowwise() %>%
  mutate(
    categorias = list(detecta_categorias(motor))
  )


