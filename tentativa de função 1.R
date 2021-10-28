

ano_inicio <- 2013
ano_fim <- 2020

anos_periodos <- tibble(
  ano_contagem_dias = ano_inicio:ano_fim,
  inicio_ano_contagem_dias = make_date(ano_contagem_dias, 1, 1 ),
  fim_ano_contagem_dias = make_date(ano_contagem_dias, 12, 31 )
)



anp_resumido <- anp_2015 %>%
  select(
    no_anp,
    data_inicio,
    prazo_utilizacao
  )


calculos <- anp_resumido %>%
  crossing(
    anos_periodos
  ) %>%
  rowwise() %>%
  mutate(
    data_inicio_contagem_projeto = max(data_inicio, inicio_ano_contagem_dias),
    data_fim_contagem_projeto = min(prazo_utilizacao, fim_ano_contagem_dias)
  ) %>%
  ungroup() %>%
  mutate(
    duracao_dias = time_length(data_fim_contagem_projeto - data_inicio_contagem_projeto, unit = "days")
  ) %>%
  mutate(
    duracao_dias = if_else(duracao_dias >= 0, duracao_dias, 0)
  ) %>%
  relocate(
    duracao_dias,
    ano_contagem_dias,
    .after = prazo_utilizacao
  )









