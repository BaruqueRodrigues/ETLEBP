#' Title
#' @import tidyverse
#' @import tidyr
#' @import lubridate
#' @param df
#' @param processo
#' @param data_inicio
#' @param prazo_utilizacao
#' @param valor_projeto
#'
#' @return
#'
#' @examples

func_a<-function(df,
               processo,
               data_inicio,
               prazo_utilizacao,
               valor_projeto){

  ano_inicio <- 2013
  ano_fim <- 2020

  anos_periodos <- tibble(
    ano_contagem_dias = ano_inicio:ano_fim,
    inicio_ano_contagem_dias = make_date(ano_contagem_dias, 1, 1 ),
    fim_ano_contagem_dias = make_date(ano_contagem_dias, 12, 31 )
  )

  df_resumido <-select(df, {{processo}}, {{data_inicio}},
                       {{prazo_utilizacao}},{{valor_projeto}})


  calculos <- df_resumido %>%
    tidyr::crossing(
      anos_periodos
    ) %>%
    rowwise() %>%
    mutate(
      data_inicio_contagem_projeto = max({{data_inicio}}, inicio_ano_contagem_dias),
      data_fim_contagem_projeto = min({{prazo_utilizacao}}, fim_ano_contagem_dias)
    ) %>%
    ungroup() %>%
    mutate(
      duracao_dias = time_length(data_fim_contagem_projeto - data_inicio_contagem_projeto, unit = "days")
    ) %>%
    group_by({{processo}}) %>%
    mutate(
      duracao_dias = if_else(duracao_dias >= 0, duracao_dias, NA_real_),
      gasto_ano = paste0("gasto_", ano_contagem_dias),
      total_dias_projeto = sum(duracao_dias, na.rm = T)

    ) %>%
    ungroup() %>%
    mutate(gasto_2013_2020 = case_when(duracao_dias >= 1 ~ (total_dias_projeto/time_length({{prazo_utilizacao}}-{{data_inicio}}, "days"))*{{valor_projeto}},
                                       duracao_dias == 0 ~ {{valor_projeto}}),
           valor_gasto_ano = (gasto_2013_2020/total_dias_projeto)*duracao_dias,
           gasto_2013_2020 = case_when(is.na(gasto_2013_2020) == T ~ gasto_2013_2020)) %>%
    tidyr::replace_na(list(valor_gasto_ano = 0, duracao_dias =0 )) %>%

    relocate(
      {{processo}},
      duracao_dias,
      ano_contagem_dias,
      gasto_ano,
      gasto_2013_2020,
      valor_gasto_ano,
      total_dias_projeto,
      .after = {{prazo_utilizacao}}
    )  %>%
    select({{processo}}, gasto_ano,valor_gasto_ano)%>%
    tidyr::pivot_wider(names_from =  gasto_ano,
                       values_from = valor_gasto_ano)%>%
    group_by({{processo}}) %>%
    mutate(gasto_2013_2020 = sum(gasto_2013,gasto_2014,gasto_2015,
                                 gasto_2016,gasto_2017,gasto_2018,
                                 gasto_2019,gasto_2020)) %>%
    ungroup()

  df <-left_join(df, calculos)
}

# usethis::use_data(func_a, overwrite = T)
