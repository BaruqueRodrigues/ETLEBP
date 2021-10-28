func_a<-function(df, data_assinatura, data_limite, duracao_dias, valor_contratado){
  df <- df %>% dplyr::mutate(n_data_contratacao  = lubridate::ymd(case_when(data_assinatura  < "2013-01-01" ~ lubridate::ymd("2013-01-01"),
                                                          data_assinatura > "2020-12-31" ~ lubridate::ymd("2020-12-31"),
                                                          data_assinatura >= "2013-01-01" ~ data_assinatura)),
                      n_prazo_utilizacao = lubridate::ymd(case_when(data_limite >"2020-12-31" ~ lubridate::ymd("2020-12-31"),
                                                         data_limite <= "2020-12-31" ~ data_limite)),
                      tempo_dias = lubridate::time_length(n_prazo_utilizacao- n_data_contratacao, "days"),
                      media_gasto      = dplyr::case_when(duracao_dias >= 1 ~ (tempo_dias/duracao_dias)* valor_contratado,
                                                   duracao_dias == 0 ~ valor_contratado
                      ),
                      dias_2013 = dplyr::case_when(
                        lubridate::year(n_data_contratacao) == 2013 & lubridate::year(n_prazo_utilizacao) == 2013 ~ lubridate::time_length(n_prazo_utilizacao - n_data_contratacao, "days"),
                        lubridate::year(n_data_contratacao) == 2013 & lubridate::year(n_prazo_utilizacao)  > 2013 ~ lubridate::time_length(ymd("2013-12-31") - n_data_contratacao,  "days"),
                        lubridate::year(n_data_contratacao)  < 2013 & lubridate::year(n_prazo_utilizacao) == 2013  ~ lubridate::time_length(n_prazo_utilizacao - ymd("2013-01-01"),  "days"),
                        lubridate::year(n_data_contratacao)  < 2013 & lubridate::year(n_prazo_utilizacao) > 2013  ~ lubridate::time_length(ymd("2013-12-31") - ymd("2013-01-01"),  "days"),
                        lubridate::year(n_data_contratacao)  > 2013                                     ~ 0),
                      dias_2014 = dplyr::case_when(
                        lubridate::year(n_data_contratacao) == 2014 & lubridate::year(n_prazo_utilizacao) == 2014  ~ lubridate::time_length(n_prazo_utilizacao - n_data_contratacao, "days"),
                        lubridate::year(n_data_contratacao) == 2014 & lubridate::year(n_prazo_utilizacao)  > 2014  ~ lubridate::time_length(ymd("2014-12-31") - n_data_contratacao,  "days"),
                        lubridate::year(n_data_contratacao)  < 2014 & lubridate::year(n_prazo_utilizacao) == 2014  ~ lubridate::time_length(n_prazo_utilizacao - ymd("2014-01-01"),  "days"),
                        lubridate::year(n_data_contratacao)  < 2014 & lubridate::year(n_prazo_utilizacao)  > 2014  ~ lubridate::time_length(ymd("2014-12-31") - ymd("2014-01-01"),  "days"),
                        lubridate::year(n_data_contratacao)  > 2014                                     ~ 0),
                      dias_2015 = dplyr::case_when(
                        lubridate::year(n_data_contratacao) == 2015 & lubridate::year(n_prazo_utilizacao) == 2015  ~ lubridate::time_length(n_prazo_utilizacao - n_data_contratacao, "days"),
                        lubridate::year(n_data_contratacao) == 2015 & lubridate::year(n_prazo_utilizacao)  > 2015  ~ lubridate::time_length(ymd("2015-12-31") - n_data_contratacao,  "days"),
                        lubridate::year(n_data_contratacao)  < 2015 & lubridate::year(n_prazo_utilizacao) == 2015  ~ lubridate::time_length(n_prazo_utilizacao - ymd("2015-01-01"),  "days"),
                        lubridate::year(n_data_contratacao)  < 2015 & lubridate::year(n_prazo_utilizacao)  > 2015  ~ lubridate::time_length(ymd("2015-12-31") - ymd("2015-01-01"),  "days"),
                        lubridate::year(n_data_contratacao)  > 2015                                     ~ 0),
                      dias_2016 = dplyr::case_when(
                        lubridate::year(n_data_contratacao) == 2016 & lubridate::year(n_prazo_utilizacao) == 2016 ~ lubridate::time_length(n_prazo_utilizacao - n_data_contratacao, "days"),
                        lubridate::year(n_data_contratacao) == 2016 & lubridate::year(n_prazo_utilizacao)  > 2016 ~ lubridate::time_length(ymd("2016-12-31") - n_data_contratacao,  "days"),
                        lubridate::year(n_data_contratacao)  < 2016 & lubridate::year(n_prazo_utilizacao) == 2016  ~ lubridate::time_length(n_prazo_utilizacao - ymd("2016-01-01"),  "days"),
                        lubridate::year(n_data_contratacao)  < 2016 & lubridate::year(n_prazo_utilizacao)  > 2016  ~ lubridate::time_length(ymd("2016-12-31") - ymd("2016-01-01"),  "days"),
                        lubridate::year(n_data_contratacao)  > 2016                                     ~ 0),
                      dias_2017 = dplyr::case_when(
                        lubridate::year(n_data_contratacao) == 2017 & lubridate::year(n_prazo_utilizacao) == 2017 ~ lubridate::time_length(n_prazo_utilizacao - n_data_contratacao, "days"),
                        lubridate::year(n_data_contratacao) == 2017 & lubridate::year(n_prazo_utilizacao)  > 2017 ~ lubridate::time_length(ymd("2017-12-31") - n_data_contratacao,  "days"),
                        lubridate::year(n_data_contratacao)  < 2017 & lubridate::year(n_prazo_utilizacao) == 2017  ~ lubridate::time_length(n_prazo_utilizacao - ymd("2017-01-01"),  "days"),
                        lubridate::year(n_data_contratacao)  < 2017 & lubridate::year(n_prazo_utilizacao)  > 2017  ~ lubridate::time_length(ymd("2017-12-31") - ymd("2017-01-01"),  "days"),
                        lubridate::year(n_data_contratacao)  > 2017                                     ~ 0),
                      dias_2018 = dplyr::case_when(
                        lubridate::year(n_data_contratacao) == 2018 & lubridate::year(n_prazo_utilizacao) == 2018 ~ lubridate::time_length(n_prazo_utilizacao - n_data_contratacao, "days"),
                        lubridate::year(n_data_contratacao) == 2018 & lubridate::year(n_prazo_utilizacao)  > 2018 ~ lubridate::time_length(ymd("2018-12-31") - n_data_contratacao,  "days"),
                        lubridate::year(n_data_contratacao)  < 2018 & lubridate::year(n_prazo_utilizacao) == 2018  ~ lubridate::time_length(n_prazo_utilizacao - ymd("2018-01-01"),  "days"),
                        lubridate::year(n_data_contratacao)  < 2018 & lubridate::year(n_prazo_utilizacao)  > 2018  ~ lubridate::time_length(ymd("2018-12-31") - ymd("2018-01-01"),  "days"),
                        lubridate::year(n_data_contratacao)  > 2018                                     ~ 0),
                      dias_2019 = dplyr::case_when(
                        lubridate::year(n_data_contratacao) == 2019 & lubridate::year(n_prazo_utilizacao) == 2019 ~ lubridate::time_length(n_prazo_utilizacao - n_data_contratacao, "days"),
                        lubridate::year(n_data_contratacao) == 2019 & lubridate::year(n_prazo_utilizacao)  > 2019 ~ lubridate::time_length(ymd("2019-12-31") - n_data_contratacao,  "days"),
                        lubridate::year(n_data_contratacao)  < 2019 & lubridate::year(n_prazo_utilizacao) == 2019  ~ lubridate::time_length(n_prazo_utilizacao - ymd("2019-01-01"),  "days"),
                        lubridate::year(n_data_contratacao)  < 2019 & lubridate::year(n_prazo_utilizacao) > 2019  ~ lubridate::time_length(ymd("2019-12-31") - ymd("2019-01-01"),  "days"),
                        lubridate::year(n_data_contratacao)  > 2019                                     ~ 0),
                      dias_2020 = dplyr::case_when(
                        lubridate::year(n_data_contratacao) == 2020 & lubridate::year(n_prazo_utilizacao) == 2020 ~ lubridate::time_length(n_prazo_utilizacao - n_data_contratacao, "days"),
                        lubridate::year(n_data_contratacao) == 2020 & lubridate::year(n_prazo_utilizacao)  > 2020 ~ lubridate::time_length(ymd("2020-12-31") - n_data_contratacao,  "days"),
                        lubridate::year(n_data_contratacao)  < 2020 & lubridate::year(n_prazo_utilizacao) == 2020  ~ lubridate::time_length(n_prazo_utilizacao - ymd("2020-01-01"),  "days"),
                        lubridate::year(n_data_contratacao)  < 2020 & lubridate::year(n_prazo_utilizacao) > 2020  ~ lubridate::time_length(ymd("2020-12-31") - ymd("2020-01-01"),  "days"),
                        lubridate::year(n_data_contratacao)  > 2020                                     ~ 0),
                      gasto_2013 = dplyr::case_when(
                        duracao_dias >= 1 ~  (media_gasto/tempo_dias)* dias_2013,
                        duracao_dias == 0 & lubridate::year(n_data_contratacao) == 2013 ~ media_gasto),
                      gasto_2014 = dplyr::case_when(
                        duracao_dias >= 1 ~  (media_gasto/tempo_dias)* dias_2014,
                        duracao_dias == 0 & lubridate::year(n_data_contratacao) == 2014 ~ media_gasto),
                      gasto_2015 =  dplyr::case_when(
                        duracao_dias >= 1 ~  (media_gasto/tempo_dias)* dias_2015,
                        duracao_dias == 0 & lubridate::year(n_data_contratacao) == 2015 ~ media_gasto),
                      gasto_2016 =  dplyr::case_when(
                        duracao_dias >= 1 ~  (media_gasto/tempo_dias)* dias_2016,
                        duracao_dias == 0 & lubridate::year(n_data_contratacao) == 2016 ~ media_gasto),
                      gasto_2017 =  dplyr::case_when(
                        duracao_dias >= 1 ~  (media_gasto/tempo_dias)* dias_2017,
                        duracao_dias == 0 & lubridate::year(n_data_contratacao) == 2017 ~ media_gasto),
                      gasto_2018 =  dplyr::case_when(
                        duracao_dias >= 1 ~  (media_gasto/tempo_dias)* dias_2018,
                        duracao_dias == 0 & lubridate::year(n_data_contratacao) == 2018 ~ media_gasto),
                      gasto_2019 =  dplyr::case_when(
                        duracao_dias >= 1 ~  (media_gasto/tempo_dias)* dias_2019,
                        duracao_dias == 0 & lubridate::year(n_data_contratacao) == 2019 ~ media_gasto),
                      gasto_2020 =  dplyr::case_when(
                        duracao_dias >= 1 ~  (media_gasto/tempo_dias)* dias_2020,
                        duracao_dias == 0 & lubridate::year(n_data_contratacao) == 2020  ~ media_gasto)

  )
}

usethis::use_data(func_a, overwrite = T)
