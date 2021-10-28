### Script CNEN
library(tidyverse)
library(readr)
library(janitor)
library(lubridate)
library(readxl)
options(scipen=999)

cnen <- read_excel("data/CNEN/Projeto CNEN_Plataforma Inova-E.xlsx",
                   col_types = c("text", "text", "text",
                                 "text", "date", "date", "text", "numeric",
                                 "text", "text", "text", "text", "text",
                                 "text", "text", "text", "text", "text")) %>% clean_names() %>% slice(-c(1,2,3)) %>%
  mutate(data_assinatura = ymd(data_assinatura),
         data_limite     = ymd(data_limite),
         duracao_dias    = time_length(data_limite - data_assinatura, "days"))

cnen <- cnen %>%
  mutate(n_data_contratacao  = ymd(case_when(data_assinatura  < "2013-01-01" ~ ymd("2013-01-01"),
                                             data_assinatura > "2020-12-31" ~ ymd("2020-12-31"),
                                             data_assinatura >= "2013-01-01" ~ data_assinatura)),
         n_prazo_utilizacao = ymd(case_when(data_limite >"2020-12-31" ~ ymd("2020-12-31"),
                                            data_limite <= "2020-12-31" ~ data_limite)),
         tempo_dias = time_length(n_prazo_utilizacao- n_data_contratacao, "days"),
         media_gasto      = case_when(duracao_dias >= 1 ~ (tempo_dias/duracao_dias)* valor_contratado,
                                      duracao_dias == 0 ~ valor_contratado
         ),dias_2013 = case_when(
           year(n_data_contratacao) == 2013 & year(n_prazo_utilizacao) == 2013 ~ time_length(n_prazo_utilizacao - n_data_contratacao, "days"),
           year(n_data_contratacao) == 2013 & year(n_prazo_utilizacao)  > 2013 ~ time_length(ymd("2013-12-31") - n_data_contratacao,  "days"),
           year(n_data_contratacao)  < 2013 & year(n_prazo_utilizacao) == 2013  ~ time_length(n_prazo_utilizacao - ymd("2013-01-01"),  "days"),
           year(n_data_contratacao)  < 2013 & year(n_prazo_utilizacao) > 2013  ~ time_length(ymd("2013-12-31") - ymd("2013-01-01"),  "days"),
           year(n_data_contratacao)  > 2013                                     ~ 0),
         dias_2014 = case_when(
           year(n_data_contratacao) == 2014 & year(n_prazo_utilizacao) == 2014  ~ time_length(n_prazo_utilizacao - n_data_contratacao, "days"),
           year(n_data_contratacao) == 2014 & year(n_prazo_utilizacao)  > 2014  ~ time_length(ymd("2014-12-31") - n_data_contratacao,  "days"),
           year(n_data_contratacao)  < 2014 & year(n_prazo_utilizacao) == 2014  ~ time_length(n_prazo_utilizacao - ymd("2014-01-01"),  "days"),
           year(n_data_contratacao)  < 2014 & year(n_prazo_utilizacao)  > 2014  ~ time_length(ymd("2014-12-31") - ymd("2014-01-01"),  "days"),
           year(n_data_contratacao)  > 2014                                     ~ 0),
         dias_2015 = case_when(
           year(n_data_contratacao) == 2015 & year(n_prazo_utilizacao) == 2015  ~ time_length(n_prazo_utilizacao - n_data_contratacao, "days"),
           year(n_data_contratacao) == 2015 & year(n_prazo_utilizacao)  > 2015  ~ time_length(ymd("2015-12-31") - n_data_contratacao,  "days"),
           year(n_data_contratacao)  < 2015 & year(n_prazo_utilizacao) == 2015  ~ time_length(n_prazo_utilizacao - ymd("2015-01-01"),  "days"),
           year(n_data_contratacao)  < 2015 & year(n_prazo_utilizacao)  > 2015  ~ time_length(ymd("2015-12-31") - ymd("2015-01-01"),  "days"),
           year(n_data_contratacao)  > 2015                                     ~ 0),
         dias_2016 = case_when(
           year(n_data_contratacao) == 2016 & year(n_prazo_utilizacao) == 2016 ~ time_length(n_prazo_utilizacao - n_data_contratacao, "days"),
           year(n_data_contratacao) == 2016 & year(n_prazo_utilizacao)  > 2016 ~ time_length(ymd("2016-12-31") - n_data_contratacao,  "days"),
           year(n_data_contratacao)  < 2016 & year(n_prazo_utilizacao) == 2016  ~ time_length(n_prazo_utilizacao - ymd("2016-01-01"),  "days"),
           year(n_data_contratacao)  < 2016 & year(n_prazo_utilizacao)  > 2016  ~ time_length(ymd("2016-12-31") - ymd("2016-01-01"),  "days"),
           year(n_data_contratacao)  > 2016                                     ~ 0),
         dias_2017 = case_when(
           year(n_data_contratacao) == 2017 & year(n_prazo_utilizacao) == 2017 ~ time_length(n_prazo_utilizacao - n_data_contratacao, "days"),
           year(n_data_contratacao) == 2017 & year(n_prazo_utilizacao)  > 2017 ~ time_length(ymd("2017-12-31") - n_data_contratacao,  "days"),
           year(n_data_contratacao)  < 2017 & year(n_prazo_utilizacao) == 2017  ~ time_length(n_prazo_utilizacao - ymd("2017-01-01"),  "days"),
           year(n_data_contratacao)  < 2017 & year(n_prazo_utilizacao)  > 2017  ~ time_length(ymd("2017-12-31") - ymd("2017-01-01"),  "days"),
           year(n_data_contratacao)  > 2017                                     ~ 0),
         dias_2018 = case_when(
           year(n_data_contratacao) == 2018 & year(n_prazo_utilizacao) == 2018 ~ time_length(n_prazo_utilizacao - n_data_contratacao, "days"),
           year(n_data_contratacao) == 2018 & year(n_prazo_utilizacao)  > 2018 ~ time_length(ymd("2018-12-31") - n_data_contratacao,  "days"),
           year(n_data_contratacao)  < 2018 & year(n_prazo_utilizacao) == 2018  ~ time_length(n_prazo_utilizacao - ymd("2018-01-01"),  "days"),
           year(n_data_contratacao)  < 2018 & year(n_prazo_utilizacao)  > 2018  ~ time_length(ymd("2018-12-31") - ymd("2018-01-01"),  "days"),
           year(n_data_contratacao)  > 2018                                     ~ 0),
         dias_2019 = case_when(
           year(n_data_contratacao) == 2019 & year(n_prazo_utilizacao) == 2019 ~ time_length(n_prazo_utilizacao - n_data_contratacao, "days"),
           year(n_data_contratacao) == 2019 & year(n_prazo_utilizacao)  > 2019 ~ time_length(ymd("2019-12-31") - n_data_contratacao,  "days"),
           year(n_data_contratacao)  < 2019 & year(n_prazo_utilizacao) == 2019  ~ time_length(n_prazo_utilizacao - ymd("2019-01-01"),  "days"),
           year(n_data_contratacao)  < 2019 & year(n_prazo_utilizacao) > 2019  ~ time_length(ymd("2019-12-31") - ymd("2019-01-01"),  "days"),
           year(n_data_contratacao)  > 2019                                     ~ 0),
         dias_2020 = case_when(
           year(n_data_contratacao) == 2020 & year(n_prazo_utilizacao) == 2020 ~ time_length(n_prazo_utilizacao - n_data_contratacao, "days"),
           year(n_data_contratacao) == 2020 & year(n_prazo_utilizacao)  > 2020 ~ time_length(ymd("2020-12-31") - n_data_contratacao,  "days"),
           year(n_data_contratacao)  < 2020 & year(n_prazo_utilizacao) == 2020  ~ time_length(n_prazo_utilizacao - ymd("2020-01-01"),  "days"),
           year(n_data_contratacao)  < 2020 & year(n_prazo_utilizacao) > 2020  ~ time_length(ymd("2020-12-31") - ymd("2020-01-01"),  "days"),
           year(n_data_contratacao)  > 2020                                     ~ 0),
         gasto_2013 = case_when(
           duracao_dias >= 1 ~  (media_gasto/tempo_dias)* dias_2013,
           duracao_dias == 0 & year(n_data_contratacao) == 2013 ~ media_gasto),
         gasto_2014 = case_when(
           duracao_dias >= 1 ~  (media_gasto/tempo_dias)* dias_2014,
           duracao_dias == 0 & year(n_data_contratacao) == 2014 ~ media_gasto),
         gasto_2015 =  case_when(
           duracao_dias >= 1 ~  (media_gasto/tempo_dias)* dias_2015,
           duracao_dias == 0 & year(n_data_contratacao) == 2015 ~ media_gasto),
         gasto_2016 =  case_when(
           duracao_dias >= 1 ~  (media_gasto/tempo_dias)* dias_2016,
           duracao_dias == 0 & year(n_data_contratacao) == 2016 ~ media_gasto),
         gasto_2017 =  case_when(
           duracao_dias >= 1 ~  (media_gasto/tempo_dias)* dias_2017,
           duracao_dias == 0 & year(n_data_contratacao) == 2017 ~ media_gasto),
         gasto_2018 =  case_when(
           duracao_dias >= 1 ~  (media_gasto/tempo_dias)* dias_2018,
           duracao_dias == 0 & year(n_data_contratacao) == 2018 ~ media_gasto),
         gasto_2019 =  case_when(
           duracao_dias >= 1 ~  (media_gasto/tempo_dias)* dias_2019,
           duracao_dias == 0 & year(n_data_contratacao) == 2019 ~ media_gasto),
         gasto_2020 =  case_when(
           duracao_dias >= 1 ~  (media_gasto/tempo_dias)* dias_2020,
           duracao_dias == 0 & year(n_data_contratacao) == 2020  ~ media_gasto)

  )
cnen<-cnen %>% mutate(
                titulo_projeto                  = titulo,
                status_projeto                  = NA,
                nome_agente_financiador         = nome_do_agente_financiador,
                nome_agente_executor            = nome_do_agente_executor,
                natureza_agente_financiador     = natureza_do_agente_financiador,
                natureza_financiamento          = natureza_do_financiamento,
                natureza_agente_executor        = natureza_do_agente_executor,
                modalidade_financiamento        = modalidade_do_financiamento,
                uf_ag_executor                  = uf_execucao,
                valor_executado_2013_2020       = media_gasto,
                valor_executado_2013            = gasto_2013,
                valor_executado_2014            = gasto_2014,
                valor_executado_2015            = gasto_2015,
                valor_executado_2016            = gasto_2016,
                valor_executado_2017            = gasto_2017,
                valor_executado_2018            = gasto_2018,
                valor_executado_2019            = gasto_2019,
                valor_executado_2020            = gasto_2020
                                                            ) %>%
  mutate(regiao_ag_executor = recode(uf_ag_executor,
                                     "AC" = "N",
                                     "AL" = "NE",
                                     "AM" = "N",
                                     "BA" = "NE",
                                     "CE" = "NE",
                                     "DF" = "CO",
                                     "ES" = "SE",
                                     "GO" = "CO",
                                     "MA" = "NE",
                                     "MG" = "SE",
                                     "MS" = "CO",
                                     "MT" = "CO",
                                     "PA" = "N",
                                     "PB" = "NE",
                                     "PE" = "NE",
                                     "PI" = "NE",
                                     "PR" = "S",
                                     "RJ" = "SE",
                                     "RN" = "NE",
                                     "RO" = "N",
                                     "RS" = "S",
                                     "SC" = "S",
                                     "SE" = "NE",
                                     "SP" = "SE",
                                     "TO" = "N"))%>% select(id,
                fonte_de_dados,
                data_assinatura,data_limite,
                duracao_dias,
                titulo_projeto,
                status_projeto,
                valor_contratado,
                valor_executado_2013_2020,
                nome_agente_financiador,
                natureza_agente_financiador,
                modalidade_financiamento,
                nome_agente_executor,
                natureza_agente_executor,
                uf_ag_executor,
                regiao_ag_executor,
                natureza_agente_executor,
                natureza_financiamento,
                modalidade_financiamento,
                valor_executado_2013,
                valor_executado_2014,
                valor_executado_2015,
                valor_executado_2016,
                valor_executado_2017,
                valor_executado_2018,
                valor_executado_2019,
                valor_executado_2020)
