### Script FINEP ###
library(tidyverse)
library(janitor)
library(lubridate)
library(readODS)
options(scipen = 999)
#fonte dos dados

url <- "http://www.finep.gov.br/images/acesso-a-informacao/projetos-contratados/2021/14_09_2021_Liberacoes.ods"

finep <- read_ods(path = "data/FINEP/14_09_2021_Liberacoes.ods",
                skip = 4,
                sheet = 1)

finep <- finep %>% clean_names()%>% slice(-1)


finep <- finep %>% mutate(
                   titulo2          = tolower(stringi::stri_trans_general(titulo, "Latin-ASCII")),
                   valor_finep      = as.numeric(valor_finep),
                   valor_liberado   = as.numeric(valor_liberado),
                   data_liberacao   = ymd(dmy(data_liberacao)),
                   data_assinatura  = ymd(dmy(data_assinatura)),
                   prazo_utilizacao = ymd(dmy(prazo_utilizacao)),
                   periodo_meses    = time_length(prazo_utilizacao- data_assinatura, "months"),
                   periodo_dias     = time_length(prazo_utilizacao - data_assinatura, "days"),
                   periodo_anos     = as.integer(time_length(prazo_utilizacao - data_assinatura, "years") )
                   ) %>%
                   filter(prazo_utilizacao >= "2013-01-01",#,
                               instrumento == "Reembolsável") %>%
    drop_na(valor_finep)



finep <- finep %>%
         mutate(n_data_contratacao  = ymd(case_when(data_assinatura  < "2013-01-01" ~ ymd("2013-01-01"),
                                                    data_assinatura > "2020-12-31" ~ ymd("2020-12-31"),
                                                    data_assinatura >= "2013-01-01" ~ data_assinatura)),
                n_prazo_utilizacao = ymd(case_when(prazo_utilizacao >"2020-12-31" ~ ymd("2020-12-31"),
                                                   prazo_utilizacao <= "2020-12-31" ~ prazo_utilizacao)),
                tempo_dias = time_length(n_prazo_utilizacao - n_data_contratacao, "days"),
                media_gasto      = case_when(periodo_dias >= 1 ~ (tempo_dias/periodo_dias)* valor_finep,
                                             periodo_dias == 0 ~ valor_finep
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
                  periodo_dias >= 1 ~  (media_gasto/tempo_dias)* dias_2013,
                  periodo_dias == 0 & year(n_data_contratacao) == 2013 ~ media_gasto),
                gasto_2014 = case_when(
                  periodo_dias >= 1 ~  (media_gasto/tempo_dias)* dias_2014,
                  periodo_dias == 0 & year(n_data_contratacao) == 2014 ~ media_gasto),
                gasto_2015 =  case_when(
                  periodo_dias >= 1 ~  (media_gasto/tempo_dias)* dias_2015,
                  periodo_dias == 0 & year(n_data_contratacao) == 2015 ~ media_gasto),
                gasto_2016 =  case_when(
                  periodo_dias >= 1 ~  (media_gasto/tempo_dias)* dias_2016,
                  periodo_dias == 0 & year(n_data_contratacao) == 2016 ~ media_gasto),
                gasto_2017 =  case_when(
                  periodo_dias >= 1 ~  (media_gasto/tempo_dias)* dias_2017,
                  periodo_dias == 0 & year(n_data_contratacao) == 2017 ~ media_gasto),
                gasto_2018 =  case_when(
                  periodo_dias >= 1 ~  (media_gasto/tempo_dias)* dias_2018,
                  periodo_dias == 0 & year(n_data_contratacao) == 2018 ~ media_gasto),
                gasto_2019 =  case_when(
                  periodo_dias >= 1 ~  (media_gasto/tempo_dias)* dias_2019,
                  periodo_dias == 0 & year(n_data_contratacao) == 2019 ~ media_gasto),
                gasto_2020 =  case_when(
                  periodo_dias >= 1 ~  (media_gasto/tempo_dias)* dias_2020,
                  periodo_dias == 0 & year(n_data_contratacao) == 2020  ~ media_gasto)

         )


finep <- finep %>% mutate(regiao_ag_executor = recode(uf,
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
                                                      "TO" = "N"
))

finep <- finep %>%
          mutate(
                 id                           = paste("Finep",
                                                        contrato, sep = "-"),
                 fonte_de_dados                 = "Finep",
                 data_limite                    = prazo_utilizacao,
                 duracao_dias                   = periodo_dias,
                 duracao_meses                  = periodo_meses,
                 duracao_anos                   = periodo_anos,
                 valor_contratado               = valor_finep,
                 titulo_projeto                 = titulo,
                 status_projeto                 = status,
                 nome_agente_financiador        = "Finep",
                 natureza_agente_financiador    = "Empresa Pública",
                 natureza_financiamento         = "pública",
                 modalidade_financiamento       = instrumento,
                 nome_agente_executor           = proponente,
                 natureza_agente_executor       = "Empresa Privada", #confirmar natureza juridica proponente
                 'p&d_ou_demonstracao'          = "Demonstração",
                 uf_ag_executor                 = uf,
                 valor_executado_2013_2020      = media_gasto,
                 valor_executado_2013           = gasto_2013,
                 valor_executado_2014           = gasto_2014,
                 valor_executado_2015           = gasto_2015,
                 valor_executado_2016           = gasto_2016,
                 valor_executado_2017           = gasto_2017,
                 valor_executado_2018           = gasto_2018,
                 valor_executado_2019           = gasto_2019,
                 valor_executado_2020           = gasto_2020)



finep <- finep %>%
              select(id,
                     fonte_de_dados,
                     data_assinatura,
                     data_limite,
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
                     `p&d_ou_demonstracao`,
                     valor_executado_2013,
                     valor_executado_2014,
                     valor_executado_2015,
                     valor_executado_2016,
                     valor_executado_2017,
                     valor_executado_2018,
                     valor_executado_2019,
                     valor_executado_2020,
                     titulo2
                     )

finep <- finep %>%
  mutate(iea1_1 = str_detect(titulo2, iea1_1),
         iea1_2 = str_detect(titulo2, iea1_2),
         iea1_3 = str_detect(titulo2, iea1_3),
         iea1_4 = str_detect(titulo2, iea1_4),
         iea2_1 = str_detect(titulo2, iea2_1),
         iea2_2 = str_detect(titulo2, iea2_2),
         iea2_3 = str_detect(titulo2, iea2_3),
         iea3_1 = str_detect(titulo2, iea3_1),
         iea3_2 = str_detect(titulo2, iea3_2),
         iea3_3 = str_detect(titulo2, iea3_3),
         iea3_4 = str_detect(titulo2, iea3_4),
         iea3_5 = str_detect(titulo2, iea3_5),
         iea3_6 = str_detect(titulo2, iea3_6),
         iea3_7 = str_detect(titulo2, iea3_7),
         iea4_1 = str_detect(titulo2, iea4_1),
         iea4_2 = str_detect(titulo2, iea4_2),
         iea5_1 = str_detect(titulo2, iea5_1),
         iea5_2 = str_detect(titulo2, iea5_2),
         iea6_1 = str_detect(titulo2, iea6_1),
         iea6_2 = str_detect(titulo2, iea6_2),
         iea6_3 = str_detect(titulo2, iea6_3),
         iea7_1 = str_detect(titulo2, iea7_1),
         iea7_2 = str_detect(titulo2, iea7_2)
  )

write.csv(finep, "finep_intermediario_06_10_2021.csv")
