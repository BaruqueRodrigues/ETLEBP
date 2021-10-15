### Script BNDES ###
library(tidyverse)
library(janitor)
library(lubridate)
library(readxl)
library(httr)
options(scipen = 999)
#fonte dos dados  
url <- "https://www.bndes.gov.br/arquivos/central-downloads/operacoes_financiamento/naoautomaticas/naoautomaticas.xlsx"

#read data

bndes <- read_excel("data/BNDES/naoautomaticas.xlsx", skip = 4)%>%clean_names()


bndes <- bndes %>% 
               mutate(prazo_execucao_meses = as.numeric(prazo_carencia_meses) + as.numeric(prazo_amortizacao_meses),
               data_da_contratacao   = ymd(data_da_contratacao),
               descricao_do_projeto2 = tolower(
                                       stringi::stri_trans_general(descricao_do_projeto, "Latin-ASCII")),
               prazo_utilizacao      = ymd(data_da_contratacao)%m+%months(prazo_execucao_meses),
               
               prazo_decorrido_anos  = as.integer(time_length(prazo_utilizacao- data_da_contratacao, "years")),
               prazo_decorrido_dias  = time_length(prazo_utilizacao- data_da_contratacao, "days")
               )%>%
               filter(prazo_utilizacao >= "2013-01-01",
                      inovacao         == "SIM")%>%drop_na(valor_contratado_r)

bndes <- bndes %>%
               mutate(n_data_contratacao  = ymd(case_when(data_da_contratacao  < "2013-01-01" ~ ymd("2013-01-01"),
                                                          data_da_contratacao > "2020-12-31" ~ ymd("2020-12-31"),
                                                          data_da_contratacao >= "2013-01-01" ~ data_da_contratacao)),
                      n_prazo_utilizacao = ymd(case_when(prazo_utilizacao >"2020-12-31" ~ ymd("2020-12-31"),
                                                         prazo_utilizacao <= "2020-12-31" ~ prazo_utilizacao)),
                      tempo_dias = time_length(n_prazo_utilizacao- n_data_contratacao, "days"),
                      media_gasto      = case_when(prazo_decorrido_dias >= 1 ~ (tempo_dias/prazo_decorrido_dias)* valor_contratado_r,
                                                   prazo_decorrido_dias == 0 ~ valor_contratado_r),
                      
                      dias_2013 = case_when(
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
                        prazo_decorrido_dias >= 1 ~  (media_gasto/tempo_dias)* dias_2013,
                        prazo_decorrido_dias == 0 & year(n_data_contratacao) == 2013 ~ media_gasto),
                      gasto_2014 = case_when(
                        prazo_decorrido_dias >= 1 ~  (media_gasto/tempo_dias)* dias_2014,
                        prazo_decorrido_dias == 0 & year(n_data_contratacao) == 2014 ~ media_gasto),
                      gasto_2015 =  case_when(
                        prazo_decorrido_dias >= 1 ~  (media_gasto/tempo_dias)* dias_2015,
                        prazo_decorrido_dias == 0 & year(n_data_contratacao) == 2015 ~ media_gasto),
                      gasto_2016 =  case_when(
                        prazo_decorrido_dias >= 1 ~  (media_gasto/tempo_dias)* dias_2016,
                        prazo_decorrido_dias == 0 & year(n_data_contratacao) == 2016 ~ media_gasto),
                      gasto_2017 =  case_when(
                        prazo_decorrido_dias >= 1 ~  (media_gasto/tempo_dias)* dias_2017,
                        prazo_decorrido_dias == 0 & year(n_data_contratacao) == 2017 ~ media_gasto),
                      gasto_2018 =  case_when(
                        prazo_decorrido_dias >= 1 ~  (media_gasto/tempo_dias)* dias_2018,
                        prazo_decorrido_dias == 0 & year(n_data_contratacao) == 2018 ~ media_gasto),
                      gasto_2019 =  case_when(
                        prazo_decorrido_dias >= 1 ~  (media_gasto/tempo_dias)* dias_2019,
                        prazo_decorrido_dias == 0 & year(n_data_contratacao) == 2019 ~ media_gasto),
                      gasto_2020 =  case_when(
                        prazo_decorrido_dias >= 1 ~  (media_gasto/tempo_dias)* dias_2020,
                        prazo_decorrido_dias == 0 & year(n_data_contratacao) == 2020  ~ media_gasto)
                      
               )



#validacao
bndes %>% select(cliente,data_da_contratacao,prazo_utilizacao,prazo_decorrido_dias, n_data_contratacao,n_prazo_utilizacao,
                 valor_contratado_r, media_gasto, gasto_2013,gasto_2014,gasto_2015,gasto_2016,gasto_2017,
                 gasto_2018,gasto_2019,gasto_2020,
                 )%>% View()


bndes <-bndes %>% mutate(regiao_ag_executor = recode(uf,
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
                                       "TO" = "N"))


bndes <- bndes %>% mutate( 
  id                           = paste("Bndes",
                                         numero_do_contrato, sep = "-"),
  titulo_projeto = descricao_do_projeto,
  fonte_de_dados                 = "Bndes",
  data_assinatura                = data_da_contratacao,
  data_limite                    = prazo_utilizacao,
  duracao_dias                   = prazo_decorrido_dias,
  status_projeto                 = situacao_do_contrato,
  duracao_meses                  = prazo_execucao_meses,
  duracao_anos                   = prazo_decorrido_anos,
  valor_contratado               = valor_contratado_r,
  valor_executado_2013_2020      = media_gasto,
  nome_agente_financiador     = "Bndes",
  natureza_agente_financiador = "empresa pública",
  natureza_financiamento      = "pública",
  modalidade_financiamento    = modalidade_de_apoio,
  nome_agente_Executor        = cliente,
  natureza_agente_executor    = natureza_do_cliente,
  'p&d_ou_demonstracao'          = NA ,
  uf_ag_executor                  = uf,
  valor_executado_2013            = gasto_2013,
  valor_executado_2014            = gasto_2014,
  valor_executado_2015            = gasto_2015,
  valor_executado_2016            = gasto_2016,
  valor_executado_2017            = gasto_2017,
  valor_executado_2018            = gasto_2018,
  valor_executado_2019            = gasto_2019,
  valor_executado_2020            = gasto_2020) 

bndes <- bndes%>%
  select(id,
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
         nome_agente_Executor,
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
         valor_executado_2020,
         descricao_do_projeto2
         )

  bndes <- bndes %>% 
        mutate(iea1_1 = str_detect(descricao_do_projeto2, iea1_1),
               iea1_2 = str_detect(descricao_do_projeto2, iea1_2),
               iea1_3 = str_detect(descricao_do_projeto2, iea1_3),
               iea1_4 = str_detect(descricao_do_projeto2, iea1_4),
               iea2_1 = str_detect(descricao_do_projeto2, iea2_1),
               iea2_2 = str_detect(descricao_do_projeto2, iea2_2),
               iea2_3 = str_detect(descricao_do_projeto2, iea2_3),
               iea3_1 = str_detect(descricao_do_projeto2, iea3_1),
               iea3_2 = str_detect(descricao_do_projeto2, iea3_2),
               iea3_3 = str_detect(descricao_do_projeto2, iea3_3),
               iea3_4 = str_detect(descricao_do_projeto2, iea3_4),
               iea3_5 = str_detect(descricao_do_projeto2, iea3_5),
               iea3_6 = str_detect(descricao_do_projeto2, iea3_6),
               iea3_7 = str_detect(descricao_do_projeto2, iea3_7),
               iea4_1 = str_detect(descricao_do_projeto2, iea4_1),
               iea4_2 = str_detect(descricao_do_projeto2, iea4_2),
               iea5_1 = str_detect(descricao_do_projeto2, iea5_1),
               iea5_2 = str_detect(descricao_do_projeto2, iea5_2),
               iea6_1 = str_detect(descricao_do_projeto2, iea6_1),
               iea6_2 = str_detect(descricao_do_projeto2, iea6_2),
               iea6_3 = str_detect(descricao_do_projeto2, iea6_3),
               iea7_1 = str_detect(descricao_do_projeto2, iea7_1),
               iea7_2 = str_detect(descricao_do_projeto2, iea7_2)
)


write.csv(bndes, "bndes_inter_06_10_2021.csv")
      
