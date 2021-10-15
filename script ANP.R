### Script Leitura ANP
library(tidyverse)
library(readr)
library(janitor)
library(lubridate)
options(scipen=999)
url1 <- "https://www.gov.br/anp/pt-br/centrais-de-conteudo/dados-abertos/arquivos/arquivos-pdi/projetos-rt-5-2005.csv"

url2 <- "https://www.gov.br/anp/pt-br/centrais-de-conteudo/dados-abertos/arquivos/arquivos-pdi/projetos-rt-3-2015.csv" 

anp_2015 <- read_delim("data/ANP/projetos-rt-3-2015.csv", 
                       ";", escape_double = FALSE, trim_ws = TRUE) %>% clean_names()
anp_2015 <- anp_2015 %>%
             mutate(valor_clausula       = as.numeric(str_replace_all(
                                           str_remove_all(valor_clausula, "[.R$ ]"), "[,]", ".")),
                    data_inicio          = dmy(data_inicio),
                    prazo_utilizacao     = data_inicio %m+% months(prazo),
                    prazo_decorrido_dias = time_length(prazo_utilizacao - data_inicio, "day"),
                    prazo_decorrido_anos = as.integer(prazo_decorrido_dias/365),
                    motor                = stringi::stri_trans_general(paste(titulo, objetivo, tema, subtema),
                                                                       "Latin-ASCII"),
                    motor                = tolower(motor)
                    ) %>%filter(prazo_utilizacao >= "2013-01-01") %>% drop_na(valor_clausula)

anp_2015 <- anp_2015 %>%
  mutate(n_data_contratacao  = ymd(case_when(data_inicio  < "2013-01-01" ~ ymd("2013-01-01"),
                                             data_inicio > "2020-12-31" ~ ymd("2020-12-31"),
                                             data_inicio >= "2013-01-01" ~ data_inicio)),
         n_prazo_utilizacao = ymd(case_when(prazo_utilizacao >"2020-12-31" ~ ymd("2020-12-31"),
                                            prazo_utilizacao <= "2020-12-31" ~ prazo_utilizacao)),
         tempo_dias = time_length(n_prazo_utilizacao- n_data_contratacao, "days"),
         media_gasto      = case_when(prazo_decorrido_dias >= 1 ~ (tempo_dias/prazo_decorrido_dias)* valor_clausula,
                                      prazo_decorrido_dias == 0 ~ valor_clausula
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
anp_2015 %>% select( no_anp, data_inicio,prazo_utilizacao,n_data_contratacao,n_prazo_utilizacao,
                     prazo_decorrido_dias,tempo_dias,
                      valor_clausula,media_gasto,
                     gasto_2013,gasto_2014,gasto_2015,gasto_2016,gasto_2017,
                     gasto_2018,gasto_2019,gasto_2020)%>% View()

anp_2015<-anp_2015 %>% mutate(id                = paste("Anp", no_anp, sep = "-"),
                    fonte_de_dados              = "Anp",
                    titulo_projeto              = titulo,
                    data_assinatura             = data_inicio,
                    data_limite                 = prazo_utilizacao,
                    duracao_meses               = prazo,
                    duracao_dias                = prazo_decorrido_dias,
                    duracao_anos                = prazo_decorrido_anos,
                    valor_contratado            = valor_clausula,
                    valor_executado_2013_2020   = media_gasto,
                    nome_agente_financiador     = empresa_responsavel,
                    natureza_agente_financiador = "Empresa Privada", # confirmar
                    natureza_financiamneto      = "publicamente orientado",
                    modalidade_financiamento    = NA,
                    nome_agente_executor        = executor_1,
                    natureza_agente_executor    = 'Empresa Privada', # confirmar
                    'p&d_ou_demonstracao'       = NA,
                    uf_ag_executor              = NA,
                    regiao_ag_executor          = NA,
                    status_projeto              = NA,
                    valor_executado_2013        = gasto_2013,
                    valor_executado_2014        = gasto_2014,
                    valor_executado_2015        = gasto_2015,
                    valor_executado_2016        = gasto_2016,
                    valor_executado_2017        = gasto_2017,
                    valor_executado_2018        = gasto_2018,
                    valor_executado_2019        = gasto_2019,
                    valor_executado_2020        = gasto_2020)

anp_2015<-anp_2015 %>% select(id,
                              fonte_de_dados,
                              data_assinatura,
                              data_limite,
                              duracao_dias,
                              titulo_projeto,
                              status_projeto,
                              valor_contratado,
                              valor_executado_2013_2020,
                              nome_agente_financiador,
                              natureza_financiamneto,
                              modalidade_financiamento,
                              nome_agente_executor,
                              natureza_agente_executor,
                              uf_ag_executor,
                              regiao_ag_executor,
                              `p&d_ou_demonstracao`,
                              valor_executado_2013,valor_executado_2014,
                              valor_executado_2015,valor_executado_2016,
                              valor_executado_2017,valor_executado_2018,
                              valor_executado_2019,valor_executado_2020,
                              motor)

anp_2015 <- anp_2015 %>%
   mutate(iea1_1 = str_detect(motor, iea1_1),
          iea1_2 = str_detect(motor, iea1_2),
          iea1_3 = str_detect(motor, iea1_3),
          iea1_4 = str_detect(motor, iea1_4),
          iea2_1 = str_detect(motor, iea2_1),
          iea2_2 = str_detect(motor, iea2_2),
          iea2_3 = str_detect(motor, iea2_3),
          iea3_1 = str_detect(motor, iea3_1),
          iea3_2 = str_detect(motor, iea3_2),
          iea3_3 = str_detect(motor, iea3_3),
          iea3_4 = str_detect(motor, iea3_4),
          iea3_5 = str_detect(motor, iea3_5),
          iea3_6 = str_detect(motor, iea3_6),
          iea3_7 = str_detect(motor, iea3_7),
          iea4_1 = str_detect(motor, iea4_1),
          iea4_2 = str_detect(motor, iea4_2),
          iea5_1 = str_detect(motor, iea5_1),
          iea5_2 = str_detect(motor, iea5_2),
          iea6_1 = str_detect(motor, iea6_1),
          iea6_2 = str_detect(motor, iea6_2),
          iea6_3 = str_detect(motor, iea6_3),
          iea7_1 = str_detect(motor, iea7_1),
          iea7_2 = str_detect(motor, iea7_2)
   )


anp_2005 <- read_delim("data/ANP/projetos-rt-5-2005.csv", 
                       ";", escape_double = FALSE, trim_ws = TRUE) %>% clean_names()

write.csv(anp_2015,"anp_2015_interm_06_10_2021.csv")
