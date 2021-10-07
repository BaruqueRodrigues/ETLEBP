### ETL ANEEL ###
library(tidyverse)
library(janitor)
library(lubridate)
options(scipen=999)
##get the data ##
url<- "https://www.aneel.gov.br/documents/656831/16363897/SGPED_BI.zip/32560baa-fd7f-5a0d-3687-9085ae0dedc7"

#importando o dataset
anel_pd <- read_delim("data/SGPED_BI/PD Busca Textual.csv", 
                     ";", escape_double = FALSE, locale = locale(encoding = "WINDOWS-1252"), 
                     trim_ws = TRUE) %>% clean_names() 

anel_pd <- anel_pd%>%
                    mutate(data_de_carregamento   = as_date(dmy_hms(data_de_carregamento)),
                           data_de_conclusao      = dmy(data_de_conclusao),
                           custo_total_previsto   = as.numeric(str_replace_all(
                                                    str_replace_all(custo_total_previsto, "[.$]", ""), "[,]", "." )),
                           custo_total_realizado  = as.numeric(str_replace_all(
                                                    str_replace_all(custo_total_realizado, "[.$]", ""), "[,]", "." )),
                           duracao_prevista       = date(data_de_carregamento+dmonths(anel_pd$duracao_prevista_meses)),
                           duracao_dias           = interval(data_de_carregamento, data_de_conclusao)/ddays(),
                           duracao_anos           = as.integer(interval(data_de_carregamento, data_de_conclusao)/dyears()),
                           data_de_conclusao      = case_when(is.na(data_de_conclusao) ~ duracao_prevista,
                                                              TRUE~data_de_conclusao ),
                           motor                  = stringi::stri_trans_general(paste(titulo,segmento,tema),
                                                                                "Latin-ASCII"),
                           motor                  = tolower(motor)) %>%
                    filter(data_de_conclusao >= "2013-01-01") %>% drop_na(custo_total_previsto)


anel_pd <- anel_pd%>%
  mutate(n_data_contratacao  = ymd(case_when(data_de_carregamento  < "2013-01-01" ~ ymd("2013-01-01"),
                                             data_de_carregamento > "2020-12-31" ~ ymd("2020-12-31"),
                                             data_de_carregamento >= "2013-01-01" ~ data_de_carregamento)),
         n_prazo_utilizacao = ymd(case_when(duracao_prevista >"2020-12-31" ~ ymd("2020-12-31"),
                                            duracao_prevista <= "2020-12-31" ~ duracao_prevista)),
         tempo_dias = time_length(n_prazo_utilizacao- n_data_contratacao, "days"),
         media_gasto      = case_when(duracao_dias >= 1 ~ (tempo_dias/duracao_dias)* custo_total_realizado,
                                      duracao_dias == 0 ~ custo_total_realizado
         ),
         gasto_2013       = case_when(
           n_data_contratacao < "2013-01-01" ~ media_gasto/time_length(ymd("2013-12-31")- ymd("2013-01-01"), "days"),
           n_data_contratacao > "2013-12-31" ~ 0,
           n_data_contratacao >= "2013-01-01" & n_data_contratacao <= "2013-12-31" ~ media_gasto/time_length(ymd("2013-12-31")- n_data_contratacao, "days") ),
         
         gasto_2014       = case_when(
           n_data_contratacao < "2014-01-01" ~ media_gasto/time_length(ymd("2014-12-31")- ymd("2014-01-01"), "days"),
           n_data_contratacao > "2014-12-31" ~ 0,
           n_data_contratacao >= "2014-01-01" & n_data_contratacao <= "2014-12-31" ~ media_gasto/time_length(ymd("2014-12-31")- n_data_contratacao, "days") ),
         
         gasto_2015       = case_when(
           n_data_contratacao < "2015-01-01" ~ media_gasto/time_length(ymd("2015-12-31")- ymd("2015-01-01"), "days"),
           n_data_contratacao > "2015-12-31" ~ 0,
           n_data_contratacao >= "2015-01-01" & n_data_contratacao <= "2015-12-31" ~ media_gasto/time_length(ymd("2015-12-31")- n_data_contratacao, "days") ),
         
         gasto_2016       = case_when(
           n_data_contratacao < "2016-01-01" ~ media_gasto/time_length(ymd("2016-12-31")- ymd("2016-01-01"), "days"),
           n_data_contratacao > "2016-12-31" ~ 0,
           n_data_contratacao >= "2016-01-01" & n_data_contratacao <= "2016-12-31" ~ media_gasto/time_length(ymd("2016-12-31")- n_data_contratacao, "days") ),
         
         gasto_2017       = case_when(
           n_data_contratacao < "2017-01-01" ~ media_gasto/time_length(ymd("2017-12-31")- ymd("2017-01-01"), "days"),
           n_data_contratacao > "2017-12-31" ~ 0,
           n_data_contratacao >= "2017-01-01" & n_data_contratacao <= "2017-12-31" ~ media_gasto/time_length(ymd("2017-12-31")- n_data_contratacao, "days") ),
         
         gasto_2018       = case_when(
           n_data_contratacao < "2018-01-01" ~ media_gasto/time_length(ymd("2018-12-31")- ymd("2018-01-01"), "days"),
           n_data_contratacao > "2018-12-31" ~ 0,
           n_data_contratacao >= "2018-01-01" & n_data_contratacao <= "2018-12-31" ~ media_gasto/time_length(ymd("2018-12-31")- n_data_contratacao, "days") ),
         
         gasto_2019       = case_when(
           n_data_contratacao < "2019-01-01" ~ media_gasto/time_length(ymd("2019-12-31")- ymd("2019-01-01"), "days"),
           n_data_contratacao > "2019-12-31" ~ 0,
           n_data_contratacao >= "2019-01-01" & n_data_contratacao <= "2019-12-31" ~ media_gasto/time_length(ymd("2019-12-31")- n_data_contratacao, "days") ),
         
         gasto_2020       = case_when(
           n_data_contratacao < "2020-01-01" ~ media_gasto/time_length(ymd("2020-12-31")- ymd("2020-01-01"), "days"),
           n_data_contratacao > "2020-12-31" ~ 0,
           n_data_contratacao >= "2020-01-01" & n_data_contratacao <= "2020-12-31" ~ media_gasto/time_length(ymd("2020-12-31")- n_data_contratacao, "days") )
         )
  
aneel_time <- read_csv2("data/SGPED_BI/5.PD RF EQUIPE.csv")%>% 
              filter(`Tipo de Entidade` == "Proponente")  %>%
              select(CodProj,`Entidade Vinculada`,`Unidade Federativa`) %>%
              distinct() %>%
              clean_names()

anel_pd <- left_join(anel_pd, aneel_time, by = "cod_proj")

anel_pd<- anel_pd %>% mutate(regiao_ag_executor = recode(unidade_federativa,
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
  anel_pd <- anel_pd %>%
            mutate(id                          = paste("Aneel", cod_proj, sep = "-"),
                   fonte_dados                 = "Aneel",
                   data_assinatura             = data_de_carregamento,
                   data_limite                 = data_de_conclusao,
                   duracao_dias                = duracao_dias,
                   duracao_meses               = duracao_prevista,
                   valor_contratado            = custo_total_previsto,
                   valor_executado_2013_2020   = media_gasto,
                   nome_agente_financiador     = proponente,
                   natureza_agente_financiador = "Empresa Privada", # confirmar
                   modalidade_financiamento    = NA,
                   nome_agente_executor        = entidade_vinculada,
                   natureza_agente_executor    = "Empresa Privada", # confirmar 
                   'p&d_ou_demonstracao'       = NA,
                   titulo_projeto              = titulo,
                   status_projeto              = situacao,
                   uf_ag_executor              = unidade_federativa,
                   valor_executado_2013        = gasto_2013,
                   valor_executado_2014        = gasto_2014,
                   valor_executado_2015        = gasto_2015,
                   valor_executado_2016        = gasto_2016,
                   valor_executado_2017        = gasto_2017,
                   valor_executado_2018        = gasto_2018,
                   valor_executado_2019        = gasto_2019,
                   valor_executado_2020        = gasto_2020,
                   motor
                   ) 

anel_pd <- anel_pd %>%
        select(
         id,
         fonte_dados,
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
         valor_executado_2013,valor_executado_2014,
         valor_executado_2015,valor_executado_2016,
         valor_executado_2017,valor_executado_2018,
         valor_executado_2019,valor_executado_2020,
         motor)

anel_pd <- anel_pd %>% 
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
            


anel_pd <- anel_pd %>% select(-motor)

write.csv(anel_pd, "aneel_interm_06_10_2021.csv")
