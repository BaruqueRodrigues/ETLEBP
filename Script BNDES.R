### Script BNDES ###
library(tidyverse)
library(janitor)
library(lubridate)
library(readxl)
library(httr)
#fonte dos dados  
url <- "https://www.bndes.gov.br/arquivos/central-downloads/operacoes_financiamento/naoautomaticas/naoautomaticas.xlsx"

#read data

bndes <- read_excel("data/BNDES/naoautomaticas.xlsx", skip = 4)%>%clean_names()


bndes <- bndes %>% 
               mutate(prazo_execucao_meses = as.numeric(prazo_carencia_meses) + as.numeric(prazo_amortizacao_meses),
               data_da_contratacao  = ymd(data_da_contratacao),
               descricao_do_projeto2= tolower(descricao_do_projeto),
               prazo_utilizacao     = ymd(data_da_contratacao)%m+%months(prazo_execucao_meses),
               #prazo_decorrido_dias = ymd(Sys.Date()) - ymd(bndes$data_da_contratacao),
               #prazo_decorrido_anos = as.integer(time_length(ymd(Sys.Date()) - ymd(bndes$data_da_contratacao), "years"))
               prazo_decorrido_anos = prazo_execucao_meses/12,
               prazo_decorrido_dias = prazo_decorrido_anos*365
               )%>%
               filter(prazo_utilizacao >= "2013-01-01",
                      inovacao         == "SIM")




bndes <- bndes %>% 
                 mutate(media_gasto = case_when(prazo_decorrido_anos == 0~valor_contratado_r,
                                                TRUE~valor_contratado_r/prazo_decorrido_anos),
                 gasto_2013  = case_when(year(data_da_contratacao) == 2013 ~valor_contratado_r,
                                         prazo_decorrido_anos >=1 ~media_gasto,
                                         TRUE ~ 0),
                 gasto_2014  = case_when(year(data_da_contratacao) == 2014 ~valor_contratado_r,
                                         prazo_decorrido_anos >= 2 ~media_gasto,
                                         TRUE ~ 0),
                 gasto_2015  = case_when(year(data_da_contratacao) == 2015 ~valor_contratado_r,
                                         prazo_decorrido_anos >= 3 ~media_gasto,
                                         TRUE ~ 0),
                 gasto_2016  = case_when(year(data_da_contratacao) == 2016 ~valor_contratado_r,
                                         prazo_decorrido_anos >= 4 ~media_gasto,
                                         TRUE ~ 0),
                 gasto_2017  = case_when(year(data_da_contratacao) == 2017 ~valor_contratado_r,
                                         prazo_decorrido_anos >= 5 ~media_gasto,
                                         TRUE ~ 0),
                 gasto_2018  = case_when(year(data_da_contratacao) == 2018 ~valor_contratado_r,
                                         prazo_decorrido_anos >= 6 ~media_gasto,
                                         TRUE ~ 0),
                 gasto_2019  = case_when(year(data_da_contratacao) == 2019 ~valor_contratado_r,
                                         prazo_decorrido_anos >= 7 ~media_gasto,
                                         TRUE ~ 0),
                 gasto_2020  = case_when(year(data_da_contratacao) == 2020 ~valor_contratado_r,
                                         prazo_decorrido_anos >= 8 ~media_gasto,
                                         TRUE ~ 0),
                 gasto_2021  = case_when(year(data_da_contratacao) == 2021 ~valor_contratado_r,
                                         prazo_decorrido_anos >= 9 ~media_gasto,
                                         TRUE ~ 0))

bndes <- bndes %>% mutate( 
  item                           = paste("Bndes",
                                         numero_do_contrato, sep = "-"),
  fonte_de_dados                 = "Bndes",
  data_assinatura                = data_da_contratacao,
  data_limite                    = prazo_utilizacao,
  duracao_dias                   = prazo_decorrido_dias,
  duracao_meses                  = prazo_execucao_meses,
  duracao_anos                   = prazo_decorrido_anos,
  valor_contratado               = valor_contratado_r,
  nome_do_agente_financiador     = NA,
  natureza_do_agente_financiador = modalidade_de_apoio,
  natureza_do_financiamento      = fonte_de_recurso_desembolsos,
  modalidade_do_financiamento    = modalidade_de_apoio,
  nome_do_agente_Executor        = cliente,
  natureza_do_agente_executor    = natureza_do_cliente,
  'P&D_ou_Demonstração'          = NA ,
  valor_liberado_2013            = gasto_2013,
  valor_liberado_2014            = gasto_2014,
  valor_liberado_2015            = gasto_2015,
  valor_liberado_2016            = gasto_2016,
  valor_liberado_2017            = gasto_2017,
  valor_liberado_2018            = gasto_2018,
  valor_liberado_2019            = gasto_2019,
  valor_liberado_2020            = gasto_2020,
  valor_liberado_2021            = gasto_2021) 

bndes <- bndes%>%
  select(item, fonte_de_dados,
         descricao_do_projeto,
         descricao_do_projeto2,
         data_assinatura,data_limite,
         duracao_dias, duracao_meses,
         duracao_anos, valor_contratado,
         natureza_do_agente_executor,
         natureza_do_financiamento,
         modalidade_do_financiamento,
         nome_do_agente_Executor,
         natureza_do_agente_executor,
         valor_liberado_2013,
         valor_liberado_2014,
         valor_liberado_2015,
         valor_liberado_2016,
         valor_liberado_2017,
         valor_liberado_2018,
         valor_liberado_2019,
         valor_liberado_2020,
         valor_liberado_2021
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

write.csv(bndes, "bndes_inter_27092021.csv")
      
