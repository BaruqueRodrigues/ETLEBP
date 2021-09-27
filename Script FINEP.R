### Script FINEP ###
library(tidyverse)
library(janitor)
library(lubridate)
library(readODS)

#fonte dos dados 

url <- "http://www.finep.gov.br/images/acesso-a-informacao/projetos-contratados/2021/14_09_2021_Liberacoes.ods"

finep <- read_ods(path = "data/FINEP/14_09_2021_Liberacoes.ods",
                skip = 4,
                sheet = 1, row_names = TRUE)

finep <- finep %>% clean_names()%>% slice(-1)

finep <- finep %>% filter(prazo_utilizacao >= "01/01/2013",#,
                   instrumento == "Reembolsável" #variável n existe nesse dataset
                 )
finep <- finep %>% mutate(
                   titulo2          = tolower(stringi::stri_trans_general(titulo, "Latin-ASCII")),
                   valor_finep      = as.numeric(valor_finep),
                   valor_liberado   = as.numeric(valor_liberado),
                   data_liberacao   = ymd(dmy(data_liberacao)),
                   data_assinatura  = ymd(dmy(data_assinatura)),
                   prazo_utilizacao = ymd(dmy(prazo_utilizacao)),
                   periodo_meses    = time_length(prazo_utilizacao- data_assinatura, "months"),
                   periodo_dias     = prazo_utilizacao - data_assinatura,
                   periodo_anos     = as.integer(time_length(prazo_utilizacao - data_assinatura, "years") )
                   )



finep <- finep %>% 
         mutate(
           media_gasto = case_when(periodo_anos == 0 ~ valor_finep,
                                        TRUE ~ valor_liberado/periodo_anos),
           gasto_2013  = media_gasto,
           gasto_2014  = case_when(periodo_anos >= 2 ~media_gasto,
                                   TRUE ~ 0),
           gasto_2015  = case_when(periodo_anos >= 3 ~media_gasto,
                                   TRUE ~ 0),
           gasto_2016  = case_when(periodo_anos >= 4 ~media_gasto,
                                   TRUE ~ 0),
           gasto_2017  = case_when(periodo_anos >= 5 ~media_gasto,
                                   TRUE ~ 0),
           gasto_2018  = case_when(periodo_anos >= 6 ~media_gasto,
                                   TRUE ~ 0),
           gasto_2019  = case_when(periodo_anos >= 7 ~media_gasto,
                                   TRUE ~ 0),
           gasto_2020  = case_when(periodo_anos >= 8 ~media_gasto,
                                   TRUE ~ 0),
           gasto_2021  = case_when(periodo_anos >= 9 ~media_gasto,
                                   TRUE ~ 0))

finep <- finep %>%
          mutate( 
                 item                           = paste("Finep",
                                                        contrato, sep = "-"),
                 fonte_de_dados                 = "Finep",
                 data_limite                    = prazo_utilizacao,
                 duracao_dias                   = periodo_dias,
                 duracao_meses                  = periodo_meses,
                 duracao_anos                   = periodo_anos,
                 valor_contratado               = valor_finep,
                 nome_do_agente_financiador     = "Finep",
                 natureza_do_agente_financiador = "Empresa Privada",
                 natureza_do_financiamento      = "Privado orientado por política",
                 modalidade_do_financiamento    = "Reembolsável",
                 nome_do_agente_Executor        = executor,
                 natureza_do_agente_executor    = "Empresa Privada",
                 'P&D_ou_Demonstração'          = "Demonstração",
                 valor_liberado_2013            = gasto_2013,
                 valor_liberado_2014            = gasto_2014,
                 valor_liberado_2015            = gasto_2015,
                 valor_liberado_2016            = gasto_2016,
                 valor_liberado_2017            = gasto_2017,
                 valor_liberado_2018            = gasto_2018,
                 valor_liberado_2019            = gasto_2019,
                 valor_liberado_2020            = gasto_2020,
                 valor_liberado_2021            = gasto_2021)

finep <- finep %>%
              select(item, fonte_de_dados,
                     titulo,
                     titulo2,
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

write.csv(finep, "finep_intermediario_27092021.csv")
