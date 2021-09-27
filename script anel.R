### ETL ANEEL ###
library(tidyverse)
library(janitor)
library(lubridate)

##get the data ##
url<- "https://www.aneel.gov.br/documents/656831/16363897/SGPED_BI.zip/32560baa-fd7f-5a0d-3687-9085ae0dedc7"

#importando o dataset
anel_pd<- read_delim("data/SGPED_BI/PD Busca Textual.csv", 
              ";", escape_double = FALSE, locale = locale(encoding = "WINDOWS-1252"), 
              trim_ws = TRUE)

## Removendo as informações indesejadas

#removendo projetos cancelados
anel_pd<-anel_pd%>%filter(Situação != "CANCELADO")                    

#checando  casos com Custo Total == NA
is.na(anel_pd$`Custo Total Previsto`)%>%table()
is.na(anel_pd$`Custo Total Realizado`)%>%table()

#devido a n° de NA a decisão é utilizar a variável Custo Total Previsto

# corrigindo o formato das datas

anel_pd<-anel_pd%>% mutate(`Data de Carregamento`=dmy(paste(
                str_sub(anel_pd$`Data de Carregamento`, start = 1,end = 2),
                str_sub(anel_pd$`Data de Carregamento`, start = 3,end = 5)%>%
                recode("JAN" = "01",
                       "FEB" = "02",
                       "MAR" = "03",
                       "APR" = "04",
                       "MAY" = "05",
                       "JUN" = "06",
                       "JUL" = "07",
                       "AUG" = "08",
                       "SEP" = "09",
                       "OCT" = "10",
                       "NOV" = "11",
                       "DEC" = "12"),
                        paste(20,str_sub(anel_pd$`Data de Carregamento`, start = 6,end = 7),sep = ""),
                        sep = "-")),
                `Data de Conclusão` = dmy(paste(
                  str_sub(anel_pd$`Data de Conclusão`, start = 1,end = 2),
                  str_sub(anel_pd$`Data de Conclusão`, start = 3,end = 5)%>%
                    recode("JAN" = "01",
                           "FEB" = "02",
                           "MAR" = "03",
                           "APR" = "04",
                           "MAY" = "05",
                           "JUN" = "06",
                           "JUL" = "07",
                           "AUG" = "08",
                           "SEP" = "09",
                           "OCT" = "10",
                           "NOV" = "11",
                           "DEC" = "12"),
                  paste(20,str_sub(anel_pd$`Data de Conclusão`, start = 6,end = 7),sep = ""),
                  sep = "-")))

#removendo os .$, da variável dinheiro.  
anel_pd<-anel_pd%>%mutate(`Custo Total Previsto` = as.numeric(str_replace_all(anel_pd$`Custo Total Previsto`, "[.,$]", "")),
                 `Custo Total Realizado`= as.numeric(str_replace_all(anel_pd$`Custo Total Realizado`, "[.,$]", ""))
         
                 )
#colocando as colunas em um formato tidy
anel_pd<-anel_pd%>%clean_names()

#removendo NA da variável custo total previsto
anel_pd<-anel_pd%>%drop_na(custo_total_previsto)

#criando a duração prevista como um formato de ano
anel_pd<-anel_pd%>%
  mutate(duracao_prevista = date(data_de_carregamento+dmonths(anel_pd$duracao_prevista_meses)),
         data_de_conclusao= case_when(is.na(data_de_conclusao) ~ duracao_prevista,
                                      TRUE~data_de_conclusao ))

#criando a variável duração dias
anel_pd<-anel_pd%>%mutate(duracao_dias = interval(data_de_carregamento, data_de_conclusao)/ddays())

## Corrigindo string das variaveis titulo, segmento e tema
#removendo acentos
anel_pd<-anel_pd%>%mutate(titulo = stringi::stri_trans_general(titulo, "Latin-ASCII"),
                 segmento = stringi::stri_trans_general(segmento, "Latin-ASCII"),
                 tema = stringi::stri_trans_general(tema, "Latin-ASCII"))

#colocando as variáveis em minusculas
anel_pd<-anel_pd%>%mutate(titulo = str_to_lower(titulo),
                 segmento = str_to_lower(segmento),
                 tema = str_to_lower(tema),
                 motor = paste(titulo,segmento,tema))

### construindo vetores com IEAs para contruir a variável ####
iea1_1 <- c("motor", 
       "maquina eletrica", 
       "equipamento", 
       "sistema eletrico", 
       "aparelho", # 5
       "etiquetagem",
       "metrologia", 
       "processo industr", "eficiencia energetica", "eficiencia",
       "injetora", #11
       "processo",
       "energ", "industr",
       "refrigeracao" #15
)

iea1_2<- c("conforto termico",
           "edificio",
           "edificao",
           "conforto ambiental",
           "habita",
           "iluminacao", #6
           "lampada",
           "led",
           "resfriamento",
           "refrigeracao", # 10
           "compressor",
           "bomba de calor",
           "eficiencia",
           "etiquetagem" #14
)

iea1_3 <- c("bateria", "transporte",  "veiculo",
            "veiculo eletrico.",
            "motor", "gasolina", "diesel", "etanol", "eficiente" # 9
)

iea1_4 <- c("eficiencia","energetica")

iea2_1 <- c("petroleo", 
            "oleo",
            "gas natural",
            "gas",
            "xisto betuminoso",
            "hidrocarboneto",
            "offshore",
            "plataforma de petr.leo",
            "semissubmersivel",
            "fpso",
            "arvore de natal",
            "construco naval",
            "reservatorio",
            "recuperacao avan.ada",
            "geoengenharia",
            "escoamento",
            "bombeamento",
            "risers",
            "refino",
            "derivado de petroleo",
            "gasolina",
            "oleo diesel",
            "nafta", #23
            "combustao", "petroleo", "gas natural", "derivado de petroleo", #27
            "turbina a gas",
            "turbina a vapor",
            "turbina de avi.o",
            "gerador a diesel",
            "dutos",
            "oleodutos",
            "tanques",
            "gasodutos",
            "gnl", #36
            "armazenamento", "petroleo", "gas natural", "derivado de petroleo")

iea2_2 <- c("carvao",
            "carvao mineral",
            "combustao", "carvao",
            "conversao", "carvao")

iea2_3 <- c("co2", "captura", "separacao",
            "escoamento", "armazenamento")

iea3_1 <- c("solar",
            "energia",
            "aquecimento",
            "arrefecimento",
            "termica",
            "concentrador", #6
            "fotovoltaica",
            "celula solar",
            "modulo fotovoltaico",
            "sistema fotovoltaico",
            "heliotermica",
            "aplicao", "alta" , "temperatura")

iea3_2 <- c("energia", "eolica",
            "eolica", "onshore", "offshore", "turbina",
            "usinade vento",
            "rotor",
            "aerogerador",
            "conversor")

iea3_3 <- c("energia", "oce.nica", "mar.", "ondas",
            "poder", "gradiente", "salinidade")

iea3_4 <-c("biocombustivel", "liquido", "solido",
           "biogas",
           "biomassa",
           "etanol",
           "bioetanol",
           "cana-de-acucar",
           "bagaco",
           "palha", #10
           "bioeletricidade",
           "gaseificacao",
           "pirolise", 
           "residuo",
           "glicerol", #15
           "enzima", "hidrolise",
           "hidrolise enzimatica",
           "hidrolise", "acida", #20
           "lignocelulose",
           "biocombustivel", "segunda","geracaoo", #24
           "biodiesel",
           "oleos vegetais", 
           "oleaginosa", 
           "gordura anima",
           "oleo de soja",
           "transesterificacao", #30
           "esterificacao",
           "algas",
           "microalga",
           "biocombustivel de terceira geracao",
           "biocombustiveis de terceira geracao", #35
           "biorreator")

iea3_5 <- c("energia geotermica",
            "energia", "recurso", "hidrotermal",
            "energia", "recurso", "rocha", "seca", "quente", 
            "rocha", "perfuracao", "exploracao")

iea3_6 <- c("hidroeletricidade",
            "hidroeletrica",
            "pch",
            "uhe",
            "barragem")

iea3_7 <- c("fontes", "energia", "renovavel")

iea4_1 <- c("fissao nuclear",
            "uranio",
            "enriquecimento",
            "combustivel nuclear", #4
            "reator", "nuclear",
            "usina nuclear",
            "pwr",
            "bwr",
            "nuclear","residuo", #11
            "radioisotopos",
            "regenardor nuclear")

iea4_2 <- c("fusão","nuclear", "fusão nuclear")

iea4_9 <- c("energia","nuclear", "energia nuclear")

iea5_1 <- c("hidrogenio",
            "producao", "armazenamento",
            "transporte","distribuicao",
            "infra-estrutura", "sistema")

iea5_2 <- c("celula a combustivel",
            "celula a combustao",
            "aplicao", "estacionaria", "movel")

iea6_1 <- c("geração de energia eletrica",
            "tecnologia de geracao de energia eletrica",
            "gerador de energia eletrica",
            "alternador",
            "cogeracao",
            "caldeira","energia eletrica")

iea6_2 <- c("transmissao de eletricidade",
            "condutor",
            "supercondutor",
            "conversor ac/dc",
            "distribuicao de eletricidade",
            "rede de energia eletrica",
            "rede inteligente", 
            "carga eletrica", # 8
            "sistema","controle", "integracao",
            "transformador", "alta tensao")

iea6_3 <- c("armazenamento", "energia", "eletrico", "termica",
            "bateria", "veiculo")

iea7_1 <- c("energ", "sistema", "modelag","planejamento",
                 "algoritmo","energia","eletricidade")

iea7_2 <- c("pesquisa","basica", "energia",
            "pesquisa energetica")


#### constrindo a variável #####


anel_pd<-anel_pd%>%mutate(iea1_1 = str_detect(motor, iea1_1),
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
#merge
aneel_time <- read_csv2("data/SGPED_BI/5.PD RF EQUIPE.csv")

aneel_time <- aneel_time %>% filter(`Tipo de Entidade`=="Proponente")  %>%
               select(CodProj,`Entidade Vinculada`,`Unidade Federativa`) %>%
               distinct()%>%
               clean_names()
anel_pd <- left_join(anel_pd,aneel_time, by="cod_proj")

#load data


  
termos_IEA<-list(iea1_1 = iea1_1,
           iea1_2 = iea1_2,
           iea1_3 = iea1_3,
           iea1_4 = iea1_4,
           iea2_1 = iea2_1,
           iea2_2 = iea2_2,
           iea2_3 = iea2_3,
           iea3_1 = iea3_1,
           iea3_2 = iea3_2,
           iea3_3 = iea3_3,
           iea3_4 = iea3_4,
           iea3_5 = iea3_5,
           iea3_6 = iea3_6,
           iea3_7 = iea3_7,
           iea4_1 = iea4_1,
           iea4_2 = iea4_2,
           iea5_1 = iea5_1,
           iea5_2 = iea5_2,
           iea6_1 = iea6_1,
           iea6_2 = iea6_2,
           iea6_3 = iea6_3,
           iea7_1 = iea7_1,
           iea7_2 = iea7_2)

write(termos_IEA, "dicionario_IEA.csv")

capture.output(termos_IEA, file = "My New File.txt")

