    #### MOTOR ####
### construindo vetores com IEAs para contruir a variável ####
iea1_1 <- c("motor",
            "(maquina.*eletrica)",
            "equipamento",
            "(sistema.*eletrico)",
            "aparelho", # 5
            "etiquetagem",
            "metrologia",
            "(processo.*industr)",
            "(eficiencia.*energetica)"
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

iea1_3 <- c("(bateria.*transporte)",  "veiculo",
            "veiculo eletrico.",
            "motor", "gasolina", "diesel",
            "(etanol.*eficiente)" # 9
)

iea1_4 <- c("(eficiencia.*energetica)")

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
            "combustao", "petroleo",
            "gas natural", "derivado de petroleo", #27
            "turbina a gas",
            "turbina a vapor",
            "turbina de avi.o",
            "gerador a diesel",
            "dutos",
            "oleodutos",
            "tanques",
            "gasodutos",
            "gnl", #36
            "(armazenamento.*petroleo)",
            "gas natural", "derivado de petroleo")

iea2_2 <- c("carvao",
            "carvao mineral",
            "(combustao.*carvao)",
            "(conversao.*carvao)")

iea2_3 <- c("(captura.*CO2)", "(separacao.*CO2)",
            "(escoamento.*CO2)",
            "(armazenamento.*CO2)")

iea3_1 <- c("(energia.*solar)",
            "(aquecimento.*solar)",
            "(arrefecimento.*solar)",
            "termica",
            "(concentrador.*solar)", #6
            "(sistema.*fotovoltaica)",
            "celula solar",
            "modulo fotovoltaico",
            "sistema fotovoltaico",
            "heliotermica",
            "(solar.*termica)",
            "aplicao", "alta" , "temperatura")

iea3_2 <- c("(energia.*eolica)",
            "(eolica.*onshore)", "(eolica.*offshore)",
            "(turbina.*eolica)",
            "usinade vento",
            "rotor",
            "aerogerador",
            "conversor",
            "pas.*eolica")

iea3_3 <- c("energia.*oceanica",
            "(energia.*mare)",
            "(energia.*ondas)",
            "mar.", "ondas",
            "(poder.*gradiente.*salinidade)")

iea3_4 <-c("(biocombustivel.*liquido)",
           "(biocombustivel.*solido)",
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
           "(enzima.*hidrolise)",
           "hidrolise enzimatica",
           "(hidrolise.*acida)", #20
           "lignocelulose",
           "(biocombustivel.*segunda.*geracao)", #24
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
            "(energia.*recurso.*hidrotermal)",
            "(energia.*recurso.*rocha.*seca.*quente)",
            "(rocha.*perfuracao)", "(rocha.*exploracao)")

iea3_6 <- c("hidroeletricidade",
            "hidroeletrica",
            "pch",
            "uhe",
            "barragem")

iea3_7 <- c("(fontes.*energia.*renovavel)")

iea4_1 <- c("fissao nuclear",
            "uranio",
            "enriquecimento",
            "combustivel nuclear", #4
            "(reator.*nuclear)",
            "(reatores.*nuclear)",
            "usina nuclear",
            "pwr",
            "bwr",
            "(residuo.*nuclear)", #11
            "(radioisotopos.*nuclear)",
            "regenardor nuclear")

iea4_2 <- c("(fusão.*nuclear)", "fusão nuclear")

iea4_9 <- c("(energia.*nuclear)", "energia nuclear")

iea5_1 <- c("hidrogenio",
            "(producao.*hidrogenio)", "(armazenamento.*hidrogenio)",
            "(transporte.*hidrogenio)","(distribuicao.*hidrogenio)",
            "(infra-estrutura.*hidrogenio)", "(sistema.*hidrogenio)")

iea5_2 <- c("celula a combustivel",
            "celula a combustao",
            "(aplicao.*estacionaria)", "movel")

iea6_1 <- c("geração de energia eletrica",
            "tecnologia de geracao de energia eletrica",
            "gerador de energia eletrica",
            "alternador",
            "cogeracao",
            "(caldeira.*energia eletrica)")

iea6_2 <- c("transmissao de eletricidade",
            "condutor",
            "supercondutor",
            "conversor ac/dc",
            "distribuicao de eletricidade",
            "rede de energia eletrica",
            "rede inteligente",
            "carga eletrica", # 8
            "(sistema.*controle)", "integracao",
            "transformador", "alta tensao")

iea6_3 <- c("(armazenamento.*energia)", "(armazenamento.*eletrico)",
            "(armazenamento.*termica)",
            "bateria", "veiculo")

iea7_1 <- c("(sistema.*energ)", "(modelag.*planejamento)",
            "(algoritmo.*nergia)","eletricidade")

iea7_2 <- c("(pesquisa.*basica.*energia)",
            "pesquisa energetica")


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




usethis::use_data(iea1_1, overwrite = TRUE)
usethis::use_data(iea1_2, overwrite = TRUE)
usethis::use_data(iea1_3, overwrite = TRUE)
usethis::use_data(iea1_4, overwrite = TRUE)
usethis::use_data(iea2_1, overwrite = TRUE)
usethis::use_data(iea2_2, overwrite = TRUE)
usethis::use_data(iea2_3, overwrite = TRUE)
usethis::use_data(iea3_1, overwrite = TRUE)
usethis::use_data(iea3_2, overwrite = TRUE)
usethis::use_data(iea3_3, overwrite = TRUE)
usethis::use_data(iea3_4, overwrite = TRUE)
usethis::use_data(iea3_5, overwrite = TRUE)
usethis::use_data(iea3_6, overwrite = TRUE)
usethis::use_data(iea3_7, overwrite = TRUE)
usethis::use_data(iea4_1, overwrite = TRUE)
usethis::use_data(iea4_2, overwrite = TRUE)
usethis::use_data(iea5_1, overwrite = TRUE)
usethis::use_data(iea5_2, overwrite = TRUE)
usethis::use_data(iea6_1, overwrite = TRUE)
usethis::use_data(iea6_2, overwrite = TRUE)
usethis::use_data(iea6_3, overwrite = TRUE)
usethis::use_data(iea7_1, overwrite = TRUE)
usethis::use_data(iea7_2, overwrite = TRUE)



