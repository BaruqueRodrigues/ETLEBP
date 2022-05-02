# Análise casos fapesp.
# Dentro do SQLite temos 15 projetos identificados como oriundos da FAPESP
# Cada projeto tem valor de financiamento, e categoria de IEA identificada
# Todavia para os atributos id_item, título de id_form temos o mesmo string de identificação
# Foi identificao posteriormente que os projetos no sqlite corresponde a soma do(s) valor(es)
# dos projeto(s) de energia inseridos no SQLite

library(ETLEBP)
library(tidyverse)

fapesp <- cria_base_intermediaria_fapesp()

sqlite <- here::here("C:/Users/quemu/Desktop/EIP_20210415.db")

# Essa etapa não retornará caso algum, já que os atributos de identificação
# tiveram seus valores alterados para FAPESP e PROJETO AGREGADO FAPESP.

fapesp <- executa_tratamento_completo(fapesp, sqlite)

fapesp <- executa_tratamento_incremental(fapesp, sqlite)

fapesp_acerto <- fapesp %>% filter(existe == "sim") %>% rowwise() %>%
  mutate(categorias_split = str_split(categorias, pattern = ",")) %>%
  ungroup() %>% unnest(categorias_split_un =categorias_split) %>%
  mutate(categorias_split_un = str_remove_all(categorias_split_un, '\\"')) %>%
  select(id,categoria_sqlite,  categorias_split_un)

fapesp_acerto_res <- fapesp_acerto %>%
  mutate(correto = categorias_split_un == categoria_sqlite ) %>% group_by(id) %>%
  summarise(correto = mean(correto))

mean(fapesp_acerto_res$correto)

fapesp$categorias %>% janitor::tabyl()



# criando um objeto com informações sobre as categorias de IEA

consulta <- dplyr::select(tbl_ft_dispendio, id_item, id_cat2, id_formnt)

# puxando as descrições das categorias
consulta <- dplyr::left_join(consulta, tbl_dm_categoria[,c("id","cat2")],
                             by = c("id_cat2" = "id"))
# trazendo as informações de fomentador
consulta <- dplyr::left_join(consulta, tbl_dm_formentador[,c("id_formentador","nme_form")],
                             by = c("id_formnt"= "id_formentador"))

# trazendo as informações de titulo
consulta <- dplyr::left_join(consulta, tbl_dm_projeto[,c("título", "id_item")]) %>%
  unique()

consulta <- consulta %>% mutate(título = stringr::str_trim(título),
                                título = abjutils::rm_accent(título)) %>%
  filter(id_formnt == 10)


# Análise das diferenças entre as bases brutas utilizadas em 2019 e 2021.

# Para o dataset utilizado para fazer a carga temos que filtrar algumas infos
# seguindo a orientação do documento PROTOCOLO FAPESP 2019
# Temos o problema que a lista de projetos excluidos por área não foi documentada.
# Temos o problema que há uma referencia a remoção de projetos referentes a reuniões
# e comunicações, todavia o procedimento fora feito manualmente, verificado linha a linha

# Importando dataset utilizado para fazer a carga em 2019
fapesp_ebp1 <- readr::read_delim(here::here("analise algoritmo/bases 1 versao ebp/7.FAPESP_energia.csv"),
                          delim = ";", escape_double = FALSE, trim_ws = TRUE) %>%
                janitor::clean_names() %>%
  rename_with(
    .fn = ~str_glue("{.x}_2019")
  )


# Importando dataset utilizado para fazer a carga em 2021
fapesp_brutos <- readxl::read_excel(here::here("data/FAPESP/PROJETOS FAPESP SELECIONADOS INOVA-E - VALORES - 13 dez 2021.xlsx")) %>%
  janitor::clean_names() %>%
  rename_with(
    .fn = ~str_glue("{.x}_2021")
  )

fapesp_brutos$area_do_conhecimento_2021 %>% janitor::tabyl() %>% tibble() %>% View()

# Quantos projetos estavam na base de 2019 e não estão presentes na base de 2021?
# Existem 3093 projetos que só existem na base utilizada em 2019.

projetos_inexistentes_fapesp <- fapesp_ebp1 %>% tidylog::anti_join(
  fapesp_brutos,
  by = c("n_processo_2019" = "n_processo_2021")
)

projetos_inexistentes_fapesp <- projetos_inexistentes_fapesp %>%
  mutate(data_de_inicio_2019 = str_replace_all(data_de_inicio_2019, "\\/", "-"),
         data_de_inicio_2019 = lubridate::dmy(data_de_inicio_2019),
         data_de_termino_2019 = str_replace_all(data_de_termino_2019, "\\/", "-"),
         data_de_termino_2019 = lubridate::dmy(data_de_termino_2019)
  )

projetos_inexistentes_fapesp$area_do_conhecimento_2019 %>% janitor::tabyl()

# Análise: dos projetos que estão na base bruta 2021 e não estão na base bruta 2019,
# quantos estão classificados em alguma categoria?
fapesp_sobra_2021 <- fapesp_brutos %>% tidylog::anti_join(
  fapesp_ebp1,
  by = c("n_processo_2021" = "n_processo_2019")
)


# Classificando em categorias de iea

fapesp_sobra_2021 <- fapesp_sobra_2021 %>%
  mutate(motor = stringi::stri_trans_general(paste(titulo_portugues_2021),
                                                                 "Latin-ASCII"),
         motor = tolower(motor),
         n_processo_2021 = paste("FAPESP", n_processo_2021, sep = "-"),
          data_inicio_2021 = lubridate::ymd(data_de_inicio_2021),
          data_termino_2021 = lubridate::ymd(data_de_termino_2021),
          duracao_dias_2021 = lubridate::time_length(data_termino_2021- data_inicio_2021, "days")
         ) %>%
  dplyr::filter(!area_do_conhecimento_2021 %in% termos_a,
                !subarea_do_conhecimento_2021 %in% termos_sa) %>%
  tidyr::drop_na(valor_desembolsado_2021) %>%
  func_a(n_processo_2021, data_inicio_2021,
         data_termino_2021,valor_desembolsado_2021) %>%
  dtc_categorias(n_processo_2021, motor)

fapesp_sobra_2021 <- fapesp_sobra_2021 %>% dplyr::mutate(
  id                          = n_processo_2021,
  fonte_de_dados              = "FAPESP",
  data_assinatura             = data_inicio_2021,
  data_limite                 = data_termino_2021,
  duracao_dias                = duracao_dias_2021,
  valor_contratado            = valor_desembolsado_2021,
  valor_executado_2013_2020   = gasto_2013_2020,
  nome_agente_financiador     = "FAPESP",
  natureza_agente_financiador = "Empresa Pública", # confirmar
  natureza_financiamento    = "Público",
  modalidade_financiamento    = NA,
  nome_agente_executor        = beneficiario_2021,
  natureza_agente_executor    = "Instituição Pública", # confirmar
  'p&d_ou_demonstracao'       = NA,
  titulo_projeto              = titulo_portugues_2021,
  status_projeto              = NA,
  uf_ag_executor              = "SP",
  regiao_ag_executor          = "SUDESTE",
  valor_executado_2013        = gasto_2013,
  valor_executado_2014        = gasto_2014,
  valor_executado_2015        = gasto_2015,
  valor_executado_2016        = gasto_2016,
  valor_executado_2017        = gasto_2017,
  valor_executado_2018        = gasto_2018,
  valor_executado_2019        = gasto_2019,
  valor_executado_2020        = gasto_2020) %>%

  dplyr::select(id,
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
                natureza_financiamento,
                `p&d_ou_demonstracao`,
                valor_executado_2013,valor_executado_2014,
                valor_executado_2015,valor_executado_2016,
                valor_executado_2017,valor_executado_2018,
                valor_executado_2019,valor_executado_2020,
                motor,
                categorias) %>%
  tibble()


# 80% dos projetos não foram identificados em uma categoria de IEA
# Observamos o comportamento de cerca de 60% de falsos negativos em outras bases

fapesp_sobra_2021$categorias %>% janitor::tabyl()

fapesp_sobra_2021 <- executa_tratamento_completo(fapesp_sobra_2021, sqlite)

fapesp_sobra_2021 <- executa_tratamento_incremental(fapesp_sobra_2021, sqlite)
writexl::write_xlsx(fapesp_sobra_2021, "fapesp_intermediaria_novos.xlsx")
#

fapesp_analise <- tidylog::full_join(fapesp_brutos, fapesp_ebp1,
                   by = c("n_processo_2021"= "n_processo_2019"),
                   suffix = c("_2021", "_2019"))

# Não conseguiremos recuperar nenhuma categoria de IEA devido a forma como os dados
# foram inseridos no SQLite censurando as informações de título do projeto e
# código único do projeto.

tibble(consulta)

fapesp_analise <- fapesp_analise %>%
  mutate(motor = stringi::stri_trans_general(paste(titulo_portugues_2021),
                                             "Latin-ASCII"),
         motor = tolower(motor))

fapesp_analise <- fapesp_analise %>% dtc_categorias(n_processo_2021, motor)

#

 supostos_projetos<- data.frame(projetos= c("14/50767-9",
  "16/50339-2",
  "14/50279-4",
  "15/01038-7",
  "16/20237-3",
  "16/12397-0",
  "18/16803-9",
  "14/27317-7",
  "17/08293-8",
  "17/17449-1",
  "17/17438-0",
  "08/57861-0",
  "17/11631-2",
  "15/50475-0"),
  titulo_eng = c(
    "Enhancing the performance of direct ethanol solid oxide fuel cells",
    "Chemical fabrication of active nanomaterials for fuel cells on natural gas and bioethanol fluels",
    "Brasil Research Centre for Gas Innovation",
    "High efficiency cogeneration and trigeneration software development and high efficiency cogeneration / trigeneration case studies",
    "Modeling and numerical simulation of cyclone separators",
    "Flow field optimization of fuel cells by using numerical methods",
    "Multiscale molecular modeling of nanostructures for natural gas separation processes",
    "Study on nanostructured catalysts for carboxylation of methane with CO2 to produce high value-added products",
    "",
    "Isolation of microorganisms (algae and bacteria) tolerant to high concentrations of CO2 and CH4",
    "Mitigation of CO2 and CH4 using microbial consortium (algae and bacteria)",
    "National Institute for the Development of Advanced Geochemical Analytical Procedures Applied to Oil and Gas Exploration",
    "Computational material science and chemistry",
    ""

  ))

 supostos_projetos <- supostos_projetos %>%
   tidylog::left_join(fapesp_ebp1,
                      by = c("projetos" = "n_processo_2019")) %>% tibble()

 supostos_projetos <- supostos_projetos %>%
   tidylog::left_join(fapesp_ebp1,
                      by = c("titulo_eng" = "titulo_ingles_2019")) %>% tibble()
x
