library(tidyverse)
library(janitor)
library(writexl)

finep_ebp1 <- readr::read_delim("analise algoritmo/bases 1 versao ebp/2.FINEP.csv",
                                delim = ";", escape_double = FALSE, trim_ws = TRUE,
                                skip = 5) %>%
  janitor::clean_names() %>%
  rename_with(
    .fn = ~str_glue("{.x}_2019")
  )

finep_brutos <- rio::import(here::here("data/FINEP/14_09_2021_Liberacoes.ods"),
                            skip = 5,
                            sheet = 1) %>%
  janitor::clean_names() %>%
  rename_with(
    .fn = ~str_glue("{.x}_2021")
  )

# Importando as tabelas do SQlite da INOVA-E
tbl_dm_agente_empresa <- DBI::dbReadTable(con,"dm_agente_empresa")
tbl_dm_projeto <- DBI::dbReadTable(con,"dm_projeto")
tbl_dm_formentador <- DBI::dbReadTable(con,"dm_formentador")
tbl_ft_dispendio <- DBI::dbReadTable(con,"ft_dispendio")


#projetos do bndes dentro do sqlite
dm_projeto_sel <- left_join(tbl_dm_projeto, tbl_ft_dispendio[,c("id_item","id_formnt")]) %>%
  filter(id_formnt == 7) %>%
  unique()

#todos os casos de dm_projeto_sel estão contidos em finep_ebp1
finep_energia_sqlite <- tidylog::semi_join(finep_ebp1,dm_projeto_sel,
                                  by = c("contrato_2019" = "id_item"))

comparacao <- tidylog::full_join(finep_energia_sqlite, finep_brutos,
                                 by = c("contrato_2019" = "contrato_2021"),
                                 keep = TRUE)

finep_sobra <- finep_brutos %>%
  tidylog::anti_join(finep_ebp1,
                     by = c("contrato_2021" = "contrato_2019"),
                     keep = TRUE) %>% unique()
finep_sobra <- finep_sobra%>%
  dplyr::mutate(
    motor            = tolower(stringi::stri_trans_general(titulo_2021, "Latin-ASCII")),
    valor_finep      = as.numeric(valor_finep_2021),
    valor_liberado   = as.numeric(valor_liberado_2021),
    data_liberacao   = lubridate::ymd(lubridate::dmy(data_liberacao_2021)),
    data_assinatura  = lubridate::ymd(lubridate::dmy(data_assinatura_2021)),
    prazo_utilizacao = lubridate::ymd(lubridate::dmy(prazo_utilizacao_2021)),
    periodo_meses    = lubridate::time_length(prazo_utilizacao- data_assinatura, "months"),
    periodo_dias     = lubridate::time_length(prazo_utilizacao - data_assinatura, "days"),
    periodo_anos     = as.integer(lubridate::time_length(prazo_utilizacao - data_assinatura, "years") ),
    contrato2 = paste(contrato_2021, 1:nrow(finep_sobra) )
  ) %>%
  dplyr::filter(
    prazo_utilizacao >= "2013-01-01") %>%
  tidyr::drop_na(valor_finep) %>%
  func_a(processo = contrato2,
         data_inicio = data_assinatura,
         prazo_utilizacao = prazo_utilizacao,
         valor_projeto = valor_finep) %>%
  dtc_categorias(processo = contrato2, motor = motor) %>%
  dplyr::mutate(
    categorias = dplyr::recode(categorias,
                               "character(0" = "nenhuma categoria encontrada")) %>%
  dplyr::mutate(regiao_ag_executor = dplyr::recode(
    uf_2021,
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

 finep_sobra <- finep_sobra %>%
  dplyr::mutate(
    id                           = paste("FINEP",
                                         contrato_2021, sep = "-"),
    fonte_de_dados                 = "FINEP",
    data_limite                    = prazo_utilizacao,
    duracao_dias                   = periodo_dias,
    duracao_meses                  = periodo_meses,
    duracao_anos                   = periodo_anos,
    valor_contratado               = valor_finep,
    titulo_projeto                 = titulo_2021,
    status_projeto                 = status_2021,
    nome_agente_financiador        = "Finep",
    natureza_agente_financiador    = "Empresa Pública",
    natureza_financiamento         = "pública",
    modalidade_financiamento       = instrumento_2021,
    nome_agente_executor           = proponente_2021,
    natureza_agente_executor       = "Empresa Privada", #confirmar natureza juridica proponente
    'p&d_ou_demonstracao'          = "Demonstração",
    uf_ag_executor                 = uf_2021,
    valor_executado_2013_2020      = gasto_2013_2020,
    valor_executado_2013           = gasto_2013,
    valor_executado_2014           = gasto_2014,
    valor_executado_2015           = gasto_2015,
    valor_executado_2016           = gasto_2016,
    valor_executado_2017           = gasto_2017,
    valor_executado_2018           = gasto_2018,
    valor_executado_2019           = gasto_2019,
    valor_executado_2020           = gasto_2020) %>%
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
                 natureza_financiamento,
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
                 motor,
                 categorias
   ) %>% tibble()

 finep_sobra <- executa_tratamento_completo(finep_sobra, sqlite)

 finep_sobra <- executa_tratamento_incremental(finep_sobra, sqlite)
 writexl::write_xlsx(finep_sobra, "finep_intermediaria_novos.xlsx")

#Não existem projetos sem valor


