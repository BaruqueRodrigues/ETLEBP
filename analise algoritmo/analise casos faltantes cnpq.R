# Análise dos dados do CNPQ
# Dentro do SQLite temos 5917 projetos identificados como oriundos do CNPq
# Dentro dos projetos de energia no SQLite da fonte CNPq existem 3995 projetos
# em que o atributo categoria IEA de digito 2 de está como NA.
# Encontramos a descontinuidade de casos na base de 2019 para as bases de 2021,
# Todavia apenas 1 projeto de energia não fora identificado na base de 2021.

library(tidyverse)
library(ETLEBP)

cnpq <- cria_base_intermediaria_cnpq()

sqlite <- here::here("C:/Users/quemu/Desktop/EIP_20210415.db")

cnpq <- executa_tratamento_completo(cnpq, sqlite)

cnpq <- executa_tratamento_incremental(cnpq, sqlite)

# Nenhuma categoria encontrada para 97% dos casos. Universo de 490513 casos.

cnpq$categorias %>% janitor::tabyl()

# Aqui não conseguiremos verificar a acurácia do acerto já que existem projetos
# de energia no SQLite que não tem digito 2 da categoria de IEA identificado.

cnpq_acerto <- cnpq %>% filter(existe == "sim") %>% rowwise() %>%
  mutate(categorias_split = str_split(categorias, pattern = ",")) %>%
  ungroup() %>% unnest(categorias_split_un =categorias_split) %>%
  mutate(categorias_split_un = str_remove_all(categorias_split_un, '\\"')) %>%
  select(id,categoria_sqlite,  categorias_split_un)

cnpq_acerto_res <- cnpq_acerto %>%
  mutate(correto = categorias_split_un == categoria_sqlite ) %>% group_by(id) %>%
  summarise(correto = mean(correto))

mean(cnpq_acerto_res$correto)

cnpq_ebp1 <- readr::read_delim("analise algoritmo/bases 1 versao ebp/8.energia_cnpq_2010_2019.csv",
                        delim = ";", escape_double = FALSE, trim_ws = TRUE) %>%
  janitor::clean_names() %>%
  rename_with(
    .fn = ~str_glue("{.x}_2019")
  )

cnpq_brutos1 <- readxl::read_excel(path = here::here("data/CNPQ/Bolsas no Exterior 2004 a 2021.xlsx"),
                                 sheet = 1, skip = 6) %>%
  janitor::clean_names() %>%
  rename_with(
    .fn = ~str_glue("{.x}_2021")
  )

cnpq_brutos2 <- readxl::read_excel(here::here("data/CNPQ/Bolsas Pais - 2004 a 2021.xlsx"), skip = 6) %>%
  janitor::clean_names() %>% rename_with(
    .fn = ~str_glue("{.x}_2021")
  ) %>% mutate(palavra_chave_2021 = as.character(palavra_chave_2021))


cnpq_brutos3 <- readxl::read_excel(here::here("data/CNPQ/Bolsas Pais - 2004 a 2021.xlsx"),
                          sheet = 2,
                          skip = 6) %>%
  janitor::clean_names() %>% rename_with(
    .fn = ~str_glue("{.x}_2021")
  )

cnpq_brutos4 <-readxl::read_excel(here::here("data/CNPQ/Fomento 2010-2021.xlsx")) %>%
  janitor::clean_names()  %>%
  dplyr::select(-x35,-x36) %>%
  rename_with(
    .fn = ~str_glue("{.x}_2021")
  )
  # dplyr::filter(termino_processo >= "2013-01-01",
  #               !area %in% termos_a,
  #               !subarea %in% termos_sa)

cnpq_brutos <- bind_rows(cnpq_brutos1, cnpq_brutos2,
                         cnpq_brutos3, cnpq_brutos4)
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
  filter(id_formnt == 9)

# Dentro dos projetos de energia no SQLite da fonte CNPq existem 3995 projetos em que o atributo
# categoria IEA de digito 2 de está como NA

## Quantos projetos estavam na base de 2019 e não estão presentes na base de 2021?
# Existem 1792 projetos que só existem na base utilizada em 2019.
# Todavia não sabemos se dos projetos não encontrados eles eram projetos de energia.
projetos_inexistentes_cnpq <- cnpq_ebp1 %>% tidylog::anti_join(
  cnpq_brutos2,
  by = c("processo_2019" = "processo_2021")
)

# Verificando quantos projetos faltantes eram projetos de energia

cnpq_energia_sqlite <- tidylog::semi_join(consulta, cnpq_ebp1,
                                           by = c("id_item" = "processo_2019"  ))%>% unique()

# Apenas 1 projeto não fora identificado.
projetos_inexistentes_cnpq_sel <- cnpq_energia_sqlite %>%
                                  tidylog::anti_join(cnpq_brutos,
                                           by = c("id_item" = "processo_2021")) %>%
                                  unique()


