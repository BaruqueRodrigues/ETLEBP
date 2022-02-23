#' Cria Base Intermediária CNPQ
#'
#' @param origem_processos1
#' @param origem_processos2
#'
#' @return
#' @export
#'
#' @examples
cria_base_intermediaria_cnpq<- function(origem_processos1 = here::here("data/CNPQ/Bolsas no Exterior 2004 a 2021.xlsx"),
                                        origem_processos2 = here::here("data/CNPQ/Bolsas Pais - 2004 a 2021.xlsx"),
                                        origem_processos3 = here::here("data/CNPQ/Fomento 2010-2021.xlsx")){




#-------------------------- termos filtrados --------------------------
  termos_a<- c("Botânica","Ciência e Tecnologia de Alimentos",
               "Direito", "Ecologia", "Educação Física", "Enfermagem",
               "Farmacologia", "Fisiologia", "Fisioterapia", "Fonoaudiologia",
               "Genética", "Imunologia", "Medicina", "Medicina Veterinária",
               "Museologia", "Oceoanografia", "Parasitologia", "Saude Coletiva",
               "Serviço Social", "Turismo", "Zoologia", "Zootecnia", "Letras",
               "Lingüística", "Artes", "Educação", "Filosofia", "Geografia","História",
               "Psicologia", "Sociologia", "Teologia", "Administração", "Comunicação",
               "Desenho Industrial", "Ciência da Informação", "Demografia",
               "Economia Doméstica","Recursos Pesqueiros e Engenharia de Pesca",
               "Morfologia", "Fisiologia", "Farmacologia")


  termos_sa<- c("Administração de Empresas","Administração de Setores Específicos",
                "Administração Pública", "Análise Nutricional de População",
                "Arquivologia","Biblioteconomia","Bioquímica da Nutrição",
                "Ciência do Solo", "Ciências Contábeis", "Comunicação Visual",
                "Conservação da Natureza", "Desnutrição e Desenvolvimento Fisiológico",
                "Dietética", "Engenharia Médica", "Enzimologia", "Estruturas",
                "Estruturas Aeroespaciais", "Estruturas Navais", "Fitossanidade",
                "Fitotecnia", "Floricultura", "Geodésia", "Geometria e Topologia",
                "Lavra", "Manejo Florestal", "Metabolismo e Bioenergética",
                "Meteorologia", "Métodos e Técnicas do Planejamento Urbano e Regional",
                "Microbiologia Aplicada", "Paisagismo", "Projeto de Arquitetura e Urbanismo",
                "Relações Públicas e Propaganda","Saneamento Ambiental", "Saneamento Básico",
                "Técnicas e Operações Florestais")

#---------- Inicio do Tratamento ---------------------------------------------


  cnpq1<-readxl::read_excel(path = origem_processos1,
                            sheet = 1, skip = 6) %>%
    janitor::clean_names()

  cnpq2<-readxl::read_excel(origem_processos2, skip = 6) %>%
    janitor::clean_names()

  cnpq3<-readxl::read_excel(origem_processos2,
                            sheet = 2,
                            skip = 6) %>%
    janitor::clean_names()



  cnpq1 <- cnpq1 %>%
  dplyr::select(-x16, -x17, -x18) %>%
  dplyr::filter(!area %in% termos_a,
                !subarea %in% termos_sa)  %>%
  dplyr::mutate(
    inicio_processo = lubridate::as_date(inicio_processo),
    termino_processo = lubridate::as_date(termino_processo),
    prazo_dias = lubridate::time_length(termino_processo - inicio_processo, "days"),
    motor = stringi::stri_trans_general(paste(titulo_do_projeto, area, subarea),
                                        "Latin-ASCII"),
    categoria_nivel = NA,
    sigla_uf_destino = NA,
    regiao_destino = NA) %>%
  dplyr::relocate(categoria_nivel, .before = grande_area) %>%
  dplyr::relocate(sigla_uf_destino, regiao_destino, .before = pais_destino)


  cnpq2 <- cnpq2 %>%
  dplyr::filter(!area %in% termos_a,
                !subarea %in% termos_sa) %>%
  dplyr::mutate(
    inicio_processo = NA,
    termino_processo = NA,
    prazo_dias = lubridate::time_length(termino_processo - inicio_processo, "days"),
    palavra_chave =as.character(palavra_chave),
    motor = stringi::stri_trans_general(paste(titulo_do_projeto, area, subarea),
                                        "Latin-ASCII")
  ) %>%
  dplyr::relocate(inicio_processo, termino_processo, .before = modalidade)


  cnpq3 <- cnpq3 %>%
  dplyr::select(-x17, -x18) %>%
  dplyr::filter(!area %in% termos_a,
                !subarea %in% termos_sa) %>%
  dplyr::mutate(
    inicio_processo = NA,
    termino_processo = NA,
    prazo_dias = lubridate::time_length(termino_processo - inicio_processo, "days"),
    motor = stringi::stri_trans_general(paste(titulo_do_projeto, area, subarea),
                                        "Latin-ASCII")
  ) %>%
  dplyr::relocate(inicio_processo, termino_processo, .before = modalidade)


  cnpq1 <- cnpq1 %>% filter(ano_referencia >=2013)

  cnpq2 <- cnpq2 %>% filter(ano_referencia >=2013)

  obj<-dplyr::bind_rows(cnpq1,cnpq2,
                         cnpq3)

  rm(list = c("cnpq1","cnpq2","cnpq3"))



  obj <- obj %>% group_split(ano_referencia)


  obj10 <- obj[1] %>% tibble() %>%
    tidyr::unnest()

  obj10 <- obj10 %>%
    dplyr::mutate(
      inicio_processo = dplyr::case_when(
        is.na(inicio_processo) ~ lubridate::make_date(2013, 1, 1),
        TRUE ~ inicio_processo
      ),
      termino_processo = dplyr::case_when(is.na(termino_processo) ~ lubridate::make_date(2013, 12, 31)),
      prazo_dias = lubridate::time_length(termino_processo - inicio_processo, "days"),
      processo2  = paste(processo, 1:nrow(obj10))
    ) %>%
    func_a(processo2, inicio_processo,
           termino_processo, valor_pago) %>% dtc_categorias(processo2, motor) %>%
    dplyr::mutate(categorias = dplyr::recode(categorias,
                                             "character(0" = "nenhuma categoria encontrada"))

  obj11 <- obj[2] %>% tibble() %>%
    tidyr::unnest() %>%
    dplyr::mutate(
      inicio_processo = dplyr::case_when(
        is.na(inicio_processo) ~ lubridate::make_date(2014, 1, 1),
        TRUE ~ inicio_processo
      ),
      termino_processo = dplyr::case_when(
        is.na(termino_processo) ~ lubridate::make_date(2014, 12, 31),
        TRUE ~ termino_processo
      ),
      prazo_dias = lubridate::time_length(termino_processo - inicio_processo, "days")
    ) %>%
    func_a(processo, inicio_processo,
           termino_processo, valor_pago) %>% dtc_categorias(processo, motor) %>%
    dplyr::mutate(categorias = dplyr::recode(categorias,
                                             "character(0" = "nenhuma categoria encontrada"))
  obj12 <- obj[3] %>% tibble() %>%
    tidyr::unnest() %>%
    dplyr::mutate(
      inicio_processo = dplyr::case_when(
        is.na(inicio_processo) ~ lubridate::make_date(2015, 1, 1),
        TRUE ~ inicio_processo
      ),
      termino_processo = dplyr::case_when(
        is.na(termino_processo) ~ lubridate::make_date(2015, 12, 31),
        TRUE ~ termino_processo
      ),
      prazo_dias = lubridate::time_length(termino_processo - inicio_processo, "days")
    ) %>%
    func_a(processo, inicio_processo,
           termino_processo, valor_pago) %>% dtc_categorias(processo, motor) %>%
    dplyr::mutate(categorias = dplyr::recode(categorias,
                                             "character(0" = "nenhuma categoria encontrada"))

  obj13 <- obj[4] %>% tibble() %>%
    tidyr::unnest() %>%
    dplyr::mutate(
      inicio_processo = dplyr::case_when(
        is.na(inicio_processo) ~ lubridate::make_date(2016, 1, 1),
        TRUE ~ inicio_processo
      ),
      termino_processo = dplyr::case_when(
        is.na(termino_processo) ~ lubridate::make_date(2016, 12, 31),
        TRUE ~ termino_processo
      ),
      prazo_dias = lubridate::time_length(termino_processo - inicio_processo, "days")
    ) %>%
    func_a(processo, inicio_processo,
           termino_processo, valor_pago) %>% dtc_categorias(processo, motor) %>%
    dplyr::mutate(categorias = dplyr::recode(categorias,
                                             "character(0" = "nenhuma categoria encontrada"))

  obj14 <- obj[5] %>% tibble() %>%
    tidyr::unnest() %>%
    dplyr::mutate(
      inicio_processo = dplyr::case_when(
        is.na(inicio_processo) ~ lubridate::make_date(2017, 1, 1),
        TRUE ~ inicio_processo
      ),
      termino_processo = dplyr::case_when(
        is.na(termino_processo) ~ lubridate::make_date(2017, 12, 31),
        TRUE ~ termino_processo
      ),
      prazo_dias = lubridate::time_length(termino_processo - inicio_processo, "days")
    ) %>%
    func_a(processo, inicio_processo,
           termino_processo, valor_pago) %>% dtc_categorias(processo, motor) %>%
    dplyr::mutate(categorias = dplyr::recode(categorias,
                                             "character(0" = "nenhuma categoria encontrada"))

  obj15 <- obj[6] %>% tibble() %>%
    tidyr::unnest() %>%
    dplyr::mutate(
      inicio_processo = dplyr::case_when(
        is.na(inicio_processo) ~ lubridate::make_date(2018, 1, 1),
        TRUE ~ inicio_processo
      ),
      termino_processo = dplyr::case_when(
        is.na(termino_processo) ~ lubridate::make_date(2018, 12, 31),
        TRUE ~ termino_processo
      ),
      prazo_dias = lubridate::time_length(termino_processo - inicio_processo, "days")
    ) %>%
    func_a(processo, inicio_processo,
           termino_processo, valor_pago) %>% dtc_categorias(processo, motor) %>%
    dplyr::mutate(categorias = dplyr::recode(categorias,
                                             "character(0" = "nenhuma categoria encontrada"))

  obj16 <- obj[7] %>% tibble() %>%
    tidyr::unnest()
  obj16 <- obj16 %>%
    dplyr::mutate(
      inicio_processo = dplyr::case_when(
        is.na(inicio_processo) ~ lubridate::make_date(2019, 1, 1),
        TRUE ~ inicio_processo
      ),
      termino_processo = dplyr::case_when(
        is.na(termino_processo) ~ lubridate::make_date(2019, 12, 31),
        TRUE ~ termino_processo
      ),
      prazo_dias = lubridate::time_length(termino_processo - inicio_processo, "days"),
      processo2  = paste(processo, 1:nrow(obj16))
    ) %>%
    func_a(processo2, inicio_processo,
           termino_processo, valor_pago) %>% dtc_categorias(processo2, motor) %>%
    dplyr::mutate(categorias = dplyr::recode(categorias,
                                             "character(0" = "nenhuma categoria encontrada"))
  obj17 <- obj[8] %>% tibble() %>%
    tidyr::unnest() %>%
    dplyr::mutate(
      inicio_processo = dplyr::case_when(
        is.na(inicio_processo) ~ lubridate::make_date(2020, 1, 1),
        TRUE ~ inicio_processo
      ),
      termino_processo = dplyr::case_when(
        is.na(termino_processo) ~ lubridate::make_date(2020, 12, 31),
        TRUE ~ termino_processo
      ),
      prazo_dias = lubridate::time_length(termino_processo - inicio_processo, "days")
    ) %>%
    func_a(processo, inicio_processo,
           termino_processo, valor_pago) %>% dtc_categorias(processo, motor) %>%
    dplyr::mutate(categorias = dplyr::recode(categorias,
                                             "character(0" = "nenhuma categoria encontrada"))

  obj18 <- obj[9] %>% tibble() %>%
    tidyr::unnest() %>%
    dplyr::mutate(
      inicio_processo = dplyr::case_when(
        is.na(inicio_processo) ~ lubridate::make_date(2021, 1, 1),
        TRUE ~ inicio_processo
      ),
      termino_processo = dplyr::case_when(
        is.na(termino_processo) ~ lubridate::make_date(2021, 12, 31),
        TRUE ~ termino_processo
      ),
      prazo_dias = lubridate::time_length(termino_processo - inicio_processo, "days")
    ) %>%
    func_a(processo, inicio_processo,
           termino_processo, valor_pago) %>% dtc_categorias(processo, motor) %>%
    dplyr::mutate(categorias = dplyr::recode(categorias,
                                             "character(0" = "nenhuma categoria encontrada"))

  cnpq_categorizado<- bind_rows(obj10,
                                obj11,obj12,obj13,obj14,
                                obj15,obj16,obj17)

  rm(list = c( "obj10",
               "obj11","obj12","obj13","obj14",
               "obj15","obj16","obj17"))

  cnpq_categorizado <- cnpq_categorizado %>%
    dplyr::mutate(
      regiao_ag_executor = dplyr::recode(
        sigla_uf_destino,
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
      )
    )

  cnpq_categorizado <- cnpq_categorizado %>%
    mutate(
      id                           = paste("CNPQ",
                                           processo, sep = "-"),
      fonte_de_dados                 = "CNPQ",
      data_assinatura                    = inicio_processo,
      data_limite                    = termino_processo,
      duracao_dias                   = prazo_dias,
      #duracao_meses                  = periodo_meses,
      #duracao_anos                   = periodo_anos,
      valor_contratado               = valor_pago,
      titulo_projeto                 = titulo_do_projeto,
      status_projeto                 = NA,
      nome_agente_financiador        = "CNPQ",
      natureza_agente_financiador    = "Empresa Pública",
      natureza_financiamento         = "pública",
      modalidade_financiamento       = "não reembolsável",
      nome_agente_executor           = instituicao_destino,
      natureza_agente_executor       = NA,
      #confirmar natureza juridica proponente
      'p&d_ou_demonstracao'          = "Demonstração",
      uf_ag_executor                 = sigla_uf_destino,
      valor_executado_2013_2020      = gasto_2013_2020,
      valor_executado_2013           = gasto_2013,
      valor_executado_2014           = gasto_2014,
      valor_executado_2015           = gasto_2015,
      valor_executado_2016           = gasto_2016,
      valor_executado_2017           = gasto_2017,
      valor_executado_2018           = gasto_2018,
      valor_executado_2019           = gasto_2019,
      valor_executado_2020           = gasto_2020
    )

  cnpq_categorizado <- cnpq_categorizado %>%
    dplyr::select(
      id,
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
    )

  cnpq4 <- readxl::read_excel(origem_processos3) %>%
    janitor::clean_names()  %>%
    dplyr::select(-x35,-x36) %>%
    dplyr::filter(termino_processo >= "2013-01-01",
                  !area %in% termos_a,
                  !subarea %in% termos_sa)

  cnpq4 <- cnpq4 %>%
    dplyr::mutate(
      processo2 = paste(processo, 1:nrow(cnpq4)),
      inicio_processo = lubridate::as_date(inicio_processo),
      termino_processo = lubridate::as_date(termino_processo),
      duracao_dias = time_length(termino_processo- inicio_processo,"days"),
      motor = tolower(stringi::stri_trans_general(titulo_do_projeto,
                                                  "Latin-ASCII"))
    ) %>%
    func_a(processo2,
           inicio_processo,
           termino_processo,
           valor_pago) %>%
    dtc_categorias(processo2,motor) %>%
    dplyr::mutate(categorias = dplyr::recode(categorias,
                                             "character(0" = "nenhuma categoria encontrada"))


  cnpq4 <- cnpq4%>%
    dplyr::mutate(
      regiao_ag_executor = dplyr::recode(
        sigla_uf_destino,
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
      )
    )

  cnpq4 <- cnpq4 %>%
    mutate(
      id                           = paste("CNPQ",
                                           processo, sep = "-"),
      fonte_de_dados                 = "CNPQ",
      data_assinatura                    = inicio_processo,
      data_limite                    = termino_processo,
      #duracao_dias                   = prazo_dias,
      #duracao_meses                  = periodo_meses,
      #duracao_anos                   = periodo_anos,
      valor_contratado               = valor_pago,
      titulo_projeto                 = titulo_do_projeto,
      status_projeto                 = NA,
      nome_agente_financiador        = "CNPQ",
      natureza_agente_financiador    = "Empresa Pública",
      natureza_financiamento         = "pública",
      modalidade_financiamento       = "não reembolsável",
      nome_agente_executor           = instituicao_destino,
      natureza_agente_executor       = NA,
      #confirmar natureza juridica proponente
      'p&d_ou_demonstracao'          = "Demonstração",
      uf_ag_executor                 = sigla_uf_destino,
      valor_executado_2013_2020      = gasto_2013_2020,
      valor_executado_2013           = gasto_2013,
      valor_executado_2014           = gasto_2014,
      valor_executado_2015           = gasto_2015,
      valor_executado_2016           = gasto_2016,
      valor_executado_2017           = gasto_2017,
      valor_executado_2018           = gasto_2018,
      valor_executado_2019           = gasto_2019,
      valor_executado_2020           = gasto_2020
    )

  cnpq4 <- cnpq4 %>%
    dplyr::select(
      id,
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
    )

  cnpq_categorizado <- dplyr::bind_rows(cnpq_categorizado, cnpq4)

  cnpq_categorizado
}
