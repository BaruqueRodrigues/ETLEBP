#' Detecta categorias de IEA dentro dos projetos de energia
#'
#' @param df dataframe que contem os projetos de energia
#' @param processo atributo de id unico do projeto de energia
#' @param motor vetor com os atributos utilizados para a classificação
#' @import dplyr
#' @import stringr
#' @import purrr
#' @return
#' @export
#'
#' @examples
#' dtc_categorias(dataset, id, motor)
dtc_categorias <- function(df,processo, motor){
  detecta_categorias <- function(texto){

    iea1_1 <- c("(eficiencia.*procedimento)",
                "(eficiencia.*processo)",
                "(eficiencia.*equipamento)",
                "(eficiencia.*sistema)",
                "(eficiencia.*motor)",
                "(eficiencia.*industria)",
                "etiquetagem",
                "analise do ciclo de vida"
    )
    iea1_2<- c("conforto termico",
               "(eficiencia.*edificio)",
               "(eficiencia.*habita.)",
               "(eficiencia.*edificao)",
               "(eficiencia.*residencia.)",
               "(eficiencia.*iluminac.)",
               "(eficiencia.*refrigerac.)",
               "(eficiencia.*aquecimento)",
               "conforto ambiental",
               "(bateria.*residencia.)",
               "(bateria.*comercia.)"
    )

    iea1_3 <- c("(eficiencia.*veiculo)",
                "(eficiencia.*carro)",
                "(eficiencia.*combustivel)",
                "(eficiencia.*infra)",
                "(eficiencia.*carro eletrico)",
                "(eficiencia.*trem)",
                "(eficiencia.*aviao)",
                "(eficiencia.*navio)"
    )

    iea1_4 <- c("(eficiencia.*energetica)",
                "eficiencia energetica")

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
                "(petroleo.*dutos)",
                "(gas.*dutos)",
                "oleodutos",
                "(petroleo.*tanques)",
                "(gas*tanques)",
                "gasodutos",
                "gnl", #36
                "(armazenamento.*petroleo)",
                "(armazenamento.*gas)",
                "gas natural", "derivado de petroleo",
                "(combustao.*petroleo)",
                "(petroleo.*dutos)",
                "(gas.*dutos)",
                "(gas.*natural)",
                "(petroleo.*producao)",
                "(gas.*producao)"
    )

    iea2_2 <- c("carvao",
                "carvao mineral",
                "(combustao.*carvao)",
                "(conversao.*carvao)",
                "(extracao.*carvao)",
                "(processamento.*carvao)",
                "(trasnporte.*carvao)")

    iea2_3 <- c("(captura.*CO2)", "(separacao.*CO2)",
                "(escoamento.*CO2)",
                "(armazenamento.*CO2)")

    iea2_9 <- c("(petroleo.*gas.*natural.*carvao)")

    iea3_1 <- c("(energia.*solar)",
                "(aquecimento.*solar)",
                "(arrefecimento.*solar)",
                "termica",
                "(concentrador.*solar)", #6
                "celula solar",
                "fotovoltaic.*",
                "(celula.*solar)",
                "heliotermica",
                "(solar.*termica)",
                "(aplicao.*alta.*temperatura)")

    iea3_2 <- c("(energia.*eolica)",
                "(eolica.*onshore)", "(eolica.*offshore)",
                "fazenda.*eolica",
                "geracao.*eolica",
                "(turbina.*eolica)",
                "usina de vento",
                "aerogerador",
                "pas.*eolica")

    iea3_3 <- c("(energia.*oceanica)",
                "(energia.*mare)",
                "(energia.*ondas)",
                "mar.", "ondas",
                "(poder.*gradiente.*salinidade)",
                "aerogerador",
                "(geracao.*eolica)",
                "(fazenda.*eolica)",
                "usina de vento",
                "(turbina.*eolica)")

    iea3_4 <-c("biocombustiveis",
               "biocombustivel",
               "bioenergia",
               "biogas",
               "biomassa",
               "etanol",
               "bioetanol",
               "(cana-de-acucar.*energia)",
               "(palha.*energia)",
               "(enzima.*hidrolise)",
               "(bagaco.*energia)",
               "(palha.*energia)", #10
               "bioeletricidade",
               "gaseificacao",
               "pirolise",
               "glicerol", #15
               "hidrolise enzimatica",
               "(hidrolise.*acida)", #20
               "lignocelulose",
               "(biocombustivel.*segunda.*geracao)", #24
               "biodiesel",
               "oleos vegetais",
               "oleaginosa",
               "(gordura.*anima.*energia)",
               "oleo de soja",
               "transesterificacao", #30
               "esterificacao",
               "(algas.*energia)",
               "biocombustivel de terceira geracao",
               "biocombustiveis de terceira geracao", #35
               "biorreator")

    iea3_5 <- c("energia geotermica",
                "(energia.*recurso.*hidrotermal)",
                "(energia.*recurso.*rocha.*seca.*quente)",
                "(rocha.*perfuracao)", "(rocha.*exploracao)")

    iea3_6 <- c("hidroeletricidade",
                "hidreletricidade",
                "central.*hidroeletrica",
                "central.*hidreletrica",
                "pequena.*central.*hidreletrica",
                "pequena.*central.*hidroletrica",
                "hidroeletrica",
                "hidrogerador",
                "hidreletrica",
                "(hidro.*energia)",
                "(turbina.*hidr)",
                "pch",
                "uhe",
                "barragem")

    iea3_7 <- c("(fontes.*energia.*renovavel)",
                "(energia.*rejeito)",
                "(energia.*residuo)")

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

    iea4_2 <- c("(fusao.*energi)")

    iea4_9 <- c("(energia.*nuclear)", "energia nuclear")

    iea5_1 <- c("hidrogenio",
                "(producao.*hidrogenio)",
                "(producao.*h2)", "(armazenamento.*hidrogenio)",
                "(armazenamento.*h2)",
                "(transporte.*hidrogenio)","(transporte.*h2)",
                "(distribuicao.*hidrogenio)","(distribuicao.*h2)",
                "(infra.*hidrogenio)","(infra.*h2)",
                "(sistema.*hidrogenio)", "(sistema.*h2)",
                "(fabricacao.*h2)","(fabricacao.*hidrogenio)")

    iea5_2 <- c("celula a combustivel",
                "(aplicacao.*estacionaria)",
                "(aplicacao.*movel)",
                "(celula.*combustiv)")

    iea6_1 <- c("geração de energia eletrica",
                "tecnologia de geracao de energia eletrica",
                "gerador de energia eletrica",
                "alternador",
                "(cogeracao.*energia.*eletricidade)",
                "(co-geracao.*energia.*eletricidade)",
                "(caldeira.*energia eletrica)",
                "(energia.*eletric)")

    iea6_2 <- c("transmissao de eletricidade",
                "transmissao de energia",
                "condutor",
                "supercondutor",
                "(conversor.*ac.*dc)",
                "(condutor.*ac.*dc)",
                "distribuicao de eletricidade",
                "rede de energia eletrica",
                "(rede.*energia.*eletrica)",
                "(rede.*eletric)",
                "rede inteligente",
                "carga eletrica", # 8
                "(planejamento.*distribuicao)",
                "(planejamento.*transmissao)",
                "transformador",
                "(linha.*transmissao)",
                "(avaliacao.*distribuicao)",
                "(avaliacao.*transmissao)",
                "(inspecao.*linha)",
                "(supervisao.*transmissao)",
                "(protecao.*transmissao)",
                "(controle.*transmissao)",
                "(sistema*transmissao)",
                "microgrid",
                "(supervisao.*distribuicao)",
                "(protecao.*distribuicao)",
                "(controle.*distribuicao)",
                "(sistema*distribuicao)",
                "(sistema.*controle)",
                "(rede.*transmissao)",
                "(rede.*distribuicao)",
                "transformador",
                "(smart.*metering)",
                "(smart.*grid)")

    iea6_3 <- c("(armazenamento.*energia)", "(armazenamento.*eletrico)",
                "(armazenamento.*termica)",
                "bateria","(bateria.*armazenamento[^carro.*veiculo]*$)")

    iea7_1 <- c(
      "(sistema.*energ.*analise)",
      "(planejamento.*energia)",
      "previsao mudanca",
      "clima",
      "(gestao.*sistema*eletric)",
      "(ferramenta.*analise)",
      "energy management system",
      "(inteligencia.*energia)",
      "(inteligencia.*sistema.*eletric)",
      "(modelag.*planejamento)",
      "(modelag.*energia)",
      "(mudanca.*clima.*energia)",
      "(mudanca.*clima.*eletric)",
      "(sistema.*gerenciamento)",
      "(algoritmo.*nergia)","eletricidade")

    iea7_2 <- c("(pesquisa.*basica.*energia)")#load data


    expressoes_capitulos <- dplyr::tibble(
      categoria = c("1.1", "1.2","1.3",
                    "1.4",
                    "2.1","2.2","2.3",
                    "3.1","3.2","3.3",
                    "3.4","3.5","3.6",
                    "3.7",
                    "4.1","4.2",
                    "5.1","5.2",
                    "6.1","6.2","6.3",
                    "7.1","7.2"),
      expressoes =c(
        stringr::str_flatten(
          stringr::str_glue("\\b{iea1_1}\\b"),
          collapse = "|"
        ),

        stringr::str_flatten(
          stringr::str_glue("\\b{iea1_2}\\b"),
          collapse = "|"
        ),
        stringr::str_flatten(
          stringr::str_glue("\\b{iea1_3}\\b"),
          collapse = "|"
        ),
        stringr::str_flatten(
          stringr::str_glue("\\b{iea1_4}\\b"),
          collapse = "|"
        ),
        stringr::str_flatten(
          stringr::str_glue("\\b{iea2_1}\\b"),
          collapse = "|"
        ),
        stringr::str_flatten(
          stringr::str_glue("\\b{iea2_2}\\b"),
          collapse = "|"
        ),
        stringr::str_flatten(
          stringr::str_glue("\\b{iea2_3}\\b"),
          collapse = "|"
        ),
        stringr::str_flatten(
          stringr::str_glue("\\b{iea3_1}\\b"),
          collapse = "|"
        ),
        stringr::str_flatten(
          stringr::str_glue("\\b{iea3_2}\\b"),
          collapse = "|"
        ),
        stringr::str_flatten(
          stringr::str_glue("\\b{iea3_3}\\b"),
          collapse = "|"
        ),
        stringr::str_flatten(
          stringr::str_glue("\\b{iea3_4}\\b"),
          collapse = "|"
        ),
        stringr::str_flatten(
          stringr::str_glue("\\b{iea3_5}\\b"),
          collapse = "|"
        ),
        stringr::str_flatten(
          stringr::str_glue("\\b{iea3_6}\\b"),
          collapse = "|"
        ),
        stringr::str_flatten(
          stringr::str_glue("\\b{iea3_7}\\b"),
          collapse = "|"
        ),
        stringr::str_flatten(
          stringr::str_glue("\\b{iea4_1}\\b"),
          collapse = "|"
        ),
        stringr::str_flatten(
          stringr::str_glue("\\b{iea4_2}\\b"),
          collapse = "|"
        ),
        stringr::str_flatten(
          stringr::str_glue("\\b{iea5_1}\\b"),
          collapse = "|"
        ),
        stringr::str_flatten(
          stringr::str_glue("\\b{iea5_2}\\b"),
          collapse = "|"
        ),
        stringr::str_flatten(
          stringr::str_glue("\\b{iea6_1}\\b"),
          collapse = "|"
        ),
        stringr::str_flatten(
          stringr::str_glue("\\b{iea6_2}\\b"),
          collapse = "|"
        ),
        stringr::str_flatten(
          stringr::str_glue("\\b{iea6_3}\\b"),
          collapse = "|"
        ),
        stringr::str_flatten(
          stringr::str_glue("\\b{iea7_1}\\b"),
          collapse = "|"
        ),
        stringr::str_flatten(
          stringr::str_glue("\\b{iea7_2}\\b"),
          collapse = "|"
        )
      )
    )

    deteccao <- map_lgl(
      .x = expressoes_capitulos$expressoes,
      .f = ~str_detect(string = texto, pattern = .x)
    )

    categorias_encontradas <- expressoes_capitulos %>%
      select(categoria) %>%
      filter(
        deteccao
      )

    saida <- case_when(
      length(categorias_encontradas$categoria) == 0 ~ "Não encontrado",
      length(categorias_encontradas$categoria) == 1 ~ categorias_encontradas$categoria,
      length(categorias_encontradas$categoria) > 1 ~ categorias_encontradas$categoria
    )


  }


  resultado <- df %>%
    select(
      {{processo}},
      {{motor}}
    ) %>%
    rowwise() %>%
    mutate(
      categorias = list(detecta_categorias({{motor}}))
    )

  resultado <- resultado %>% as_tibble() %>%
    mutate(categorias = stringr::str_remove_all(stringr::str_remove_all(categorias, 'c\\('), "\\)")) %>%
    dplyr::mutate(categorias = dplyr::recode(categorias,
                                             "character(0" = "nenhuma categoria encontrada")) %>%
    select(-motor)

  df<-left_join(df, resultado)


}


usethis::use_data(dtc_categorias, overwrite = T)
#usethis::use_data(expressoes_capitulos, overwrite = T)


