#' captura os termos GESI
#'
#' @param df dataset que contém os projetos de energia
#' @param processo atributo de codigo único do projeto
#' @param motor vetor com os atributos utilizados para a classificação
#' @import stringr
#' @import purrr
#' @import dplyr
#' @return
#' @export
#'
#' @examples
#' dtc_gesi(dataset, id, motor)
dtc_gesi<- function(df,processo, motor){
  dtc_texto <- function(texto){

    energia <-c("energia","limpa","energia","renovavel","transicao energetica",
                "transformacao energetica","GEE","mudanca climactica","eolica",
                "solar","fotovoltaica","FV","biomassa","biocombustivel","biodiesel",
                "diesel","residuos","biogas","biodigestor","hidrogenio","baterias",
                "medidores inteligentes","sistemas","inteligentes","GD","DER",
                "distribuida","consorcio","cooperativa","concessionaria","tarifa")

    baixa_renda <- c("pobreza","vulnerav*","vulnerabilidade","pobre","TSEE",
                     "tarifa social","baixa-renda", "baixa renda",
                     "acessibildiade","acesso a energia","subsistencia",
                     "bolsa familia","programa social",
                     "comunidade","periferia","periferico",
                     "(vulnerabilidade.*socia)","favela")
    geracao_renda <- c("microempresa","microempreendedor","MPE","MEI","pequena empresa",
                       "uso produtivo","start up", "start-up")

    negros<- c("quilombo","pardos","afro-brasileir*","negr*","minoria","raca",
               'etnia',"pretos", "preta","pardos", "parda")

    indigenas <- c("tribo","indigena","indio","FUNAI","aldeia","APA",
                   "(area.*preservacao.*ambiental)","(area.*preservacao)",
                   "RESEX","reserva extratavista")

    jovens <- c("adolescente","crianca","infan*","alun*","estudante", "aprendiz" ,"jovem aprendiz","estagi*")

    mulheres <- c("feminin*","menin*","mocas","mulher","esposa",
                  "empreendedora","empresaria",'produtora',"aluna","coordenadora")

    pcd <- c("deficiencia", "(deficiente.*visual)", "(deficiente.*auditivo)",
             "(deficiente.*fisico)", "(deficiente.*motora)","(paralisia.*cerebral)",
             "incapacitad", "(pessoa.*deficiencia)","(deficiente.*intelectual)",
             "(deficiencia.*fisica)", "(deficiencia.*auditiva)",
             "(deficiencia.*motora)","ELA", "pcd")

    nordeste <- c("baian", "pernambucan", "cearense", "piauiense",
                  "paraiban","potiguar", "maranhense", "sergipan",
                  "alagoan",  "soteropolitano", "recifense",
                  "(sertao.*bahia)","(sertao.*pernambuco)","(sertao.*ceara)",
                  "(sertao.*piaui)","(sertao.*paraiba)","(sertao.*maranhao)",
                  "(sertao.sergipe*)","(sertao.*alagoas)", "alagoas", "bahia",
                  "sergipe", "ceara", "paraiba", "piaui", "pernambuco", "maranhao",
                  "rio grande do norte", "nordeste", "caatinga", "mata atlantica",
                  "parnaiba", "sao francisco")

    norte <- c( "amazonia", "(amazonia.*legal)", "(populacao.*ribeirinha)",
                "ribeirinh", "desmatamento", "desmatado", "madeira",
                "acre", "amazonas", "tocantins", "roraima", "rondonia",
                "amapa", "norte", 'nortista', "amazonense","paraense", "manauara",
                "acreano", "acriano", "rondonense", "roraimense", "tocantinense",
                "amazonica")

    centro_oeste <- c("fazend", "grao", "soja", "(centro.*oeste)", "goias",
                      "goiania", "mato grosso", "pantanal", "pantaneiro",
                      "mato grosso do sul", "goiano","mato grossense", "sul mato grossense",
                      "cerrado", "(bacia.*paraguai)")

    sudeste <- c("sao paulo", "rio de janeiro", "minas gerais", "espirito santo",
                 "minas gerais", "belo horizonte", "vitoria", "baia de santos",
                 "baia de guanabara", "sudeste", "paulista", "paulistano",
                 "carioca", "fluminense", "mineiro", "capixaba", "mata atlantica")

    sul <- c("sul", "sulista", "parana", "santa catarina", "rio grande do sul",
             "florianopolis", "curitiba", "porto alegre", "gauch","pampa")

    urbano <- c("metropolitan", "urbano", "saneamento", "medidor inteligente",
                "rede inteligente", "suburbio", "periferia", "favela", "comunidade",
                "assentamento")

    rural <- c("suino", "gado", "bovino", "ovino", "aves", "avicultora", "psicultura",
               "bovinocultura",  "suinocultura", "agricultura", "pecuaria",
               "pecuarista", "(pequeno.*agricultor)", "extrativista", "terra",
               "agro","sistema isolado",
               "acesso a energia da rede", "SHS", "energia da qualidade")

    termos_gesi <- dplyr::tibble(termos = c("baixa_renda",
                                            "centro_oeste",
                                            #"energia",
                                            "geracao_renda",
                                            "indigenas",
                                            "jovens",
                                            "mulheres",
                                            "negros",
                                            "nordeste",
                                            "norte",
                                            "pcd",
                                            "rural",
                                            "sudeste",
                                            "sul",
                                            "urbano"),
                          expressoes = c(str_flatten(
                            str_glue("\\b{baixa_renda}\\b"),
                            collapse = "|"
                          ),
                          str_flatten(
                            str_glue("\\b{centro_oeste}\\b"),
                            collapse = "|"
                          ),
                          #str_flatten(
                          #  str_glue("\\b{energia}\\b"),
                          #  collapse = "|"
                          #),
                          str_flatten(
                            str_glue("\\b{geracao_renda}\\b"),
                            collapse = "|"
                          ),
                          str_flatten(
                            str_glue("\\b{indigenas}\\b"),
                            collapse = "|"
                          ),
                          str_flatten(
                            str_glue("\\b{jovens}\\b"),
                            collapse = "|"
                          ),
                          str_flatten(
                            str_glue("\\b{mulheres}\\b"),
                            collapse = "|"
                          ),
                          str_flatten(
                            str_glue("\\b{negros}\\b"),
                            collapse = "|"
                          ),
                          str_flatten(
                            str_glue("\\b{nordeste}\\b"),
                            collapse = "|"
                          ),
                          str_flatten(
                            str_glue("\\b{norte}\\b"),
                            collapse = "|"
                          ),
                          str_flatten(
                            str_glue("\\b{pcd}\\b"),
                            collapse = "|"
                          ),
                          str_flatten(
                            str_glue("\\b{rural}\\b"),
                            collapse = "|"
                          ),
                          str_flatten(
                            str_glue("\\b{sudeste}\\b"),
                            collapse = "|"
                          ),
                          str_flatten(
                            str_glue("\\b{sul}\\b"),
                            collapse = "|"
                          ),
                          str_flatten(
                            str_glue("\\b{urbano}\\b"),
                            collapse = "|"
                          )
                          )

    )

    deteccao <- map_lgl(
      .x = termos_gesi$expressoes,
      .f = ~str_detect(string = texto, pattern = .x)
    )

    categorias_encontradas <- termos_gesi %>%
      select(termos) %>%
      filter(
        deteccao
      )

    saida <- case_when(
      length(categorias_encontradas$termos) == 0 ~ "Não encontrado",
      length(categorias_encontradas$termos) == 1 ~ categorias_encontradas$termos,
      length(categorias_encontradas$termos) > 1 ~ categorias_encontradas$termos
    )


  }
  resultado <- df %>%
    select(
      {{processo}},
      {{motor}}
    ) %>%
    rowwise() %>%
    mutate(
      termos = list(dtc_texto({{motor}}))
    )

  resultado <- resultado %>% as_tibble() %>%
    mutate(termos = str_remove_all(str_remove_all(termos, 'c\\('), "\\)")) %>%
    select(-motor)

  df<-left_join(df, resultado)

  df

}


