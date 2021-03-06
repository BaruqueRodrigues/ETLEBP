
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ETLEBP

Bem-vindos ao pacote ETLEBP. O pacote executa as etapas de extração,
tratamento e carga da plataforma INOVA-E.

``` r
devtools::install_github("BaruqueRodrigues/ETLEBP")
```

O processo de ETL começa com a função com a criação das bases
intermediárias. Cada fonte de dados tem uma função especifica que
executa os tratamentos particulares e necessários a cada fonte. No caso
da fonte ANEEL, que usaremos como exemplo para fins didáticos, só é
necessário executar a seguinte função

``` r
base_intermediaria_aneel <- cria_base_intermediaria_aneel()
```

Em seguida decidimos qual tipo de tratamento iremos aplicar, para fins
didáticos iremos fazer uma carga intermediária, onde o objetivo da carga
é inserir apenas novos casos

``` r
base_intermediaria_aneel <- executa_tratamento_incremental(base_intermediaria_aneel, diretorio_sqlite)
```

Após executar o tratamento para a carga incremental a base deve ter as
categorias de IEA avaliadas por um analista. Essa etapa é feita no
pacote EBPInterface, que produz um dataset validado que enriquecerá o
dataset intermediario.

``` r
base_validada_aneel <- valida_dataset(base_intermediaria_aneel, dataset_validado_aneel)
```

Em seguida a base está pronta para a carga.

``` r
executa_carga_incremental(base_validada_aneel, diretorio_sqlite)
```
