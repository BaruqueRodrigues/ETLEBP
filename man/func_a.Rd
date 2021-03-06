% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/func_a.R
\name{func_a}
\alias{func_a}
\title{Calcula o valor dos dispêndios do projeto por ano em que o projeto foi executado}
\usage{
func_a(df, processo, data_inicio, prazo_utilizacao, valor_projeto)
}
\arguments{
\item{df}{dataframe que contem os projetos de energia}

\item{processo}{atributo de id unico do projeto de energia}

\item{data_inicio}{atributo que mede a data de inicio do contrato do projeto de energia}

\item{prazo_utilizacao}{atributo que mede a data de finalização do contrato do projeto de energia}

\item{valor_projeto}{atributo que mede o valor de financiamento do projeto de energia}
}
\value{

}
\description{
Calcula o valor dos dispêndios do projeto por ano em que o projeto foi executado
}
\examples{
func_a(dataset, id, data_inicio, prazo_utilizacao, valor_projeto)
}
