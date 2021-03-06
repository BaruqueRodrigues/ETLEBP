% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cria_base_intermediaria_aneel.R
\name{cria_base_intermediaria_aneel}
\alias{cria_base_intermediaria_aneel}
\title{Cria a base intemediária para a aneel criando um dataframe}
\usage{
cria_base_intermediaria_aneel(
  origem_processos = here::here("data/SGPED_BI/PD Busca Textual.csv"),
  origem_equipes = here::here("data/SGPED_BI/5.PD RF EQUIPE.csv")
)
}
\arguments{
\item{origem_processos}{dataset PD Busca Textual que contém os projetos de energia da fonte aneel}

\item{origem_equipes}{dataset que 5.PD RF EQUIPE contém informações complementares da fonte aneel}
}
\value{

}
\description{
Cria a base intemediária para a aneel criando um dataframe
}
\examples{
cria_base_intermediaria_aneel()
}
