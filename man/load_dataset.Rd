% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/csv.R
\name{load_dataset}
\alias{load_dataset}
\title{Carga el dataset en el entorno global}
\usage{
load_dataset(dataset, datadir, envir = .GlobalEnv, recursive = TRUE,
  ...)
}
\arguments{
\item{dataset}{nombre del dataset}

\item{datadir}{directorio en donde se encuentra el dataset}

\item{envir}{entorno donde asignar la variable}

\item{recursive}{buscar recursivamente el dataset dentro del `datadir``}

\item{...}{parametros adicionales para \link[OHLCMerge]{read_ohlcv}}
}
\value{
el nombre de la variable a la que se asigno el dataset
}
\description{
Carga el dataset en el entorno global
}
\examples{
\donttest{
load_dataset("US30D1", datadir = "~/Downloads/datos_ohlc")
}
}
