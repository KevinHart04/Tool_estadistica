#!/usr/bin/env Rscript

#' Script de entrada para R.
#' Orquesta el análisis importando los módulos estadísticos y gráficos.

paquetes <- c("readxl", "cli", "knitr", "optparse", "ggplot2")
faltantes <- paquetes[!(paquetes %in% installed.packages()[,"Package"])]
if(length(faltantes)) install.packages(faltantes, repos = "http://cran.us.r-project.org")
suppressPackageStartupMessages({ library(cli); library(knitr); library(optparse); library(ggplot2) })

source("modules/estadistica.R")
source("modules/graficos.R")

#' Lee el archivo base.
#'
#' @param ruta Path al archivo.
#' @return DataFrame.
#' @export
leer_archivo <- function(ruta) {
  ext <- tolower(tools::file_ext(ruta))
  if (ext == "csv") return(read.csv(ruta))
  if (ext %in% c("xlsx", "xls")) return(as.data.frame(readxl::read_excel(ruta)))
  cli::cli_abort("Extensión no soportada.")
}

option_list <- list(
  make_option(c("-b", "--base"), type="character", default=NULL),
  make_option(c("-v", "--var"), type="character", default=NULL),
  make_option(c("-t", "--tipo"), type="character", default="discreta"),
  make_option(c("-g", "--graph"), type="character", default=NULL),
  make_option(c("-c", "--centralizacion"), action="store_true", default=FALSE),
  make_option(c("-m", "--min"), type="numeric", default=NULL),
  make_option(c("-a", "--amp"), type="numeric", default=NULL)
)

opt <- parse_args(OptionParser(option_list=option_list))
if (is.null(opt$base) || is.null(opt$var)) cli::cli_abort("Faltan argumentos.")

data <- leer_archivo(opt$base)
colnames(data) <- trimws(colnames(data))
if (!(opt$var %in% colnames(data))) cli::cli_abort("Variable no encontrada.")

vector_datos <- data[[opt$var]]
es_continua <- tolower(opt$tipo) == "continua"
cortes_sturges <- NULL

if (es_continua) {
  res <- procesar_continua(vector_datos, opt$min, opt$amp)
  vector_a_tabular <- res$vector_categorico
  cortes_sturges <- res$cortes
} else { vector_a_tabular <- vector_datos }

if (opt$centralizacion) {
  cli::cli_h2(paste("Centralización -", opt$var))
  if (is.numeric(vector_datos)) {
    val_med <- mean(vector_datos, na.rm=TRUE); val_mdn <- median(vector_datos, na.rm=TRUE); val_mod <- calcular_moda(vector_datos)
    cli::cli_bullets(c("*" = paste("Media:", round(val_med, 4)), "*" = paste("Mediana:", val_mdn), "*" = paste("Moda:", val_mod)))
    cli::cli_alert_info(paste("Interpretación:", interpretar_tendencia(val_med, val_mdn, val_mod)))
  } else {
    cli::cli_alert_warning("Categórica. Solo aplica la moda.")
    cli::cli_bullets(c("*" = paste("Moda:", calcular_moda(vector_datos))))
  }
} else {
  cli::cli_h1("Distribución de Frecuencias")
  cat(knitr::kable(tabla_frecuencias(vector_a_tabular, !es_continua), format="markdown", align="c"), sep="\n")
  if (es_continua) cli::cli_bullets(c("*" = paste("Intervalos:", res$metricas["Intervalos"]), "*" = paste("Amplitud:", round(res$metricas["Amplitud"], 4))))
  if (!is.null(opt$graph)) procesar_graficos(opt$graph, vector_datos, vector_a_tabular, cortes_sturges, opt$var, es_continua)
}