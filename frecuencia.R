#!/usr/bin/env Rscript

# =========================
# Dependencias
# =========================
paquetes_necesarios <- c("readxl", "cli", "knitr", "optparse", "ggplot2")
nuevos_paquetes <- paquetes_necesarios[!(paquetes_necesarios %in% installed.packages()[,"Package"])]

if(length(nuevos_paquetes)) {
  message("Instalando dependencias visuales faltantes... paciencia.")
  install.packages(nuevos_paquetes, repos = "http://cran.us.r-project.org")
}

suppressPackageStartupMessages({
  library(cli)
  library(knitr)
  library(optparse)
  library(ggplot2)
})

# =========================
# Funciones Analíticas
# =========================

#' Calcula y formatea una tabla de frecuencias absolutas y relativas.
#'
#' @description Toma un vector de datos, elimina los valores nulos (NA), 
#' construye un data.frame con las métricas estadísticas y calcula las acumuladas.
#' Permite elegir si ordenar por frecuencia o mantener el orden lógico original de las categorías.
#'
#' @param var Vector con los datos a analizar (puede ser categórico, numérico o factor).
#' @param ordenar_fr Lógico. Si es TRUE (por defecto), ordena de menor a mayor frecuencia relativa.
#' @return Un data.frame estadístico formateado con sus acumuladas.
#' @export
tabla_frecuencias <- function(var, ordenar_fr = TRUE) {
  var <- var[!is.na(var)]
  tabla <- table(var)
  df <- as.data.frame(tabla)
  colnames(df) <- c("Categoria", "Frecuencia")

  # 1. Frecuencias simples
  total <- sum(df$Frecuencia)
  df$fr <- df$Frecuencia / total
  df$porcentaje <- df$fr * 100

  # 2. Ordenamiento condicional (Acá salvamos los intervalos de las continuas)
  if (ordenar_fr) {
    df <- df[order(df$fr), ]
  }

  # 3. Las acumuladas
  df$Fi <- cumsum(df$Frecuencia)
  df$Fr <- cumsum(df$fr)
  df$porcentaje_acum <- cumsum(df$porcentaje)

  # 4. Formateo visual
  df$fr <- sprintf("%.4f", df$fr)
  df$porcentaje <- sprintf("%.2f%%", df$porcentaje)
  df$Fr <- sprintf("%.4f", df$Fr)
  df$porcentaje_acum <- sprintf("%.2f%%", df$porcentaje_acum)

  return(df)
}

#' Calcula métricas de dispersión y agrupa una variable continua.
#'
#' @description Toma un vector numérico continuo, calcula el Rango,
#' la cantidad de intervalos usando la Regla de Sturges y la Amplitud.
#' Luego, utiliza la función nativa cut() para agrupar los datos en esos intervalos.
#'
#' @param var Vector numérico con los datos continuos a procesar.
#' @return Una lista que contiene las métricas (rango, k, amplitud) y 
#' el vector de datos agrupados como un factor, listo para ser tabulado.
#' @export
procesar_continua <- function(var) {
  var <- var[!is.na(var)]
  
  # 1. Rango (R)
  val_max <- max(var)
  val_min <- min(var)
  rango <- val_max - val_min
  
  # 2. Cantidad de Intervalos / Clases (k) - Regla de Sturges
  n <- length(var)
  k <- ceiling(1 + 3.322 * log10(n))
  
  # 3. Amplitud (A)
  amplitud <- rango / k
  
  # 4. Agrupar los datos
  datos_agrupados <- cut(var, breaks = k, include.lowest = TRUE, right = FALSE)
  
  resultados <- list(
    metricas = c(Rango = rango, Intervalos = k, Amplitud = amplitud),
    vector_categorico = datos_agrupados
  )
  
  return(resultados)
}

#' Lee un archivo de datos (.csv o .xlsx) y lo convierte en un data.frame puro.
#'
#' @description Evalúa la extensión y carga el archivo en memoria.
#'
#' @param ruta Cadena de texto con la ruta del archivo.
#' @return Un data.frame con los datos.
#' @export
leer_archivo <- function(ruta) {
  ext <- tolower(tools::file_ext(ruta))
  
  if (ext == "csv") {
    df <- read.csv(ruta)
  } else if (ext %in% c("xlsx", "xls")) {
    df <- as.data.frame(readxl::read_excel(ruta))
  } else {
    cli::cli_abort("Extensión {.val {ext}} no soportada. Usa .csv o .xlsx")
  }
  return(df)
}

# =========================
# Funciones de Gráficos
# =========================

#' Genera y guarda un histograma con ggplot2.
#'
#' @description Valida que los datos sean numéricos y exporta un PNG.
#' @param var Vector con los datos a graficar.
#' @param nombre_var Nombre de la variable (para el título y el archivo).
#' @return NULL (Guarda un archivo PNG).
plot_histograma <- function(var, nombre_var) {
  if (!is.numeric(var)) {
    cli::cli_alert_warning("Omitiendo histograma: {.var {nombre_var}} no es numérica.")
    return()
  }
  
  archivo_salida <- paste0("histograma_", nombre_var, ".png")
  df_plot <- data.frame(valor = var[!is.na(var)])
  
  p <- ggplot(df_plot, aes(x = valor)) +
    geom_histogram(fill = "#FF8C00", color = "black", alpha = 0.8, bins = 30) +
    theme_minimal(base_family = "sans") +
    labs(title = paste("Histograma de", nombre_var), x = nombre_var, y = "Frecuencia")
  
  ggsave(archivo_salida, plot = p, width = 8, height = 6, bg = "white")
  cli::cli_alert_success("Guardado: {.file {archivo_salida}}")
}

#' Genera y guarda un gráfico de barras con ggplot2.
#'
#' @description Ideal para variables categóricas. Exporta un PNG.
#' @param var Vector con los datos a graficar.
#' @param nombre_var Nombre de la variable (para el título y el archivo).
#' @return NULL (Guarda un archivo PNG).
plot_barras <- function(var, nombre_var) {
  archivo_salida <- paste0("barras_", nombre_var, ".png")
  df_plot <- data.frame(categoria = var[!is.na(var)])
  
  p <- ggplot(df_plot, aes(x = as.factor(categoria))) +
    geom_bar(fill = "#FF8C00", color = "black", alpha = 0.8) +
    theme_minimal(base_family = "sans") +
    labs(title = paste("Gráfico de Barras de", nombre_var), x = nombre_var, y = "Frecuencia") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggsave(archivo_salida, plot = p, width = 8, height = 6, bg = "white")
  cli::cli_alert_success("Guardado: {.file {archivo_salida}}")
}

# Diccionario de funciones gráficas
mapeo_graficos <- list(
  "histograma" = plot_histograma,
  "barras"     = plot_barras
)

#' Orquesta la generación de múltiples gráficos desde un string.
#'
#' @description Parsea los argumentos solicitados por consola e invoca las funciones.
#' @param string_graficos Cadena de texto con los gráficos (ej: "histograma,barras").
#' @param var Vector de datos a graficar.
#' @param nombre_var El nombre de la variable para etiquetar los gráficos.
#' @export
procesar_graficos <- function(string_graficos, var, nombre_var) {
  lista_graficos <- strsplit(string_graficos, ",")[[1]]
  cli::cli_h2("Renderizado de Gráficos")
  
  for (grafico in lista_graficos) {
    grafico_limpio <- tolower(trimws(grafico))
    funcion_graficadora <- mapeo_graficos[[grafico_limpio]]
    
    if (!is.null(funcion_graficadora)) {
      funcion_graficadora(var, nombre_var)
    } else {
      cli::cli_alert_danger("Gráfico {.val {grafico_limpio}} no reconocido. Opciones: {paste(names(mapeo_graficos), collapse = ', ')}")
    }
  }
}

# =========================
# Parseo de Argumentos CLI
# =========================
option_list <- list(
  make_option(c("-b", "--base"), type="character", default=NULL, 
              help="Ruta del archivo de datos (.csv o .xlsx)"),
  make_option(c("-v", "--var"), type="character", default=NULL, 
              help="Nombre de la variable a analizar"),
  make_option(c("-t", "--tipo"), type="character", default="discreta", 
              help="Tipo de variable: 'discreta' (por defecto) o 'continua'"),
  make_option(c("-g", "--graph"), type="character", default=NULL, 
              help="Gráficos opcionales separados por coma (ej: histograma,barras)")
)

opt_parser <- OptionParser(option_list=option_list, description="Analizador Estadístico y Generador de Gráficos v1.1")
opt <- parse_args(opt_parser)

if (is.null(opt$base) || is.null(opt$var)) {
  cli::cli_alert_danger("Faltan argumentos obligatorios.")
  cli::cli_text("Uso: {.code ./frecuencia.R -b datos.xlsx -v SALARIO -t continua [-g histograma]}")
  quit(status = 1)
}

if (!file.exists(opt$base)) {
  cli::cli_abort("El archivo {.file {opt$base}} no existe.")
}

# =========================
# Ejecución Principal
# =========================
data <- leer_archivo(opt$base)
colnames(data) <- trimws(colnames(data))

if (!(opt$var %in% colnames(data))) {
  cli::cli_abort("La variable {.var {opt$var}} no existe en el dataset.")
}

vector_datos <- data[[opt$var]]

cli::cli_h1("Análisis Estadístico")
cli::cli_alert_info("Dataset: {.file {basename(opt$base)}} | Variable: {.var {opt$var}} | Tipo: {.val {opt$tipo}}")
cat("\n")

# Lógica de bifurcación según el tipo de variable
es_continua <- tolower(opt$tipo) == "continua"

if (es_continua) {
  if (!is.numeric(vector_datos)) {
    cli::cli_abort("La variable {.var {opt$var}} debe ser numérica para tratarla como continua.")
  }
  
  res_continua <- procesar_continua(vector_datos)
  vector_a_tabular <- res_continua$vector_categorico
  
  cli::cli_text(cli::col_green("Métricas calculadas:"))
  cli::cli_bullets(c(
    "*" = paste("Rango (R):", res_continua$metricas["Rango"]),
    "*" = paste("Intervalos (k):", res_continua$metricas["Intervalos"]),
    "*" = paste("Amplitud (A):", round(res_continua$metricas["Amplitud"], 4))
  ))
  cat("\n")
} else {
  vector_a_tabular <- vector_datos
}

# 1. Tabla de Frecuencias (le pasamos el booleano invertido: si es continua NO ordena por fr)
resultado <- tabla_frecuencias(vector_a_tabular, ordenar_fr = !es_continua)

cat(knitr::kable(resultado, format = "markdown", align = "c"), sep = "\n")
cat("\n")
cli::cli_alert_success("Cálculo finalizado exitosamente.")

# 2. Generación de Gráficos (Opcional)
if (!is.null(opt$graph)) {
  procesar_graficos(opt$graph, vector_datos, opt$var)
}