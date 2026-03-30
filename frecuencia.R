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
  colnames(df) <- c("Valores", "Frecuencia")

  # Evitar problemas con factores
  df$Valores <- as.character(df$Valores)

  # 1. Frecuencias simples
  total <- sum(df$Frecuencia)
  df$fr <- df$Frecuencia / total
  df$porcentaje <- df$fr * 100

  # 2. Ordenamiento correcto por categoría
  if (all(!is.na(suppressWarnings(as.numeric(df$Valores))))) {
    df <- df[order(as.numeric(df$Valores)), ]
  } else {
    df <- df[order(df$Valores), ]
  }

  # 3. Acumuladas (DESPUÉS de ordenar)
  df$Fi <- cumsum(df$Frecuencia)
  df$Fr <- cumsum(df$fr)
  df$porcentaje_acum <- cumsum(df$porcentaje)

  # 4. Guardar totales ANTES de formatear
  total_frecuencia <- sum(df$Frecuencia)
  total_fr <- sum(df$fr)
  total_porcentaje <- sum(df$porcentaje)

  # 5. Formateo visual
  df$fr <- sprintf("%.4f", df$fr)
  df$porcentaje <- sprintf("%.2f%%", df$porcentaje)
  df$Fr <- sprintf("%.4f", df$Fr)
  df$porcentaje_acum <- sprintf("%.2f%%", df$porcentaje_acum)

  # 6. Fila TOTAL
  fila_total <- data.frame(
    Valores = "TOTAL",
    Frecuencia = total_frecuencia,
    fr = sprintf("%.4f", total_fr),
    porcentaje = sprintf("%.2f%%", total_porcentaje),
    Fi = total_frecuencia,
    Fr = sprintf("%.4f", total_fr),
    porcentaje_acum = sprintf("%.2f%%", total_porcentaje)
  )

  # 7. Agregar al final
  df <- rbind(df, fila_total)

  return(df)
}

#' Calcula métricas de dispersión y agrupa una variable continua.
#'
#' @description Toma un vector numérico continuo, calcula el Rango,
#' la cantidad de intervalos usando la Regla de Sturges y la Amplitud.
#' Calcula los cortes exactos (breaks) para alinear los gráficos con la tabla.
#' Luego, utiliza la función nativa cut() para agrupar los datos en esos intervalos.
#'
#' @param var Vector numérico con los datos continuos a procesar.
#' @return Una lista que contiene las métricas (rango, k, amplitud), los cortes calculados, 
#' y el vector de datos agrupados como un factor.
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
  
  # 4. Cortes exactos para garantizar alineación entre gráficos y tabla
  cortes <- seq(from = val_min, to = val_max, length.out = k + 1)
  
  # 5. Agrupar los datos
  datos_agrupados <- cut(var, breaks = cortes, include.lowest = TRUE, right = FALSE)
  
  resultados <- list(
    metricas = c(Rango = rango, Intervalos = k, Amplitud = amplitud),
    cortes = cortes,
    vector_categorico = datos_agrupados
  )
  
  return(resultados)
}

#' Calcula la moda de un vector numérico o categórico.
#'
#' @description R no tiene una función nativa para la moda, así que 
#' esta función encuentra el valor con mayor frecuencia absoluta tabulando las coincidencias.
#'
#' @param v Vector con los datos a analizar.
#' @return El valor más frecuente dentro del vector.
#' @export
calcular_moda <- function(v) {
  v <- v[!is.na(v)]
  unicos <- unique(v)
  unicos[which.max(tabulate(match(v, unicos)))]
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
#' @description Representa la distribución de frecuencias de una variable continua mediante un gráfico de superficies, respetando los intervalos calculados.
#' @param var Vector con los datos a graficar (debe ser numérico).
#' @param nombre_var Nombre de la variable.
#' @param cortes Vector numérico opcional con los límites exactos de los intervalos.
#' @return NULL (Guarda un archivo PNG).
plot_histograma <- function(var, nombre_var, cortes = NULL) {
  if (!is.numeric(var)) return()
  archivo_salida <- paste0("histograma_", nombre_var, ".png")
  df_plot <- data.frame(valor = var[!is.na(var)])
  
  p <- ggplot(df_plot, aes(x = valor))
  
  if (!is.null(cortes)) {
    p <- p + geom_histogram(breaks = cortes, fill = "#FF8C00", color = "black", alpha = 0.8) +
             scale_x_continuous(breaks = round(cortes, 2))
  } else {
    p <- p + geom_histogram(fill = "#FF8C00", color = "black", alpha = 0.8, bins = 30)
  }
  
  p <- p + theme_minimal(base_family = "sans") +
       labs(title = paste("Histograma de", nombre_var), x = nombre_var, y = "Frecuencia")
  
  ggsave(archivo_salida, plot = p, width = 8, height = 6, bg = "white")
  cli::cli_alert_success("Guardado: {.file {archivo_salida}}")
}

#' Genera y guarda un gráfico de barras con ggplot2.
#'
#' @description Eleva barras para cada categoría con altura proporcional a su frecuencia.
#' @param var Vector con los datos a graficar (tabulados o categóricos).
#' @param nombre_var Nombre de la variable.
#' @param cortes Placeholder para mantener firma compatible (no se usa).
#' @return NULL (Guarda un archivo PNG).
plot_barras <- function(var, nombre_var, cortes = NULL) {
  archivo_salida <- paste0("barras_", nombre_var, ".png")
  df_plot <- data.frame(Valores = var[!is.na(var)])
  
  p <- ggplot(df_plot, aes(x = as.factor(Valores))) +
    geom_bar(fill = "#FF8C00", color = "black", alpha = 0.8) +
    theme_minimal(base_family = "sans") +
    labs(title = paste("Gráfico de Barras de", nombre_var), x = nombre_var, y = "Frecuencia") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggsave(archivo_salida, plot = p, width = 8, height = 6, bg = "white")
  cli::cli_alert_success("Guardado: {.file {archivo_salida}}")
}

#' Genera y guarda un gráfico de sectores (torta) con ggplot2.
#'
#' @description Divide el círculo en sectores cuyo ángulo refleja el porcentaje del total.
#' @param var Vector con los datos a graficar (tabulados o categóricos).
#' @param nombre_var Nombre de la variable.
#' @param cortes Placeholder para mantener firma compatible (no se usa).
#' @return NULL (Guarda un archivo PNG).
plot_sectores <- function(var, nombre_var, cortes = NULL) {
  archivo_salida <- paste0("sectores_", nombre_var, ".png")
  df_plot <- as.data.frame(table(var[!is.na(var)]))
  colnames(df_plot) <- c("Valores", "frecuencia")
  
  p <- ggplot(df_plot, aes(x = "", y = frecuencia, fill = Valores)) +
    geom_bar(stat = "identity", width = 1, color = "black") +
    coord_polar("y", start = 0) +
    theme_void() +
    labs(title = paste("Gráfico de Sectores de", nombre_var))
    
  ggsave(archivo_salida, plot = p, width = 8, height = 6, bg = "white")
  cli::cli_alert_success("Guardado: {.file {archivo_salida}}")
}

#' Genera y guarda un diagrama de frecuencias acumuladas (escalonado).
#'
#' @description Traza una función escalonada con tramos constantes entre valores consecutivos.
#' @param var Vector numérico con los datos a graficar.
#' @param nombre_var Nombre de la variable.
#' @param cortes Placeholder para mantener firma compatible (no se usa).
#' @return NULL (Guarda un archivo PNG).
plot_escalonado <- function(var, nombre_var, cortes = NULL) {
  if (!is.numeric(var)) return()
  archivo_salida <- paste0("escalonado_", nombre_var, ".png")
  df_plot <- data.frame(valor = var[!is.na(var)])
  
  p <- ggplot(df_plot, aes(x = valor)) +
    stat_ecdf(geom = "step", color = "#FF8C00", linewidth = 1) +
    theme_minimal(base_family = "sans") +
    labs(title = paste("Frecuencias Acumuladas de", nombre_var), x = nombre_var, y = "Frecuencia Relativa Acumulada")
    
  ggsave(archivo_salida, plot = p, width = 8, height = 6, bg = "white")
  cli::cli_alert_success("Guardado: {.file {archivo_salida}}")
}

#' Genera y guarda un polígono de frecuencias.
#'
#' @description Línea poligonal uniendo los puntos medios (marcas de clase) superiores de los intervalos.
#' @param var Vector numérico con los datos a graficar.
#' @param nombre_var Nombre de la variable.
#' @param cortes Vector numérico opcional con los límites exactos de los intervalos.
#' @return NULL (Guarda un archivo PNG).
plot_poligono <- function(var, nombre_var, cortes = NULL) {
  if (!is.numeric(var)) return()
  archivo_salida <- paste0("poligono_", nombre_var, ".png")
  df_plot <- data.frame(valor = var[!is.na(var)])
  
  p <- ggplot(df_plot, aes(x = valor))
  
  if (!is.null(cortes)) {
    p <- p + geom_freqpoly(breaks = cortes, color = "#FF8C00", linewidth = 1) +
             scale_x_continuous(breaks = round(cortes, 2))
  } else {
    p <- p + geom_freqpoly(color = "#FF8C00", linewidth = 1, bins = 30)
  }
  
  p <- p + theme_minimal(base_family = "sans") +
       labs(title = paste("Polígono de Frecuencias de", nombre_var), x = nombre_var, y = "Frecuencia")
    
  ggsave(archivo_salida, plot = p, width = 8, height = 6, bg = "white")
  cli::cli_alert_success("Guardado: {.file {archivo_salida}}")
}

#' Genera y guarda una ojiva (polígono de frecuencias acumuladas).
#'
#' @description Representación gráfica de las frecuencias acumuladas de una variable en intervalos.
#' @param var Vector numérico con los datos a graficar.
#' @param nombre_var Nombre de la variable.
#' @param cortes Vector numérico opcional con los límites exactos de los intervalos.
#' @return NULL (Guarda un archivo PNG).
plot_ojiva <- function(var, nombre_var, cortes = NULL) {
  if (!is.numeric(var)) return()
  archivo_salida <- paste0("ojiva_", nombre_var, ".png")
  df_plot <- data.frame(valor = var[!is.na(var)])
  
  p <- ggplot(df_plot, aes(x = valor))
  
  if (!is.null(cortes)) {
    p <- p + stat_bin(aes(y = cumsum(after_stat(count))), breaks = cortes, geom = "line", color = "#FF8C00", linewidth = 1) +
             stat_bin(aes(y = cumsum(after_stat(count))), breaks = cortes, geom = "point", color = "black", size = 2) +
             scale_x_continuous(breaks = round(cortes, 2))
  } else {
    p <- p + stat_bin(aes(y = cumsum(after_stat(count))), geom = "line", color = "#FF8C00", linewidth = 1, bins = 30) +
             stat_bin(aes(y = cumsum(after_stat(count))), geom = "point", color = "black", size = 2, bins = 30)
  }
  
  p <- p + theme_minimal(base_family = "sans") +
       labs(title = paste("Ojiva de", nombre_var), x = nombre_var, y = "Frecuencia Acumulada")
    
  ggsave(archivo_salida, plot = p, width = 8, height = 6, bg = "white")
  cli::cli_alert_success("Guardado: {.file {archivo_salida}}")
}

# Diccionario de funciones gráficas
mapeo_graficos <- list(
  "histograma" = plot_histograma,
  "barras"     = plot_barras,
  "sectores"   = plot_sectores,
  "escalonado" = plot_escalonado,
  "poligono"   = plot_poligono,
  "ojiva"      = plot_ojiva
)

#' Orquesta la generación de múltiples gráficos con enrutamiento inteligente y validación.
#'
#' @description Parsea los argumentos de la consola y delega la graficación. 
#' Filtra los gráficos para que solo se generen aquellos estadísticamente viables 
#' para el tipo de variable (continua o discreta/categórica).
#' @param string_graficos Cadena de texto con los gráficos o 'all'.
#' @param var_cruda Vector numérico original sin agrupar.
#' @param var_tabular Vector categorizado (factor con intervalos).
#' @param cortes Vector numérico opcional con los límites exactos calculados.
#' @param nombre_var El nombre de la variable para etiquetar los gráficos.
#' @param es_continua Booleano que indica si la variable es cuantitativa continua.
#' @export
procesar_graficos <- function(string_graficos, var_cruda, var_tabular, cortes, nombre_var, es_continua) {
  cli::cli_h2("Renderizado de Gráficos")
  
  graficos_continuos <- c("histograma", "poligono", "ojiva")
  graficos_discretos <- c("barras", "sectores", "escalonado")
  
  if (tolower(trimws(string_graficos)) == "all") {
    lista_graficos <- if(es_continua) graficos_continuos else graficos_discretos
    cli::cli_alert_info("Ejecutando gráficos adecuados para variable {.val {ifelse(es_continua, 'continua', 'discreta')}}...")
  } else {
    lista_graficos <- unlist(strsplit(string_graficos, ","))
    lista_graficos <- tolower(trimws(lista_graficos))
    
    # Filtro de seguridad (el patovica)
    graficos_permitidos <- if(es_continua) graficos_continuos else graficos_discretos
    graficos_invalidos <- setdiff(lista_graficos, graficos_permitidos)
    
    if (length(graficos_invalidos) > 0) {
      cli::cli_alert_warning("Excluyendo gráficos estadísticamente incorrectos para este tipo de variable: {.val {graficos_invalidos}}")
    }
    
    lista_graficos <- intersect(lista_graficos, graficos_permitidos)
  }
  
  if (length(lista_graficos) == 0) {
    cli::cli_alert_danger("Ningún gráfico solicitado es válido para esta variable.")
    return()
  }
  
  for (grafico in lista_graficos) {
    funcion_graficadora <- mapeo_graficos[[grafico]]
    
    if (!is.null(funcion_graficadora)) {
      if (grafico %in% graficos_continuos) {
        funcion_graficadora(var = var_cruda, nombre_var = nombre_var, cortes = cortes)
      } else {
        funcion_graficadora(var = var_tabular, nombre_var = nombre_var)
      }
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
              help="Gráficos opcionales separados por coma, o 'all' para todos")
)

opt_parser <- OptionParser(option_list=option_list, description="Analizador Estadístico y Generador de Gráficos v1.6")
opt <- parse_args(opt_parser)

if (is.null(opt$base) || is.null(opt$var)) {
  cli::cli_alert_danger("Faltan argumentos obligatorios.")
  cli::cli_text("Uso: {.code ./frecuencia.R -b datos.xlsx -v SALARIO -t continua [-g all]}")
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
cortes_sturges <- NULL

if (es_continua) {
  if (!is.numeric(vector_datos)) {
    cli::cli_abort("La variable {.var {opt$var}} debe ser numérica para tratarla como continua.")
  }
  
  res_continua <- procesar_continua(vector_datos)
  vector_a_tabular <- res_continua$vector_categorico
  cortes_sturges <- res_continua$cortes
} else {
  vector_a_tabular <- vector_datos
}

# 1. Tabla de Frecuencias
resultado <- tabla_frecuencias(vector_a_tabular, ordenar_fr = !es_continua)

cat(knitr::kable(resultado, format = "markdown", align = "c"), sep = "\n")
cat("\n")

# Imprimir métricas de continuas ABAJO de la tabla
if (es_continua) {
  cli::cli_text(cli::col_green("Métricas de dispersión calculadas:"))
  cli::cli_bullets(c(
    "*" = paste("Rango (R):", res_continua$metricas["Rango"]),
    "*" = paste("Intervalos (k):", res_continua$metricas["Intervalos"]),
    "*" = paste("Amplitud (A):", round(res_continua$metricas["Amplitud"], 4))
  ))
  cat("\n")
}

# Imprimir Tendencia Central
cli::cli_h2("Tendencia Central")
if (is.numeric(vector_datos)) {
  cli::cli_bullets(c(
    "*" = paste("Media:", round(mean(vector_datos, na.rm = TRUE), 4)),
    "*" = paste("Mediana:", median(vector_datos, na.rm = TRUE)),
    "*" = paste("Moda:", calcular_moda(vector_datos))
  ))
} else {
  cli::cli_bullets(c(
    "*" = paste("Moda:", calcular_moda(vector_datos)),
    "*" = "Media: (No aplicable a variables categóricas)",
    "*" = "Mediana: (No aplicable a variables categóricas)"
  ))
}
cat("\n")
cli::cli_alert_success("Cálculo finalizado exitosamente.")

# 2. Generación de Gráficos (Opcional)
if (!is.null(opt$graph)) {
  procesar_graficos(string_graficos = opt$graph, 
                    var_cruda = vector_datos, 
                    var_tabular = vector_a_tabular, 
                    cortes = cortes_sturges, 
                    nombre_var = opt$var,
                    es_continua = es_continua)
}