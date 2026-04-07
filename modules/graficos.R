#' Genera un histograma.
#'
#' @param var Datos continuos.
#' @param nombre_var Etiqueta.
#' @param cortes Límites de intervalos.
#' @export
plot_histograma <- function(var, nombre_var, cortes = NULL) {
  if (!is.numeric(var)) return()
  archivo <- paste0("histograma_", nombre_var, ".png")
  p <- ggplot(data.frame(valor = var[!is.na(var)]), aes(x = valor))
  if (!is.null(cortes)) {
    p <- p + geom_histogram(breaks = cortes, fill = "#FF8C00", color = "black") + scale_x_continuous(breaks = cortes)
  } else {
    p <- p + geom_histogram(fill = "#FF8C00", color = "black", bins = 30)
  }
  ggsave(archivo, plot = p + theme_minimal() + labs(title=paste("Histograma:", nombre_var)), width = 8, height = 6)
  cli::cli_alert_success("Guardado: {.file {archivo}}")
}

#' Genera un gráfico de barras.
#'
#' @param var Datos discretos.
#' @param nombre_var Etiqueta.
#' @export
plot_barras <- function(var, nombre_var, cortes = NULL) {
  archivo <- paste0("barras_", nombre_var, ".png")
  p <- ggplot(data.frame(Valores = var[!is.na(var)]), aes(x = as.factor(Valores))) +
    geom_bar(fill = "#FF8C00", color = "black") + theme_minimal()
  ggsave(archivo, plot = p + labs(title=paste("Barras:", nombre_var)), width = 8, height = 6)
  cli::cli_alert_success("Guardado: {.file {archivo}}")
}

#' Genera gráfico de sectores con porcentajes y leyenda inferior.
#'
#' @description Crea un gráfico circular (pie chart) calculando las proporciones
#' exactas e imprimiendo las etiquetas de porcentaje sobre cada sector.
#' @param var Datos discretos a graficar.
#' @param nombre_var Etiqueta de la variable.
#' @param cortes Placeholder por compatibilidad.
#' @export
plot_sectores <- function(var, nombre_var, cortes = NULL) {
  archivo <- paste0("sectores_", nombre_var, ".png")
  
  # 1. Armamos el dataframe
  df <- as.data.frame(table(var[!is.na(var)]))
  colnames(df) <- c("Valores", "f")
  
  # 2. Calculamos los porcentajes y preparamos la etiqueta
  df$porcentaje <- (df$f / sum(df$f)) * 100
  df$etiqueta <- sprintf("%.1f%%", df$porcentaje)
  
  # 3. Construimos el gráfico
  p <- ggplot(df, aes(x = "", y = f, fill = Valores)) + 
    # Agregamos un borde blanco sutil a las porciones para que se vea más limpio
    geom_bar(stat = "identity", width = 1, color = "white") + 
    coord_polar("y", start = 0) + 
    # Colocamos las etiquetas en el medio de cada sector
    geom_text(aes(label = etiqueta), position = position_stack(vjust = 0.5), 
              color = "black", fontface = "bold", size = 4) +
    theme_void() +
    # Movemos la leyenda abajo y le damos un título amigable
    theme(legend.position = "bottom") +
    labs(title = paste("Sectores:", nombre_var), fill = "Categorías:")
    
  ggsave(archivo, plot = p, width = 8, height = 6, bg = "white")
  cli::cli_alert_success("Guardado: {.file {archivo}}")
}

#' Genera gráfico escalonado.
#'
#' @param var Datos numéricos discretos.
#' @param nombre_var Etiqueta.
#' @export
plot_escalonado <- function(var, nombre_var, cortes = NULL) {
  archivo <- paste0("escalonado_", nombre_var, ".png")
  p <- ggplot(data.frame(v = var[!is.na(var)]), aes(x = v)) + stat_ecdf(geom = "step", color = "#FF8C00", linewidth = 1) + theme_minimal()
  ggsave(archivo, plot = p + labs(title=paste("Escalonado:", nombre_var)), width = 8, height = 6)
  cli::cli_alert_success("Guardado: {.file {archivo}}")
}

#' Genera polígono de frecuencias.
#'
#' @param var Datos numéricos.
#' @param nombre_var Etiqueta.
#' @export
plot_poligono <- function(var, nombre_var, cortes = NULL) {
  archivo <- paste0("poligono_", nombre_var, ".png")
  p <- ggplot(data.frame(v = var[!is.na(var)]), aes(x = v))
  if (!is.null(cortes)) p <- p + geom_freqpoly(breaks = cortes, color = "#FF8C00") else p <- p + geom_freqpoly(color = "#FF8C00")
  ggsave(archivo, plot = p + theme_minimal() + labs(title=paste("Polígono:", nombre_var)), width = 8, height = 6)
  cli::cli_alert_success("Guardado: {.file {archivo}}")
}

#' Genera una ojiva.
#'
#' @param var Datos numéricos.
#' @param nombre_var Etiqueta.
#' @export
plot_ojiva <- function(var, nombre_var, cortes = NULL) {
  archivo <- paste0("ojiva_", nombre_var, ".png")
  p <- ggplot(data.frame(v = var[!is.na(var)]), aes(x = v))
  if (!is.null(cortes)) {
    p <- p + stat_bin(aes(y = cumsum(after_stat(count))), breaks = cortes, geom = "line", color = "#FF8C00")
  } else {
    p <- p + stat_bin(aes(y = cumsum(after_stat(count))), geom = "line", color = "#FF8C00")
  }
  ggsave(archivo, plot = p + theme_minimal() + labs(title=paste("Ojiva:", nombre_var)), width = 8, height = 6)
  cli::cli_alert_success("Guardado: {.file {archivo}}")
}

mapeo_graficos <- list(
  "histograma" = plot_histograma, "barras" = plot_barras, "sectores" = plot_sectores,
  "escalonado" = plot_escalonado, "poligono" = plot_poligono, "ojiva" = plot_ojiva
)

#' Orquesta la generación de gráficos.
#'
#' @param string_graficos Flags de entrada.
#' @param var_cruda Datos originales.
#' @param var_tabular Datos categorizados.
#' @param cortes Límites.
#' @param nombre_var Etiqueta.
#' @param es_continua Booleano.
#' @export
procesar_graficos <- function(string_graficos, var_cruda, var_tabular, cortes, nombre_var, es_continua) {
  graficos_c <- c("histograma", "poligono", "ojiva")
  graficos_d <- c("barras", "sectores", "escalonado")
  lista <- if(tolower(string_graficos) == "all") { if(es_continua) graficos_c else graficos_d } else { unlist(strsplit(string_graficos, ",")) }
  lista <- intersect(lista, if(es_continua) graficos_c else graficos_d)
  
  for (g in lista) {
    if (g %in% graficos_c) mapeo_graficos[[g]](var_cruda, nombre_var, cortes)
    else mapeo_graficos[[g]](var_tabular, nombre_var)
  }
}