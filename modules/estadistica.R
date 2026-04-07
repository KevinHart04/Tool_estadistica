#' Calcula y formatea una tabla de frecuencias.
#'
#' @param var Vector con los datos a analizar.
#' @param ordenar_fr Lógico. Si es TRUE, ordena de menor a mayor frecuencia.
#' @return Un data.frame estadístico.
#' @export
tabla_frecuencias <- function(var, ordenar_fr = TRUE) {
  var <- var[!is.na(var)]
  tabla <- table(var)
  df <- as.data.frame(tabla)
  colnames(df) <- c("Valores", "Frecuencia")
  df$Valores <- as.character(df$Valores)
  total <- sum(df$Frecuencia)
  df$fr <- df$Frecuencia / total
  df$porcentaje <- df$fr * 100
  if (all(!is.na(suppressWarnings(as.numeric(df$Valores))))) {
    df <- df[order(as.numeric(df$Valores)), ]
  } else {
    df <- df[order(df$Valores), ]
  }
  df$Fi <- cumsum(df$Frecuencia)
  df$Fr <- cumsum(df$fr)
  df$porcentaje_acum <- cumsum(df$porcentaje)
  total_frecuencia <- sum(df$Frecuencia)
  total_fr <- sum(df$fr)
  total_porcentaje <- sum(df$porcentaje)
  df$fr <- sprintf("%.4f", df$fr)
  df$porcentaje <- sprintf("%.2f%%", df$porcentaje)
  df$Fr <- sprintf("%.4f", df$Fr)
  df$porcentaje_acum <- sprintf("%.2f%%", df$porcentaje_acum)
  fila_total <- data.frame(
    Valores = "TOTAL", Frecuencia = total_frecuencia, fr = sprintf("%.4f", total_fr),
    porcentaje = sprintf("%.2f%%", total_porcentaje), Fi = total_frecuencia,
    Fr = sprintf("%.4f", total_fr), porcentaje_acum = sprintf("%.2f%%", total_porcentaje)
  )
  return(rbind(df, fila_total))
}

#' Agrupa una variable continua.
#'
#' @param var Vector numérico continuo.
#' @param min_custom Límite inferior forzado.
#' @param amp_custom Amplitud de clase forzada.
#' @return Lista con métricas, cortes y vector categórico.
#' @export
procesar_continua <- function(var, min_custom = NULL, amp_custom = NULL) {
  var <- var[!is.na(var)]
  val_max <- max(var); val_min <- min(var); rango <- val_max - val_min
  inicio <- if (!is.null(min_custom)) min_custom else val_min
  if (!is.null(amp_custom)) { amplitud <- amp_custom } else {
    k_sturges <- ceiling(1 + 3.322 * log10(length(var)))
    amplitud <- rango / k_sturges
  }
  fin <- inicio + ceiling((val_max - inicio) / amplitud) * amplitud
  cortes <- seq(from = inicio, to = fin, by = amplitud)
  return(list(
    metricas = c(Rango = rango, Intervalos = length(cortes) - 1, Amplitud = amplitud),
    cortes = cortes,
    vector_categorico = cut(var, breaks = cortes, include.lowest = TRUE, right = FALSE)
  ))
}

#' Calcula la moda.
#'
#' @param v Vector con los datos.
#' @return El valor más frecuente.
#' @export
calcular_moda <- function(v) {
  v <- v[!is.na(v)]
  unicos <- unique(v)
  unicos[which.max(tabulate(match(v, unicos)))]
}

#' Interpreta la asimetría de una distribución.
#'
#' @param media Media aritmética.
#' @param mediana Mediana.
#' @param moda Moda.
#' @return Dictamen de asimetría.
#' @export
interpretar_tendencia <- function(media, mediana, moda) {
  if (is.na(media) || is.na(mediana) || is.na(moda)) return("Faltan datos.")
  moda <- as.numeric(moda)
  tol <- abs(media) * 0.05 
  if (abs(media - mediana) <= tol && abs(mediana - moda) <= tol) {
    return("Distribución Simétrica (campana). Datos balanceados en el centro.")
  } else if (media > mediana && mediana >= moda) {
    return("Asimetría Positiva (sesgada a la derecha). Atípicos altos elevan la media.")
  } else if (media < mediana && mediana <= moda) {
    return("Asimetría Negativa (sesgada a la izquierda). Atípicos bajos arrastran la media.")
  } else {
    return("Distribución atípica o bimodal.")
  }
}