#' Entrenar un perceptron simple
#'
#' @param X Matriz o data.frame de predictores numericos.
#' @param y Vector binario con etiquetas 0 y 1.
#' @param epocas Numero de pasadas por los datos.
#' @param tasa Tasa de aprendizaje positiva.
#' @return Lista con pesos, sesgo, medias y desviaciones.
#' @export
entrenar_perceptron <- function(X, y, epocas = 100, tasa = 0.01) {
  X <- as.matrix(X)
  if (!is.numeric(X) || length(dim(X)) != 2L || nrow(X) < 2L || ncol(X) < 1L || any(!is.finite(X))) {
    stop("X debe contener datos numericos finitos, con al menos dos filas.")
  }
  if (is.factor(y)) y <- as.character(y)
  y <- suppressWarnings(as.numeric(y))
  if (length(y) != nrow(X) || anyNA(y) || !all(y %in% c(0, 1))) {
    stop("y debe contener solo etiquetas 0 y 1, una por fila de X.")
  }
  if (length(epocas) != 1L || !is.finite(epocas) || epocas < 1 || epocas != as.integer(epocas)) {
    stop("epocas debe ser un entero positivo.")
  }
  if (length(tasa) != 1L || !is.finite(tasa) || tasa <= 0) {
    stop("tasa debe ser un numero positivo.")
  }
  medias <- colMeans(X)
  desviaciones <- apply(X, 2L, sd)
  desviaciones[desviaciones == 0] <- 1
  Xs <- scale(X, center = medias, scale = desviaciones)
  pesos <- rep(0, ncol(Xs))
  sesgo <- 0
  for (e in seq_len(epocas)) {
    for (i in sample(seq_len(nrow(Xs)))) {
      z <- sum(Xs[i, ] * pesos) + sesgo
      pred <- as.integer(z >= 0)
      error <- y[i] - pred
      pesos <- pesos + tasa * error * Xs[i, ]
      sesgo <- sesgo + tasa * error
    }
  }
  list(pesos = pesos, sesgo = sesgo,
       medias = medias, desviaciones = desviaciones)
}
