#' Fonction pour déterminer le top 10 ou le bottom 10 
#'
#' @param data Table à séléctionner
#' @param col Nom de la colonne de tri.
#' @param n_top Nombre de valeur à retenir (dans l'ordre décroissant si négatif).
#'
#' @return a data subset with n_top ordered rows 
#' @export
#' 

top <- function(data, col, n_top){
  data <- data[order(data[[col]]),]
  if (n_top > 0){
    top <- head(data, n_top)
  }
  else{
    top <- tail(data, -n_top)
    top <- top[seq(dim(top)[1],1),]
  }
  top
}
