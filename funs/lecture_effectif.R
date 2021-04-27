
#' Lecture fichier des effectifs 
#'
#' @param chemin Chemin vers le fichier.
#'
#' @return Un \code{data.table}.
#' @export
#'
#' @examples
#' 
#' lecture_effectif("inputs/graph_effectifs.csv")
#' 
lecture_effectif <- function(chemin) {
  data <- fread(file = chemin)
  data <- clean_names(data)
  melt(
    data = data, 
    id.vars = "annee", 
    variable.name = "fap", 
    value.name = "effectif", 
    variable.factor = FALSE
  )
}








