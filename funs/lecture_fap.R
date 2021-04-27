
#' @title Lecture des fichiers CSV
#' 
#' @description Cette fonction permet de lire tous les fichiers CSV
#'  d'un repertoire et d'uniformiser le nom de la colonne contenant le code FAP.
#'
#' @param chemin Chemin vers le repertoire contenant les fichiers CSV.
#' @param fap_vars Noms potentiels que peut avoir la variable contenant
#'  les codes FAP, si un CSV ne contient pas une colonne avec un des noms defini.
#'
#' @return Une \code{list} de \code{data.table}
#' @export
#' 
#' @importFrom data.table fread setnames
#' @importFrom janitor clean_names
#'
#' @examples
#' 
#' fap_data <- lecture_fap("inputs/")
#' 
lecture_fap <- function(chemin, fap_vars = c("fap", "fap3", "fap_3")) {
  chemin <- normalizePath(path = chemin, mustWork = TRUE)
  fread2 <- function(...)
    fread(..., encoding = "Latin-1")
  datas <- Map(f = fread2, list.files(path = chemin, full.names = TRUE, pattern = "\\.csv$"))
  names(datas) <- basename(names(datas))
  names(datas) <- gsub(pattern = "\\.csv$", replacement = "", x = names(datas))
  datas <- lapply(datas, clean_names)
  lapply(datas, setnames, old = fap_vars, new = rep("fap", times = length(fap_vars)), skip_absent = TRUE)
  datas[vapply(datas, hasName, name = "fap", FUN.VALUE = logical(1))]
}

# Cette fonction est disponible depuis R 3.4.0,
# On la redefini ici au cas où l'utilisateur ait une version plus ancienne.
hasName <- function (x, name) {
  match(name, names(x), nomatch = 0L) > 0L
}

