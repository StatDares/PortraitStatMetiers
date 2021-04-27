
#' Filtrer les donnees selon un code FAP 
#'
#' @param datas Liste contenant les donnees, resultat de la fonction \code{\link{lecture_fap}}.
#' @param fap_code Un code FAP valide.0
#'
#' @return Une \code{list} de \code{data.table}.
#' @export
#' 
#' @importFrom data.table is.data.table
#'
#' @examples
#' 
#' # Lecture des donnees
#' fap_data <- lecture_fap("inputs/")
#' 
#' # Filtre: Marins, pêcheurs, aquaculteurs
#' filtre_fap(datas, "A3Z")
#' 
filtre_fap <- function(datas, fap_code) {
  fap_code <- tolower(fap_code)
  if (is.data.table(datas)) {
    return(datas[tolower(fap) %in% fap_code])
  }
  if (inherits(datas, "list")) {
    lapply(
      X = datas,
      FUN = function(data) {
        data[tolower(fap) %in% fap_code]
      }
    )
  } else {
    stop("filtre_fap: format non support\u00e9 !")
  }
}

#' Filtrer les donnees FAP 225 selon un code FAP 87  
#'
#' @param datas Liste contenant les donnees, resultat de la fonction \code{\link{lecture_fap}}.
#' @param fap_code Un code FAP valide.0
#'
#' @return Une \code{list} de \code{data.table}.
#' @export
#' 
#' @importFrom data.table is.data.table
#'
#' @examples
#' 
#' # Lecture des donnees
#' fap_data <- lecture_fap("inputs/")
#' 
#' # Filtre: Marins, pêcheurs, aquaculteurs
#' filtre_fap(datas, "A3Z")
#' 
filtre_fap_detaillee <- function(datas, fap_code) {
  fap_code <- tolower(fap_code)
  if (is.data.table(datas)) {
    return(datas[tolower(substr(fap,1,3)) %in% fap_code])
  }
  if (inherits(datas, "list")) {
    lapply(
      X = datas,
      FUN = function(data) {
        data[tolower(substr(fap,1,3)) %in% fap_code]
      }
    )
  } else {
    stop("filtre_fap_detaillee: format non support\u00e9 !")
  }
}

