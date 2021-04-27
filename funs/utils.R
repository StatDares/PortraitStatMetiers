

#' Ajoute une colonne pourcentage a un tableau
#'
#' @param data Un jeu de donnees (format \code{data.table}).
#' @param var Varaible contenant les valeurs pour lesquelles calculer un pourcentage.
#' @param by Effectuer l'operation selon une ou des variables de groupes.
#'
#' @return Le \code{data.table} avec une colonne supplementaire.
#' @export
#'
#' @examples
#' dt <- data.table(
#'   groupe = rep(c("a", "b")),
#'   valeurs = runif(100, 1, 1000)
#' )
#' 
#' ajout_pourcentage(dt, "valeurs")
#' dt
#' 
#' ajout_pourcentage(dt, "valeurs", "groupe")
#' dt
ajout_pourcentage <- function(data, var, by = NULL) {
  data[, pourcentage := get(var) / sum(get(var), na.rm = TRUE), by = by]
}



#' Ordonne les niveaux de diplome
#'
#' @param x Une table avec une variable \code{diplome}.
#'
#' @return La table avec la colonne \code{diplome} ordonnee.
#' @export
#'
ordonne_niv_dipl <- function(x) {
  ordre_dipl <- c(
    "Dipl\u00f4me sup\u00e9rieur (bac +3 ou plus)",
    "Bac + 2",
    "Bac, brevet professionnel ou \u00e9quivalent",
    "CAP, BEP ou autre dipl\u00f4me \u00e9quivalent",
    "Aucun dipl\u00f4me, CEP ou brevet des coll\u00e8ges",
    "En cours d\'\u00e9tudes initiales"
  )
  x[, diplome := gsub(pattern = "+ 3", replacement = "+3", x = enc2utf8(diplome), fixed = TRUE)]
  x[, diplome := factor(x = diplome, levels = rev(ordre_dipl), ordered = TRUE)]
  if (!is.null(x$milliers))
    x[is.na(milliers), milliers := 0]
  if (!is.null(x$pourcentage))
    x[is.na(pourcentage), pourcentage := 0]
  setorder(x, diplome)
}


#' Ordonne les niveaux de salaire
#'
#' @param x Une table avec une variable \code{salaire}.
#'
#' @return La table avec la colonne \code{salaire} ordonnee.
#' @export
#'
ordonne_salaire <- function(x) {
  ordre_salaire <- c(
    "Moins de 1250 \u20ac", 
    "De 1250 \u20ac \u00e0 moins de 1500 \u20ac", 
    "De 1500 \u20ac \u00e0 moins de 2000 \u20ac", 
    "De 2000 \u20ac \u00e0 moins de 3000 \u20ac", 
    "3000 \u20ac ou plus"
  )
  # x[, salaire := icon]
  x[, salaire := factor(x = salaire, levels = rev(ordre_salaire), ordered = TRUE)]
  setorder(x, salaire)
}

#' Ordonne les modalites de temps de travail
#'
#' @param x Une table avec une variable \code{Temps_travail}.
#'
#' @return La table avec la colonne \code{Temps_travail} ordonnee.
#' @export
#'
ordonne_temps_travail <- function(x) {
  ordre_temps_travail <- c(
    "dont sous-emploi",
    "Temps partiel",
    "Temps complet travaillant plus de 40h par semaine"
  )
  # x[, salaire := icon]
  x[, temps_travail := factor(x = temps_travail, levels = rev(ordre_temps_travail), ordered = TRUE)]
  setorder(x, temps_travail)
}


wrap_labels <- function(x) {
  strwrap(
    x = enc2utf8(as.character(unique(x))),
    width = 16, simplify = FALSE
  )
}



`%notin%` <- Negate(`%in%`)


