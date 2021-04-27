
#' Lecture des fichiers Excel de donnees
#'
#' @param chemin Chemin vers un ou plusieurs fichiers Excel a lire.
#' 
#' @note Seule les feuilles commençant par les colonnes "Code" et "Fap" seront lues.
#' 
#' @importFrom readxl excel_sheets read_excel
#' @importFrom data.table rbindlist setDT melt setnames setcolorder
#' @importFrom stats setNames
#'
#' @return un \code{data.table}.
#' @export
#'
#' @examples
lecture_fap_excel <- function(chemin) {
  chemin <- normalizePath(path = chemin, mustWork = TRUE)
  chemin <- enc2native(chemin)
  if (length(chemin) == 1 && dir.exists(chemin)) {
    chemin <- list.files(path = chemin, full.names = TRUE)
  }
  chemin <- stats::setNames(chemin, basename(chemin))
  rbindlist(lapply(
    X = chemin, FUN = lecture_fap_excel_fichier
  ), idcol = "fichier")
}

# Lecture individuelle d'un fichier Excel
lecture_fap_excel_fichier <- function(chemin) {
  feuilles <- readxl::excel_sheets(chemin)
  usethis::ui_todo(paste("Lecture du fichier :", basename(chemin)))
  donnees_feuille <- lapply(
    X = stats::setNames(feuilles, feuilles), 
    FUN = function(x) {
      verification <- readxl::read_excel(
        path = chemin, sheet = x,
        range = "A1:B1",
        col_names = c("v1", "v2")
      )
      verification <- unlist(verification, use.names = FALSE)
      verification <- tolower(verification)
      verification[verification %in% "fap3"] <- "fap"
      if (identical(verification, c("code", "fap"))) {
        donnees <- readxl::read_excel(path = chemin, sheet = x)
        setDT(donnees)
        noms <- copy(names(donnees))
        # Patch emploi & demandeurs emploi
        if (grepl(pattern = "^\\d{4}-\\d{4}$", x = noms[3]) |
            grepl(pattern = "^\\d{4}$", x = noms[3])) {
          donnees[, indicateur_modalite := "-"]
          setcolorder(donnees, append(noms, "indicateur_modalite", after = 2))
        }
        donnees <- melt(
          data = donnees, 
          id.vars = 1:3,
          variable.name = "annees",
          value.name = "valeur", 
          variable.factor = FALSE
        )
        # Correction donnees numerique mal interpretees
        if (!is.numeric(donnees$valeur)) {
          donnees[, valeur := gsub(pattern = ".", replacement = "", fixed = TRUE, x = valeur)]
          donnees[, valeur := gsub(pattern = ",", replacement = ".", fixed = TRUE, x = valeur)]
          donnees[, valeur := as.numeric(valeur)]
        }
        donnees[, indicateur := names(donnees)[3]]
        setnames(donnees, 3, "indicateur_modalite")
        setnames(donnees, 1:2, c("code", "fap"))
        usethis::ui_done(paste("Lecture feuille :", x))
        donnees
      } else {
        usethis::ui_oops(paste("Pas de donn\u00e9es, feuille :", x))
        NULL
      }
    }
  )
  rbindlist(donnees_feuille, idcol = "feuille")
}






#' Ajout du dictionnaire des modalite
#'
#' @param donnees Donnees FAP, lues avec \code{lecture_fap_excel}.
#' @param dictionnaire Dictionnaire (ou chemin vers le fichier Excel)
#'
#' @return La table \code{donnees} avec une colonne en plus: \code{indicateur_label}
#' @export
#'
ajout_dictionnaire <- function(donnees, dictionnaire) {
  if (is.character(dictionnaire)) {
    dictionnaire <- normalizePath(dictionnaire, mustWork = TRUE)
    dictionnaire <- read_excel(path = dictionnaire)
    setDT(dictionnaire)
  }
  merge(
    x = donnees, 
    y = dictionnaire[, list(
      indicateur_modalite = Modalite, indicateur_label = Label, indicateur = Nom_variable
    )],
    by = c("indicateur", "indicateur_modalite"),
    all.x = TRUE
  )
}









