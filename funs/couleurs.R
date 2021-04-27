
#' Palette de couleurs Dares
#'
#' @param couleur Nom de la couleur souhaitée, si \code{NULL} (defaut) toutes les couleurs sont retournees.
#'
#' @return Un vecteur caractere, \code{NULL} si aucune couleur n'est trouve.
#' @export
#'
#' @examples
#' # Toutes les couleurs
#' couleurs_dares()
#' 
#' # Couleur specifique
#' couleurs_dares("bordeau")
#' 
#' # Deux teintes
#' couleurs_dares(c("bleu1", "bleu2"))
#' 
#' # Couleur qui n'existet pas
#' couleurs_dares("bleu4")
#'
couleurs_dares <- function(couleur = NULL) {
  palette_dares <- list(
    "bleu" = "#2d378c",
    "vertcyan" = "#88cac0",
    "grisclair" = "#ededf2",
    "saumon" = "#ff887e",
    "vertclair" = "#77d4c3",
    "vertfonce" = "#326c63",
    
    "pantone178" = "#E30613",
    "bordeau" = "#AA0A2F",
    "pantone178C" = "#FF5959",
    "orange" = "#F39200",
    "pantone_yellowC" = "#FFDD00",
    "pantone_reflex_blueC" = "#001780",
    "bleu1" = "#1D71B8",
    "bleu2" = "#36A9E1",
    "bleu3" = "#5395d0",
    "pantone_bleu" = "#004A99",
    "pantone_C632" = "#0091B3",
    "violet100" = "#4c2795",
    "violet50" = "#9b8ac6",
    "magentaCMJN" = "#E6007E",
    "pantone322C" = "#007078",
    "vertCMJN" = "#009640",
    "vert2" = "#88c946",
    "pantone7545C" = "#435363"
  )
  if (is.null(couleur))
    return(palette_dares)
  couleur <- unique(couleur)
  valide <- intersect(couleur, names(palette_dares))
  if (length(valide) != length(couleur))
    warning("Certaines couleurs n'existent pas dans la palette!")
  unlist(palette_dares[valide], use.names = FALSE)
}



