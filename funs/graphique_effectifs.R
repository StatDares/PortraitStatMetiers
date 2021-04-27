
#' Graphiques des effectifs par annees pour un code FAP
#'
#' @param data Donnees des effectifs, sortie de \code{lecture_effectif}.
#' @param fap_code Code FAP valide.
#' @param source Texte indiquant la source des donnees.
#' @param couleurs Couleurs du graphique. Voir \code{\link{couleurs_dares}}.
#'
#' @return Un \code{htmlwidget}.
#' @export
#' 
#' @importFrom apexcharter apex ax_yaxis ax_tooltip JS ax_title ax_subtitle
#'
#' @examples
#' 
#' effectifs <- lecture_effectif("inputs/graph_effectifs.csv")
#' 
#' graphique_effectifs(effectifs, "T4Z")
#' 
graphique_effectifs <- function(data, fap_code = NULL, source = NULL,
                                x_min = NULL, x_max = NULL,
                                couleurs = "pantone_bleu") {
  data <- copy(data)
  if (!is.null(fap_code)) {
    data <- filtre_fap(datas = data, fap_code = fap_code)
  } else {
    if (is.null(data$fap))
      stop("'data' foit avoir une colonne appelée 'fap'")
    fap_code <- unique(data$fap)
  }
  if (!is.null(x_min)) {
    data <- data[data$annee >= x_min]
  }
  if (!is.null(x_max)) {
    data <- data[data$annee <= x_max]
  }

  ax <- apex(
    data = data,
    mapping = aes(x = as.Date(paste0(annee, "-01-01")), y = effectif*1000, group = libelle),
    type = "line", serie_name = "Effectif",
    auto_update = FALSE
  ) %>% 
    ax_colors(couleurs_dares(couleurs)) %>% 
    ax_legend(showForSingleSeries = TRUE) %>% 
    # ax_xaxis(min = x_min, max = x_max) %>% 
    ax_yaxis(
      min = 0, forceNiceScale = TRUE,
      title = list(text = ""),
      labels = list(
        formatter = format_num(",.0f", locale = "fr-FR")
      )
    ) %>% 
    ax_tooltip(
      x = list(
        format = "yyyy"
      ), 
      y = list(
        formatter = format_num(",.0f", locale = "fr-FR")#, suffix = " 000")
      )
    )
  if (!is.null(source)) {
    ax <- ax %>% 
      ax_subtitle(
        text = paste0("Source : ", source),
        style = list(
          fontSize = "11px"
        )
      )
  }
  return(ax)
}








