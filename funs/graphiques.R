
graph_vide <- function() {
  apexchart(list(
    chart = list(
      type = "bar"
    ),
    series = list(),
    xaxis = list(
      labels = list(show = FALSE)
    ),
    yaxis = list(
      labels = list(show = FALSE)
    ),
    noData = list(
      text = "Pas de données à visualiser pour cet indicateur.",
      style = list(
        fontSize = "15px",
        color = "#818181"
      )
    )
  ), auto_update = FALSE)
}



#' @title Graphique en barres
#' 
#' @description Barres verticales ou horizontales avec ou sans variable de groupe.
#'
#' @param data Un \code{data.frame}.
#' @param x Nom de la variable a utiliser pour l'axe des X.
#' @param y Nom de la variable a utiliser pour l'axe des Y.
#' @param group Nom de la variable a utiliser pour grouper les barres.
#' @param verticales Tracer des barres verticales, si \code{FALSE} barres horizontales.
#' @param titre Titre du graphique.
#' @param couleurs Couleurs du graphique. Voir \code{\link{couleurs_dares}}.
#' @param format_y Format pour l'axe des ordonnees, defaut a pourcentage.
#'
#' @return un \code{htmlwidget}.
#' @export
#'
#' @examples
graph_barres <- function(data,
                         x, y, group = NULL,
                         verticales = TRUE, 
                         titre = NULL,
                         couleurs = c("pantone_bleu", "bleu2"),
                         format_y = ".0%",
                         auto_update = FALSE) {
  if (identical(NROW(data), 0L))
    return(graph_vide())
  if (!is.null(data$libelle) && identical(unique(data$libelle), "Tous métiers"))
    return(graph_vide())
  x_ <- rlang::enquo(x)
  y_ <- rlang::enquo(y)
  group_ <- rlang::enquo(group)
  if (rlang::quo_is_null(group_)) {
    mapping <- aes(x = !!x_, y = !!y_)
  } else {
    mapping <- aes(x = !!x_, y = !!y_, fill = !!group_)
  }
  graph <- apex(
    data = data, 
    auto_update = auto_update,
    mapping = mapping,
    type = if (isTRUE(verticales)) "column" else "bar"
  ) %>% 
    ax_chart(
      defaultLocale = "fr", 
      fontFamily = "Arial, sans-serif"
    )
  
  if (all(couleurs %in% names(couleurs_dares()))) {
    graph <- ax_colors(graph, couleurs_dares(couleurs))
  } else {
    graph <- ax_colors(graph, couleurs)
  }
  
  y_value <- rlang::eval_tidy(rlang::expr(!!y_), data = data)
  pretty_y <- pretty(c(0, max(abs(y_value)))) * sign(max(y_value))
  if (isTRUE(verticales)) {
    graph <- ax_yaxis(
      ax = graph,
      max = max(c(0, pretty_y)),
      min = min(c(pretty_y, 0)),
      tickAmount = length(pretty_y) - 1,
      labels = list(
        formatter = format_num(format_y, locale = "fr-FR")
      )
    )
  } else {
    graph <- ax_xaxis(
      ax = graph,
      max = max(c(0, pretty_y)),
      min = min(c(pretty_y, 0)),
      tickAmount = length(pretty_y) - 1,
      labels = list(
        formatter = format_num(format_y, locale = "fr-FR")
      )
    ) %>% 
      ax_tooltip(y = list(
        formatter = format_num(format_y, locale = "fr-FR")
      ))
  }
  
  if (!is.null(titre)) {
    graph <- ax_title(
      ax = graph,
      text = titre
    )
  }
  
  return(graph)
}



graph_barres_divergentes <- function(data, x, y, group,
                                     titre = NULL,
                                     couleurs = c("pantone_bleu", "bleu2"),
                                     format_y = ".0%",
                                     auto_update = FALSE) {
  if (identical(NROW(data), 0L))
    return(graph_vide())
  x_ <- rlang::enquo(x)
  y_ <- rlang::enquo(y)
  group_ <- rlang::enquo(group)
  if (rlang::quo_is_null(group_)) {
    mapping <- aes(x = !!x_, y = !!y_)
  } else {
    mapping <- aes(x = !!x_, y = !!y_, fill = !!group_)
  }
  graph <- apex(
    data = data, 
    auto_update = auto_update,
    mapping = mapping,
    type = "bar"
  ) %>% 
    ax_chart(
      stacked = TRUE,
      defaultLocale = "fr", 
      fontFamily = "Arial, sans-serif"
    )
  
  if (all(couleurs %in% names(couleurs_dares()))) {
    graph <- ax_colors(graph, couleurs_dares(couleurs))
  } else {
    graph <- ax_colors(graph, couleurs)
  }
  
  y_value <- rlang::eval_tidy(rlang::expr(!!y_), data = data)
  pretty_y <- pretty(c(0, max(abs(y_value)))) * sign(max(y_value))
  graph <- ax_xaxis(
    ax = graph,
    max = max(c(0, pretty_y)),
    min = -max(c(0, pretty_y)),
    tickAmount = (length(pretty_y) - 1) * 2,
    labels = list(
      formatter = JS("function(value) {\n return d3.format('.0%')(Math.abs(value)); \n}")
    )
  ) %>% 
    ax_tooltip(shared = TRUE, y = list(
      formatter = JS("function(value) {\n return d3.format('.0%')(Math.abs(value)); \n}")
    ))
  
  if (!is.null(titre)) {
    graph <- ax_title(
      ax = graph,
      text = titre
    )
  }
  
  return(graph)
}





#' @title Courbes
#' 
#' @description Courbes ou aires avec ou sans variable de groupe.
#'
#' @param data Un \code{data.frame}.
#' @param x Nom de la variable a utiliser pour l'axe des X.
#' @param y Nom de la variable a utiliser pour l'axe des Y.
#' @param group Nom de la variable a utiliser pour dessiner plusieurs courbes.
#' @param aire Tracer l'aire sous la courbe.
#' @param titre Titre du graphique.
#' @param couleurs Couleurs du graphique. Voir \code{\link{couleurs_dares}}.
#' @param epaisseur Epaisseur des traits.
#' @param format_y Format pour l'axe des ordonnees, defaut a pourcentage.
#'
#' @return un \code{htmlwidget}.
#' @export
#'
#' @examples
graph_courbes <- function(data, x, y, group = NULL, 
                          aire = FALSE, titre = NULL, 
                          couleurs = c("pantone_bleu", "bleu2"),
                          epaisseur = c(4, 2),
                          format_y = ".0%") {
  if (identical(NROW(data), 0L))
    return(graph_vide())
  if (!is.null(data$libelle) && identical(unique(data$libelle), "Tous métiers"))
    return(graph_vide())
  x_ <- rlang::enquo(x)
  y_ <- rlang::enquo(y)
  group_ <- rlang::enquo(group)
  if (rlang::quo_is_null(group_)) {
    mapping <- aes(x = !!x_, y = !!y_)
  } else {
    mapping <- aes(x = !!x_, y = !!y_, fill = !!group_)
  }
  
  y_value <- rlang::eval_tidy(rlang::expr(!!y_), data = data)
  y_min <- min(0, min(y_value))
  pretty_y <- pretty(c(y_min, max(y_value)))
  
  graph <- apex(
    data = data,
    mapping = mapping,
    type = if (isTRUE(aire)) "area" else "line", 
    auto_update = FALSE
  ) %>% 
    ax_chart(
      defaultLocale = "fr", 
      fontFamily = "Arial, sans-serif"
    ) %>% 
    ax_stroke(width = epaisseur) %>% 
    ax_tooltip(
      x = list(
        format = "yyyy"
      )
    ) %>% 
    ax_yaxis(
      min = min(pretty_y),
      max = max(pretty_y),
      tickAmount = length(pretty_y) - 1,
      labels = list(
        formatter = format_num(format = format_y, locale = "fr-FR")
      )
    )
  
  if (all(couleurs %in% names(couleurs_dares()))) {
    graph <- ax_colors(graph, couleurs_dares(couleurs))
  } else {
    graph <- ax_colors(graph, couleurs)
  }
  
  if (!is.null(titre)) {
    graph <- ax_title(
      ax = graph,
      text = titre
    )
  }
  
  return(graph)
}






#' Radar
#'
#' @param data Un \code{data.frame}.
#' @param x Nom de la variable a utiliser pour l'axe des X.
#' @param y Nom de la variable a utiliser pour l'axe des Y.
#' @param group Nom de la variable a utiliser pour dessiner plusieurs lignes.
#' @param titre Titre du graphique.
#' @param couleurs Couleurs du graphique. Voir \code{\link{couleurs_dares}}.
#' @param format_y Format pour l'axe des ordonnees, defaut a pourcentage.
#'
#' @return
#' @export
#'
#' @examples
graph_radar <- function(data, x, y, group = NULL, 
                        titre = NULL, 
                        couleurs = c("pantone_bleu", "bleu2"),
                        format_y = ".0%") {
  if (identical(NROW(data), 0L))
    return(graph_vide())
  if (!is.null(data$libelle) && identical(unique(data$libelle), "Tous métiers"))
    return(graph_vide())
  x_ <- rlang::enquo(x)
  y_ <- rlang::enquo(y)
  group_ <- rlang::enquo(group)
  if (rlang::quo_is_null(group_)) {
    mapping <- aes(x = !!x_, y = !!y_)
  } else {
    mapping <- aes(x = !!x_, y = !!y_, fill = !!group_)
  }
  
  x_value <- rlang::eval_tidy(rlang::expr(!!x_), data = data)
  y_value <- rlang::eval_tidy(rlang::expr(!!y_), data = data)
  pretty_y <- pretty(c(0, max(y_value)))
  
  graph <- apex(
    data = data,
    mapping = mapping,
    type = "radar", 
    auto_update = FALSE
  ) %>% 
    ax_chart(
      fontFamily = "Arial, sans-serif"
    ) %>% 
    ax_markers(size = 5) %>% 
    ax_yaxis(
      min = 0,
      max = max(pretty_y),
      tickAmount = length(pretty_y) - 1,
      labels = list(
        formatter = format_num(format_y, locale = "fr-FR"),
        offsetY = 15
      )
    ) %>% 
    ax_xaxis(
      categories = wrap_labels(x_value)
    ) %>% 
    ax_plotOptions(radar = list(
      polygons = list(
        strokeColors = "#D8D8D8",
        connectorColors = "#D8D8D8",
        fill = list(
          colors = c("#FAFAFA", "#fff")
        )
      )
    ))
  
  if (all(couleurs %in% names(couleurs_dares()))) {
    graph <- ax_colors(graph, couleurs_dares(couleurs))
  } else {
    graph <- ax_colors(graph, couleurs)
  }
  
  graph
}






