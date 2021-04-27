
#' Creation d'un caroussel pour faire defiler des images
#'
#' @param ... Des \code{list} avec deux proprietes: \code{src}
#'  le chemin de l'image, et \code{caption} une legende pour l'image.
#'
#' @return des tags HTML
#' @export
#'
carousel <- function(...) {
  slides <- list(...)
  id <- paste0("carousel", sample.int(1e9, 1))
  tags$div(
    id = id,
    class = "carousel slide",
    `data-ride` = "carousel",
    `data-interval` = "false",
    
    tags$ol(
      class = "carousel-indicators",
      lapply(
        X = seq_along(slides),
        FUN = function(i) {
          tags$li(
            `data-target` = paste0("#", id), 
            `data-slide-to` = i - 1,
            class = if (i == 1) "active"
          )
        }
      )
    ),
    
    tags$div(
      class = "carousel-inner",
      role = "listbox",
      lapply(
        X = seq_along(slides),
        FUN = function(i) {
          tags$div(
            class = "item",
            class = if (i == 1) "active",
            tags$img(src = slides[[i]]$src, style = "margin: auto; height: 550px;"),
            tags$div(
              class = "carousel-caption",
              style = "background: #FFF; color: #818181; border-radius: 15px; border: 1px solid #88cac0; text-shadow: none; padding: 0 20px 30px 20px;",
              slides[[i]]$caption
            )
          )
        }
      )
    ),
    
    tags$a(
      class = "left carousel-control", 
      href = paste0("#", id),
      role="button",
      `data-slide` = "prev",
      icon("chevron-left", lib = "glyphicon")
    ),
    tags$a(
      class = "right carousel-control", 
      href = paste0("#", id),
      role="button",
      `data-slide` = "next",
      icon("chevron-right", lib = "glyphicon")
    )
  )
}




#' Bouton pour revenir en haut de la page
#'
#' @return des tags HTML
#' @export
#'
retour_haut <- function() {
  tags$button(
    class = "btn btn-link pull-right scroll-top",
    "Retour en haut", icon("chevron-up")
  )
}






#' Carte pour afficher dans le menu d'accueil
#'
#' @param titre Titre
#' @param description Une description du contenu. 
#' @param image Lien vers une image.
#' @param bouton_id Id du bonton pour rediriger vers un onglet.
#'
#' @return des tags HTML
#' @export
#'
carte_menu <- function(titre, description, image, bouton_id) {
  tags$div(
    class = "col-xs-12 col-md-4",
    panel(
      style = "height: 300px; position: relative;",
      tags$div(
        style = "text-align: center;",
        tags$img(
          src = image,
          style = "height: 64px;"
        )
      ),
      tags$h4(titre, class = "text-center"),
      tags$br(),
      tags$p(
        description
      ),
      tags$div(
        style = "position: absolute; bottom: 10px; right: 15px; left: 15px;",
        actionButton(
          inputId = bouton_id,
          label = tagList(
            "Découvrir",
            icon("angle-right")
          ),
          class = "btn-block"
        )
      )
    )
  )
}








