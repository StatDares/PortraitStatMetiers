
#' Bas de page a afficher dans l'application
#'
#' @param version Numero de version de l'application.
#'
bas_de_page <- function(version = "0.0.1") {
  tags$footer(
    class = "footer navbar-bottom",  
    tags$nav(
      class = "navbar navbar-default navbar-dares footer-dares",
      style = "margin-bottom: 0; line-height: 3; height: 50px;",
      tags$div(
        style = "text-align: center; margin-top: 10px;",
        conditionalPanel(
          condition = "input.main_nav != 'accueil' & input.main_nav != 'documentation'",
          bookmarkButton("Générer un lien vers cette page")
        ),
        tags$div(
          HTML("&copy;"), 
          sprintf(
            "Dares - %s - Portrait statistique des m\u00e9tiers v%s", 
            format(Sys.Date(), format = "%Y"), version
          )
        )
      )
    )
  )
}
