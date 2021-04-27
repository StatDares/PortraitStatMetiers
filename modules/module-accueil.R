
#  ------------------------------------------------------------------------
#
# Title : Module accueil
#    By : Dares
#  Date : 2020
#
#  ------------------------------------------------------------------------


accueil_UI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$div(
      class = "container-fluid",
      tags$div(
        class = "col-xs-12 col-sm-10 col-sm-offset-1 col-md-10 col-md-offset-1 col-lg-8 col-lg-offset-2",
        tags$br(),
        tags$h2(
          style = "text-align: center; font-weight: bold;",
          "Portraits statistiques des métiers"
        ),
        tags$br(),
        tags$br(),
        
        carousel(
          list(
            src = "images/portrait.png", 
            caption = tagList(
              tags$h3("Portrait des métiers"),
              tags$p(
                "Découvrez le portrait statistique d'un métier",
                "ou d'une famille de métiers selon",
                "de multiples dimensions : évolution, qualité de l'emploi,",
                "caractéristiques des personnes en emploi, etc."
              ),
              actionButton(
                inputId = ns("go_to_metier"), 
                label = tagList(
                  "Découvrir",
                  icon("angle-right")
                ),
                width = "250px"
              )
            )
          ),
          list(
            src = "images/comparateur.png", 
            caption = tagList(
              tags$h3("Comparateur de métiers"),
              tags$p(
                "Comparez le portrait statistique de deux métiers,",
                "ou un métier à un groupe de métiers,",
                "en affichant dans la même visualisation les différentes",
                "dimensions permettant de décrire une famille professionnelle."
              ),
              actionButton(
                inputId = ns("go_to_comparateur"), 
                label = tagList(
                  "Découvrir",
                  icon("angle-right")
                ),
                width = "250px"
              )
            )
          ),
          list(
            src = "images/indicateur.png", 
            caption = tagList(
              tags$h3("Indicateurs"),
              tags$p(
                "Visualisez l'ensemble des familles professionnelles pour un indicateur spécifique."
              ),
              actionButton(
                inputId = ns("go_to_indicateurs"), 
                label = tagList(
                  "Découvrir",
                  icon("angle-right")
                ),
                width = "250px"
              )
            )
          )
        ),
        
        tags$br(),
        tags$p(
          icon("question-circle"), 
          "Obtenez de l'aide et de la documentation sur cette application",
          actionLink(inputId = ns("go_to_doc"), label = "en cliquant ici"), "."
        )
      )
    )
  )
}

accueil_Server <- function(id, parent_session) {
  moduleServer(
    id,
    function(input, output, session) {
      
      observeEvent(input$go_to_metier, {
        updateNavbarPage(
          session = parent_session, 
          inputId = "main_nav", 
          selected = "portrait"
        )
      })
      
      observeEvent(input$go_to_comparateur, {
        updateNavbarPage(
          session = parent_session, 
          inputId = "main_nav", 
          selected = "comparateur"
        )
      })
      
      observeEvent(input$go_to_indicateurs, {
        updateNavbarPage(
          session = parent_session, 
          inputId = "main_nav", 
          selected = "indicateurs"
        )
      })
      
      observeEvent(input$go_to_doc, {
        updateNavbarPage(
          session = parent_session, 
          inputId = "main_nav", 
          selected = "documentation"
        )
      })
    }
  )
}
