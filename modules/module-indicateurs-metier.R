
#  ------------------------------------------------------------------------
#
# Title : Module indicateurs
#    By : Dares
#  Date : 2020
#
#  ------------------------------------------------------------------------


indicateurs_UI <- function(id) {
  ns <- NS(id)

  panel_tension <- carte_menu(
    titre = "Les tensions par métiers",
    description = tagList(
      "Toutes les données sur les tensions par métiers et départements"
    ),
    image = "images/stress.svg",
    bouton_id = ns("go_to_tensions")
  )
  
  
    tagList(
    tags$div(
      class = "container-fluid",
      tags$div(
        class = "col-xs-12 col-sm-10 col-sm-offset-1 col-md-10 col-md-offset-1 col-lg-8 col-lg-offset-2",
        tags$br(),
        tags$h2(
          style = "text-align: center; font-weight: bold;",
          "Approche par indicateurs"
        ),
        tags$br(),
        tags$div(
          class = "container-fluid",
          carte_menu(
            titre = "La mixité des métiers",
            description = tagList(
              "Quels sont les métiers où il y a le plus d'hommes ? de femmes ?"
            ),
            image = "images/women.svg",
            bouton_id = ns("go_to_feminisation")
          ),
          carte_menu(
            titre = "Les métiers en croissance",
            description = tagList(
              "Quels sont les métiers les plus en croissance ? les plus en baisse ?",
              "Comment cela évolue dans le temps"
            ),
            image = "images/growth.svg",
            bouton_id = ns("go_to_croissance_emp")
          ),
          carte_menu(
            titre = "Les métiers par âge",
            description = tagList(
              "Quels sont les métiers avec le plus de seniors ? le plus de jeunes ?",
              "et comment cela évolue avec le temps"
            ),
            image = "images/age.svg",
            bouton_id = ns("go_to_age")
          ),
          carte_menu(
            titre = "Temps partiel et sous-emploi",
            description = tagList(
              "Quels sont les métiers où le temps partiel est le plus fréquent ? avec le plus de sous-emploi ?"
            ),
            image = "images/work.svg",
            bouton_id = ns("go_to_tpsousempl")
          ),
          carte_menu(
            titre = "CDD et interim",
            description = tagList(
              "Quels sont les métiers où les CDD sont les contrats les plus fréquents ? avec le plus de personnes en interim ?"
            ),
            image = "images/curriculum.svg",
            bouton_id = ns("go_to_cddinterim")
          ),
          
          # Panel tension
          panel_tension
          ###############
          
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

indicateurs_Server <- function(id, parent_session) {
  moduleServer(
    id,
    function(input, output, session) {
          
      observeEvent(input$go_to_feminisation, {
        updateNavbarPage(
          session = parent_session,
          inputId = "main_nav",
          selected = "feminisation"
        )
      })  
      
      observeEvent(input$go_to_croissance_emp, {
        updateNavbarPage(
          session = parent_session,
          inputId = "main_nav",
          selected = "croissance_emp"
        )
      })
      
      observeEvent(input$go_to_age, {
        updateNavbarPage(
          session = parent_session,
          inputId = "main_nav",
          selected = "age"
        )
      })
      
      observeEvent(input$go_to_tpsousempl, {
        updateNavbarPage(
          session = parent_session,
          inputId = "main_nav",
          selected = "tpsousempl"
        )
      })
      
      observeEvent(input$go_to_cddinterim, {
        updateNavbarPage(
          session = parent_session,
          inputId = "main_nav",
          selected = "cddinterim"
        )
      })
      
      observeEvent(input$go_to_tensions, {
        updateNavbarPage(
          session = parent_session,
          inputId = "main_nav",
          selected = "ind-tensions"
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
