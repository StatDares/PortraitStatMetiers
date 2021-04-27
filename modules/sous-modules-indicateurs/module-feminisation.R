
#  ------------------------------------------------------------------------
#
# Title : Module visualisation indicateur feminisation des metiers
#    By : Dares
#  Date : 2020
#
#  ------------------------------------------------------------------------


# tableau_part_femme <- fap_data$graph_part_femmes
# tableau_part_femme = tableau_part_femme[,c("libelle", "part_h", "annee")]
# tableau_part_femme <- reshape(tableau_part_femme, 
#                               idvar = "libelle", timevar = "annee",
#                               direction="wide")

feminisation_UI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$div(
      class = "container-fluid",
      tags$div(
        class = "col-xs-12 col-sm-12 col-md-10 col-md-offset-1 col-lg-8 col-lg-offset-2",
        tags$h3("Mixité des métiers", class = "text-center"),
        
        tags$br(),
        
        sliderInput(
          inputId = ns("annee"), label = "Année :",
          min = 1983, max = 2018, sep = "",
          value = 2018, step = 1,
          width = "100%"
        ),

 
        tags$br(),
        uiOutput(outputId = ns("texte_annee"), style = "height: 40px"),
        fluidRow(
          column(
            width = 6,
            box_graphique_UI(
              id = ns("top-10-femme"), 
              titre = "Top 10 des métiers féminins",
              source = "enquêtes Emploi, Insee, données lissées par moyenne mobile d'ordre 3, traitement Dares."
            )
          ),
          column(
            width = 6,
            box_graphique_UI(
              id = ns("top-10-homme"), 
              titre = "Top 10 des métiers masculins",
              source = "enquêtes Emploi, Insee, données lissées par moyenne mobile d'ordre 3, traitement Dares."
            )
          )
        ),
        actionButton(ns("retour_indicateur"), "Retour à l'aperçu"),
        retour_haut()
      )
    )
  )
}


feminisation_Server <- function(id, parent_session) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
    
      output$texte_annee <- renderUI({
        glue::glue("Pourcentage de femmes et d'hommes en {as.character(input$annee)}")
      })
      
      observeEvent(input$retour_indicateur, {
        updateNavbarPage(
          session = parent_session, 
          inputId = "main_nav", 
          selected = "indicateurs"
        )
      })
    
      data_feminisation_annee <- reactive({
        dat <- fap_data$graph_part_femmes
        dat[, part_h := (100 - part)]
        dat_annee <- dat[annee %like% paste0("^", input$annee - 1)]
        na.omit(dat_annee)
      })
      
    
      box_graphique(
        id = "top-10-homme",
        data = reactive(top(data_feminisation_annee(), "part_h", -10)),
        data_all = reactive(top(data_feminisation_annee(), "part_h", -88)),
        download_all=TRUE,
        graphique = reactive({
          data_hommes <- top(data_feminisation_annee(), "part_h", -10)
          data_hommes$part_hommes <- data_hommes$part_h/100
          graph_barres(
            data = data_hommes,
            x = libelle, y = part_hommes,
            verticales = FALSE, titre = "Part d'hommes",
            auto_update = config_update(update_options = TRUE)
          )
        })
      )
    
      box_graphique(
        id = "top-10-femme",
        data = reactive(top(data_feminisation_annee(), "part", -10)),
        data_all = reactive(top(data_feminisation_annee(), "part", -88)),
        download_all=TRUE,
        graphique = reactive({
          data_femmes <- top(data_feminisation_annee(), "part", -10) 
          data_femmes$part_femmes <- data_femmes$part/100
          graph_barres(
            data = data_femmes,
            x = libelle, y = part_femmes,
            verticales = FALSE, titre = "Part de femmes",
            auto_update = config_update(update_options = TRUE)
          )
        })
      )
    }
    
    
  )
}
