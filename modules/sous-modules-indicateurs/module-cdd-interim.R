
#  ------------------------------------------------------------------------
#
# Title : Module visualisation indicateur CDD/interim
#    By : Dares
#  Date : 2020
#
#  ------------------------------------------------------------------------



cddinterim_UI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$div(
      class = "container-fluid",
      tags$div(
        class = "col-xs-12 col-sm-12 col-md-10 col-md-offset-1 col-lg-8 col-lg-offset-2",
        tags$h3("CDD et interim", class = "text-center"),
        
        tags$br(),
        
        sliderInput(
          inputId = ns("annee"), label = "Année :",
          min = 1983, max = 2018, sep = "",
          value = 2018, step = 1,
          width = "100%"
        ),
        tags$br(),
        uiOutput(outputId = ns("texte_annee_cddinterim"), style = "height: 40px"),
        fluidRow(
          column(
            width = 6,
            box_graphique_UI(
              id = ns("top-10-cdd"), 
              titre = "Les 10 métiers avec la plus grande part de CDD",
              source = "enquêtes Emploi, Insee, données lissées par moyenne mobile d'ordre 3, traitement Dares."
            )
          ),
          column(
            width = 6,
            box_graphique_UI(
              id = ns("top-10-interim"), 
              titre = "Les 10 métiers avec la plus grande part de personnes en intérim",
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


cddinterim_Server <- function(id, parent_session) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
    
      output$texte_annee_cddinterim <- renderUI({
        glue::glue("Pourcentage de personnes en CDD ou en interim en {as.character(input$annee)}")
      })
    
      observeEvent(input$retour_indicateur, {
        updateNavbarPage(
          session = parent_session, 
          inputId = "main_nav", 
          selected = "indicateurs"
        )
      })
      
      data_partcdd_annee <- reactive({
        dat <- fap_data$graph_part_cdd
        dat_annee <- dat[annee %like% paste0("^", input$annee - 1)]
        dat_annee <- dat_annee[fap %notin% c("ens", "ENS", "Ensemble", "ZZZ", "zzz", "X0Z", "x0z", "T2Z", "t2z")]
        dat_annee <- na.omit(dat_annee)
      })
      
      box_graphique(
        id = "top-10-cdd",
        data = reactive(top(data_partcdd_annee(), "part", -10)),
        data_all = reactive(top(data_partcdd_annee(), "part", -88)),
        download_all = TRUE,
        graphique = reactive({
          data_cdd <- top(data_partcdd_annee(), "part", -10)
          data_cdd$part <- data_cdd$part /100
          graph_barres(
            data = data_cdd, 
            x = libelle, y = part,
            verticales = FALSE, titre = "Part de personnes en CDD",
            auto_update = config_update(update_options = TRUE)
          )
        })
      )
      
      data_partinterim_annee <- reactive({
        dat <- fap_data$graph_part_interim
        dat_annee <- dat[annee %like% paste0("^", input$annee - 1)]
        dat_annee <- dat_annee[fap %notin% c("ens", "ENS", "Ensemble", "ZZZ", "zzz", "X0Z", "x0z", "T2Z", "t2z")]
        dat_annee <- na.omit(dat_annee)
      })
      
      
      box_graphique(
        id = "top-10-interim",
        data = reactive(top(data_partinterim_annee(), "part", -10)),
        data_all = reactive(top(data_partinterim_annee(), "part", -88)),
        download_all = TRUE,
        graphique = reactive({
          data_interim <- top(data_partinterim_annee(), "part", -10)
          data_interim$part <- data_interim$part/100
          graph_barres(
            data = data_interim, 
            x = libelle, y = part,
            verticales = FALSE, titre = "Part de personnes en interim",
            auto_update = config_update(update_options = TRUE)
          )
        })
      )
    }
  )
}
    