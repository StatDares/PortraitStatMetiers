
#  ------------------------------------------------------------------------
#
# Title : Module visualisation indicateur temps partiel et sous-emploi
#    By : Dares
#  Date : 2020
#
#  ------------------------------------------------------------------------


tpsousempl_UI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$div(
      class = "container-fluid",
      tags$div(
        class = "col-xs-12 col-sm-12 col-md-10 col-md-offset-1 col-lg-8 col-lg-offset-2",
        tags$h3("Temps partiel et sous-emploi", class = "text-center"),
        
        tags$br(),
        
        sliderInput(
          inputId = ns("annee"), label = "Année :",
          min = 2004, max = 2018, sep = "",
          value = 2018, step = 1 ,
          width = "100%"
        ),
        tags$br(),
        uiOutput(outputId = ns("texte_annee_tp"), style = "height: 40px"),
        fluidRow(
          column(
            width = 6,
            box_graphique_UI(
              id = ns("top-10-temps-partiel"), 
              titre = "Les 10 métiers avec la plus grande part de personnes a temps partiel",
              source = "enquêtes Emploi, Insee, données lissées par moyenne mobile d'ordre 3, traitement Dares."
            )
          ),
          column(
            width = 6,
            box_graphique_UI(
              id = ns("top-10-sous-emploi"), 
              titre = "Les 10 des métiers avec la plus grande part de personnes en sous-emploi",
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


tpsousempl_Server <- function(id, parent_session) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
    
      output$texte_annee_tp <- renderUI({
        glue::glue("Pourcentage de personnes a temps partiel ou en sous-emploi en {as.character(input$annee)}")
      })
    
      observeEvent(input$retour_indicateur, {
        updateNavbarPage(
          session = parent_session, 
          inputId = "main_nav", 
          selected = "indicateurs"
        )
      })
      
      data_parttempspartiel_annee <- reactive({
        dat <- fap_data$graph_part_tempspartiel
        dat_annee <- dat[annee %like% paste0("^", input$annee - 1)]
        dat_annee <- dat_annee[fap %notin% c("ens", "ENS", "Ensemble", "ZZZ", "zzz","X0Z", "x0z", "T2Z", "t2z")]
        dat_annee <- na.omit(dat_annee)
      })
      
      box_graphique(
        id = "top-10-temps-partiel",
        data = reactive(top(data_parttempspartiel_annee(),"part", -10)),
        data_all = reactive(top(data_parttempspartiel_annee(),"part", -88)),
        download_all = TRUE,
        graphique = reactive({
          data_temps_partiel <- top(data_parttempspartiel_annee(),"part", -10)
          data_temps_partiel$part <- data_temps_partiel$part/100
          graph_barres(
            data = data_temps_partiel,
            x = libelle, y = part,
            verticales = FALSE, titre = "Part de personnes a temps partiel",
            auto_update = config_update(update_options = TRUE)
          )
        })
      )
      
      data_partsousemploi_annee <- reactive({
        dat <- fap_data$graph_part_sousemploi
        dat_annee <- dat[annee %like% paste0("^", input$annee - 1)]
        dat_annee <- dat_annee[fap %notin% c("ens", "ENS", "Ensemble", "ZZZ", "zzz", "X0Z", "x0z", "T2Z", "t2z")]
        dat_annee <- na.omit(dat_annee)
      })
      
      box_graphique( 
        id = "top-10-sous-emploi",
        data = reactive(top(data_partsousemploi_annee(),"part", -10)),
        data_all = reactive(top(data_partsousemploi_annee(),"part", -88)),
        download_all = TRUE,
        graphique = reactive({
          data_sous_emploi <- top(data_partsousemploi_annee(),"part", -10)
          data_sous_emploi$part <- data_sous_emploi$part/100
          graph_barres(
            data = data_sous_emploi,
            x = libelle, y = part,
            verticales = FALSE, titre = "Part de personnes en sous-emploi",
            auto_update = config_update(update_options = TRUE)
          )
        })
      )
    }
  )
}
