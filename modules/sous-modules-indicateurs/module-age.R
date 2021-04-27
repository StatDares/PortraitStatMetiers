
#  ------------------------------------------------------------------------
#
# Title : Module visualisation indicateur age
#    By : Dares
#  Date : 2020
#
#  ------------------------------------------------------------------------


age_UI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$div(
      class = "container-fluid",
      tags$div(
        class = "col-xs-12 col-sm-12 col-md-10 col-md-offset-1 col-lg-8 col-lg-offset-2",
        tags$h3("Métiers selon les âges", class = "text-center"),
        
        tags$br(),
        
        sliderInput(
          inputId = ns("annee"), label = "Année :",
          min = 1983, max = 2018, sep = "",
          value = 2018, step = 1 ,
          width = "100%"
        ),
        tags$br(),
        uiOutput(outputId = ns("texte_anneeage"), style = "height: 40px"),
        fluidRow(
          column(
            width = 6,
            box_graphique_UI(
              id = ns("top-10-senior"), 
              titre = "Top 10 des métiers avec la plus grande part de seniors (50 ans ou plus)",
              source = "enquêtes Emploi, Insee, données lissées par moyenne mobile d'ordre 3, traitement Dares."
            )
          ),
          column(
            width = 6,
            box_graphique_UI(
              id = ns("top-10-jeune"), 
              titre = "Top 10 des métiers avec la plus grande part de jeunes (moins de 30 ans)",
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


age_Server <- function(id, parent_session) {
  moduleServer(
    id,
    function(input, output, session) {
          
      ns <- session$ns
    
      output$texte_anneeage <- renderUI({
        glue::glue("Pourcentage de seniors ou de jeunes en {as.character(input$annee)}")
      })
    
      observeEvent(input$retour_indicateur, {
        updateNavbarPage(
          session = parent_session, 
          inputId = "main_nav", 
          selected = "indicateurs"
        )
      })
      
      
      data_age  <- reactive({
        dat <- fap_data$graph_age_agrege_1982_1984_a_2017_2019_restructure
        dat_annee <- dat[annee %like% paste0("^", input$annee - 1)]
        dat_annee <- dat_annee[fap %notin% c("ens", "ENS", "Ensemble", "ZZZ", "zzz", "X0Z", "x0z", "T2Z", "t2z")]
        dat_annee <- na.omit(dat_annee)
        dat_annee
      })
      
      
      box_graphique(
        id = "top-10-senior",
        data = reactive(top(subset(data_age(), age == "50 ans ou plus"),"pourcentage",-10)),
        data_all = reactive(top(subset(data_age(), age == "50 ans ou plus"),"pourcentage",-88)),
        download_all = TRUE,
        graphique = reactive({
          data_seniors <- subset(data_age(), age == "50 ans ou plus")
          data_seniors <- top(data_seniors, "pourcentage", -10)
          data_seniors$pourcentage_seniors <- data_seniors$pourcentage/100
          graph_barres(
            data = data_seniors,
            x = libelle, y = pourcentage_seniors,
            verticales = FALSE, titre = "Part de seniors",
            auto_update = config_update(update_options = TRUE)
          )
        })
      )
      
      box_graphique(
        id = "top-10-jeune",
        data = reactive(top(subset(data_age(), age == "Moins de 30 ans"),"pourcentage",-10)),
        data_all = reactive(top(subset(data_age(), age == "Moins de 30 ans"),"pourcentage",-88)),
        download_all = TRUE,
        graphique = reactive({
          data_jeunes <- subset(data_age(), age == "Moins de 30 ans")
          data_jeunes <- top(data_jeunes, "pourcentage", -10)
          data_jeunes$pourcentage_jeunes <- data_jeunes$pourcentage/100
          graph_barres(
            data = data_jeunes,
            x = libelle, y = pourcentage_jeunes,
            verticales = FALSE, titre = "Part de jeunes",
            auto_update = config_update(update_options = TRUE)
          )
        })
      )
    }
  )
}