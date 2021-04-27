
#  ------------------------------------------------------------------------
#
# Title : Module visualisation indicateur croissance de l'emploi
#    By : Dares
#  Date : 2020
#
#  ------------------------------------------------------------------------


croissance_emp_UI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$div(
      class = "container-fluid",
      tags$div(
        class = "col-xs-12 col-sm-12 col-md-10 col-md-offset-1 col-lg-8 col-lg-offset-2",
        tags$h3("Croissance des métiers", class = "text-center"),
        
        tags$br(),
        
        sliderInput(
          inputId = ns("annees_croissance"), 
          label = "Période :",
          min = 1983, max = 2018, value = c(1983, 2018),
          sep = "", step = 1, width = "100%"
        ),
        tags$br(),
        fluidRow(
          column(
            width = 8,
            uiOutput(outputId = ns("texte_annee"), style = "height: 40px")
          ),
          column(
            width = 4,
            style = "text-align: right;",
            switchInput(
              inputId = ns("echelle"), 
              label = "\u00c9chelle",
              offLabel = "Milliers",
              onLabel = "Pourcentage",
              value = TRUE,
              size = "mini", 
              width = "100%"
            )
          )
        ),
        fluidRow(
          column(
            width = 6,
            box_graphique_UI(
              id = ns("top-10-augmentation"), 
              titre = "Les 10 métiers en plus forte croissance",
              source = "enquêtes Emploi, Insee, données lissées par moyenne mobile d'ordre 3, traitement Dares."
            )
          )
          ,
          column(
            width = 6,
            box_graphique_UI(
              id = ns("top-10-baisse"), 
              titre = "Les 10 métiers en plus forte diminution",
              source = "enquêtes Emploi, Insee, données lissées par moyenne mobile d'ordre 3, traitement Dares."
            )
          )
        ),        
        actionButton(ns('retour_indicateur'), "Retour à l'aperçu"),
        retour_haut()
      )
    )
  )
}


croissance_emp_Server <- function(id, parent_session) {
  moduleServer(
    id,
    function(input, output, session) {
          
      ns <- session$ns
    
      output$texte_annee <- renderUI({
        if (isTRUE(input$echelle)){
          glue::glue(
            "\u00c9volution de l'emploi en pourcentage entre {input$annees_croissance[1]} et {input$annees_croissance[2]}"
          )
        }
        else{
          glue::glue(
            "\u00c9volution de l'emploi en milliers entre {input$annees_croissance[1]} et {input$annees_croissance[2]}"
        )
        }
      })
    
      observeEvent(input$retour_indicateur, {
        updateNavbarPage(
          session = parent_session, 
          inputId = "main_nav", 
          selected = "indicateurs"
        )
      })
      
      data_croissance <- reactive({
        effectifs <- effectifs[nchar(as.character(fap)) == 3]
        effectifs_year_selected <- 
          effectifs[annee == input$annees_croissance[1] | annee == input$annees_croissance[2]]
        effectifs_year_selected <- effectifs_year_selected[fap %notin% c("ens", "ENS", "ZZZ", "zzz", "x0z", "t2z")]
        if (isTRUE(input$echelle)) {
          effectifs_year_selected[, Evolution := ((effectif - shift(effectif, type = "lag")) / shift(effectif, type = "lag")), by = fap]
          effectifs_year_selected[, Evolution := round(Evolution, 2)]
        } else {
          effectifs_year_selected[, Evolution := ((effectif - shift(effectif, type = "lag"))), by = fap]
          effectifs_year_selected[, Evolution := round(Evolution, 0)]
        }
        effectifs_year_selected <- effectifs_year_selected[!is.na(Evolution)]
        return(effectifs_year_selected)
      })
    
      
      box_graphique(
        id = "top-10-augmentation",
        data = reactive(top(data_croissance(), "Evolution", -10)),
        data_all = reactive(top(data_croissance(), "Evolution", -88)),
        download_all = TRUE,
        graphique = reactive({
          if (isTRUE(input$echelle)) {
            titre <- "\u00c9volution en %"
            format_y <- ".0%"
          } else {
            titre <- "\u00c9volution en milliers"
            format_y <- ","
          }
          graph_barres(
            data = top(data_croissance(), "Evolution", -10),
            x = libelle, y = Evolution,
            verticales = FALSE,
            titre = titre,
            format_y = format_y,
            auto_update = config_update(update_options = TRUE)
          )
        })
      )
    
      
      box_graphique(
        id = "top-10-baisse",
        data = reactive(top(data_croissance(), "Evolution", 10)),
        data_all = reactive(top(data_croissance(), "Evolution", 88)),
        download_all = TRUE,
        graphique = reactive({
          if (isTRUE(input$echelle)) {
            titre <- "\u00c9volution en %"
            format_y <- ".0%"
          } else {
            titre <- "\u00c9volution en milliers"
            format_y <- ","
          }
          graph_barres(
            data = top(data_croissance(), "Evolution", 10), 
            x = libelle, y = Evolution,
            verticales = FALSE, 
            titre = titre,
            format_y = format_y,
            auto_update = config_update(update_options = TRUE)
          )
        })
      )
    }
  )
}