
#  ------------------------------------------------------------------------
#
# Title : Portrait metier : dynamique de l'emploi
#    By : DARES
#  Date : 2020-07-17
#
#  ------------------------------------------------------------------------


panel_dynamique_ui <- function(id) {
  ns <- NS(id)
  tabPanel(
    title = "Dynamique de l\'emploi",
    value = "dynamique",
    tags$div(
      class = "box-ombrage",
      br(),
      uiOutput(outputId = ns("range_dynamique"), style = "height: 70px", align="center",),
      box_graphique_UI(
        id = ns("effectif"), 
        titre = "", 
        source = "enqu\u00eates Emploi, Insee, donn\u00e9es liss\u00e9es par moyenne mobile d\'ordre 3, traitement Dares. Par souci de simplification, on désigne dans le graphique les périodes 1982-1984 par 1983 et 2017-2019 par 2018.",
        ombrage = FALSE
      )
    )
  )
}

panel_dynamique <- function(id, r_fap) {
  moduleServer(
    id,
    function(input, output, session) {
      
      box_graphique(
        id = "effectif",
        data = reactive({
          filtre_fap(effectifs, r_fap())
        }),
        graphique = reactive({
          source <- NULL # "enqu\u00eates Emploi, Insee, donn\u00e9es liss\u00e9es par moyenne mobile d\'ordre 3, traitement Dares."
          
          effec <- filtre_fap(effectifs, r_fap())
          effec <- effec[!is.na(effectif)]
          shiny::validate(
            need(nrow(effec) > 0, "D\u00e9sol\u00e9 pas de donn\u00e9es !")
          )
          
          graphique_effectifs(
            data = effec, fap_code = NULL,
            # x_min = input$annees_dynamique[1], x_max = input$annees_dynamique[2],
            source = source
          ) %>% 
            set_input_zoom(inputId = "selection_dynamique", session = session)
        })
      )
      
      
      output$range_dynamique <- renderUI({
        # req(input$selection_dynamique)
        print(input$selection_dynamique)
        effectifs_fap_selected <- filtre_fap(effectifs, r_fap())
        effectifs_fap_selected <- effectifs_fap_selected[!is.na(effectif)]
        
        
        if (!is.null(input$selection_dynamique$x$min)) {
          annee1 <- format(input$selection_dynamique$x$min, format = "%Y")
          annee1 <- as.numeric(annee1)
          annee1_1 <- annee1-1
          annee1_2 <- annee1+1
        } else {
          annee1 <- min(effectifs_fap_selected$annee, na.rm = TRUE)
          annee1_1 <- annee1-1
          annee1_2 <- annee1+1
        }
        if (!is.null(input$selection_dynamique$x$max)) {
          annee2 <- format(input$selection_dynamique$x$max, format = "%Y")
          annee2 <- as.numeric(annee2)
          annee2_1 <- annee2-1
          annee2_2 <- annee2+1
        } else {
          annee2 <- max(effectifs_fap_selected$annee, na.rm = TRUE)
          annee2_1 <- annee2-1
          annee2_2 <- annee2+1
        }
        
        shiny::validate(
          need(nrow(effectifs_fap_selected) > 0, "D\u00e9sol\u00e9 pas de donn\u00e9es !")
        )
        effectifs_fap_year_selected <- effectifs_fap_selected[annee >= annee1 & annee <= annee2]
        eff_annee1 <- head(effectifs_fap_year_selected$effectif, n = 1)
        eff_annee2 <- tail(effectifs_fap_year_selected$effectif, n = 1)
        evolution_periode <- round(((eff_annee2 - eff_annee1)/eff_annee1) * 100)
        duree <- annee2 - annee1
        evolution_annuelle <- (eff_annee2/eff_annee1)^ (1 / duree)-1
        evolution_annuelle <- abs(round(evolution_annuelle*100, 1))
        
        verbe <- if(evolution_periode >= 0) {"a augment\u00e9 de"} else {"a diminu\u00e9 de"}
        tags$h4(
          "Entre", paste(annee1_1,"-",annee1_2,sep=""), "et ", paste(annee2_1,"-",annee2_2,", le nombre de",sep=""),
           tolower(obtenir_libelle(r_fap())),verbe, 
          tags$b(paste0(round(abs(evolution_periode), 1), " %,")),
          " soit en moyenne de ",
          tags$b(paste0(format(evolution_annuelle, decimal.mark = ",")," %")),
          "par an.",align="justify"
        )
      })
      
    }
  )
}


