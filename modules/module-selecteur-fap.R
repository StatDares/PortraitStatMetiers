
#  ------------------------------------------------------------------------
#
# Title : Selecteur FAP
#  Date : 2020-05-15
#
#  ------------------------------------------------------------------------


selecteur_fap_UI <- function(id, arbre_fap, selection = NULL, niveau = "225", multiple = FALSE) {
  ns <- NS(id)
  
  label <- "Sélectionner une ou plusieurs famille(s) de métier"
  if (multiple == FALSE) {
    label <- "Sélectionner une famille de métier"
  }
  
  tagList(
    
    tags$style(".tippy-content {padding: 10px 0px 10px 15px;}"),
    
    dropMenu(
      actionButton(
        inputId = ns("btn_fap"),
        label = label,
        icon = icon("caret-down"),
        class = "btn-block"
      ),
      placement = "bottom",
      maxWidth = "600px",
      padding = 0,
      tags$div(
        style = "height: 80vh; overflow-y: auto;",
        selectizeInput(
          inputId = ns("recherche"), 
          label = "Rechercher un ou plusieurs mots-clefs :",
          choices = NULL,
          multiple = TRUE,
          options = list(
            plugins = list("remove_button"),
            placeholder = "Saisir des mots-clefs"
          ),
          width = "100%"
        ),
        conditionalPanel(
          condition = "output.is_recherche ", 
          ns = ns,
          style = "text-align: right;",
          actionLink(
            inputId = ns("effacer"),
            label = "Effacer la recherche",
            style = "margin-right: 5px;"
          )
        ),
        treeviewInput(
          inputId = ns("fap"), 
          label = NULL,
          choices = arbre_fap, 
          selected = selection,
          multiple = multiple,
          borders = FALSE,
          prevent_unselect = TRUE,
          return_value = "id",
          width = "600px",
          expandIcon = "fa fa-chevron-right",
          collapseIcon = "fa fa-chevron-down",
          wrapNodeText = TRUE,
          selectedBackColor = "#88cac0"
        )
      )
    )
  )
}

selecteur_fap <- function(id,
                          liste_mots_clefs, 
                          table_mots_clefs,
                          table_libelles_fap,
                          multiple = FALSE) {
  moduleServer(
    id,
    function(input, output, session) {

      
      updateSelectizeInput(
        session = session,
        inputId = "recherche",
        choices = liste_mots_clefs,
        server = TRUE
      )
      
      observeEvent(input$effacer, {
        updateSelectizeInput(
          session = session,
          inputId = "recherche",
          selected = character(0)
        )
      })
      
      observeEvent(input$recherche, {
        if (isTruthy(input$recherche)) {
          faps <- table_mots_clefs[keywords %chin% input$recherche, fap]
          libs <- table_libelles_fap[fap %in% faps, libelle]
          if (length(libs) < 1) {
            collapseTreeview("fap")
          } else {
            searchTreeview("fap", paste(libs, collapse = "|"))
          }
        } else {
          collapseTreeview("fap")
        }
      }, ignoreInit = TRUE, ignoreNULL = FALSE)
      
      label <- "Sélectionner une ou plusieurs famille(s) de métier"
      if (multiple == FALSE) {
        label <- "Sélectionner une famille de métier"
      }
      
      output$is_recherche <- reactive({
        return(isTruthy(input$recherche))
      })
      outputOptions(output, "is_recherche", suspendWhenHidden = FALSE)
      
      observe({
        if (isTruthy(input$fap)) {
          updateActionButton(
            session = session,
            inputId = "btn_fap",
            label = table_libelles_fap[fap %in% (input$fap), c(libelle)]
          )
        } else {
          updateActionButton(
            session = session,
            inputId = "btn_fap",
            label = label
          )
        }
      })
      
      if (!multiple){
        observeEvent(input$fap, {
          hideDropMenu("btn_fap_dropmenu")
        })
      }
      
      return(reactive({
        if (isTruthy(input$fap)) {
          input$fap
        } else {
          NULL
        }
      }))
    }
  )
}
