
#  ------------------------------------------------------------------------
#
# Title : Module affichage graphiques + telechargement tableau
#    By : Dares
#  Date : 2020
#
#  ------------------------------------------------------------------------


#' Module pour afficher un indicateur
#'
#' @param id Id du module.
#' @param titre Titre du panneau.
#' @param sous_titre Sous-titre du panneau.
#' @param source Ajouter la source des donnees en bas a droite du panneau.
#' @param carte L'indicateur est-il represente sur une carte ?
#' @param tableau Afficher ou pas un bouton pour voir le tableau de donnees.
#' @param ombrage Ajouter un effet d'ombrage.
#' @param class Classe CSS a ajouter au tag HTML.
#' @param height Hauteur du graphique (ou de la carte).
#'
#' @export
#' 
#' @name box-graphique
#'
#' @examples
box_graphique_UI <- function(id, 
                             titre = NULL, 
                             sous_titre = NULL,
                             source = NULL, 
                             carte = FALSE, 
                             tableau = TRUE, 
                             ombrage = TRUE, 
                             class = "tuile-graphique",
                             height = "400px") {
  ns <- NS(id)
  tagList(
    tags$div(
      class = class,
      class = if (isTRUE(ombrage)) "ombrage",
      if (!is.null(titre))
        tags$h5(titre, style = "padding-right: 30px;", id = ns("titre")),
      if (!is.null(sous_titre))
        tags$h6(sous_titre, style = "padding-right: 30px;", id = ns("sous_titre")),
      if (tableau) 
        tagList(
          actionButton(
            inputId = ns("modal_tableau"), 
            label = NULL, 
            icon = icon("table"),
            style = "position: absolute; top: 10px; right: 5px;",
            class = "btn-sm"
          ),
          tags$hr()
        ),
      if (isTRUE(carte)) {
        leafletOutput(outputId = ns("graphique"), height = height)
      } else {
        apexchartOutput(outputId = ns("graphique"), height = height)
      },
      if (!is.null(source))
        tagList(
          tags$br(),
          tags$em("Source :", source, class = "source")
        )
    )
  )
}


#' @param input,output,session Arguments de la fonction server de shiny
#' @param data Une fonction reactive renvoyant les donnees
#' @param graphique Une fonction reactive renvoyant le graphique
#' @param titre_modal Titre de la fenetre affichant le tableau de donnees
#'
#' @export
#' 
#' @rdname box-graphique
#'
box_graphique <- function(id, data, data_all = NULL, graphique, carte = FALSE, download_all = FALSE) {
  moduleServer(
    id,
    function(input, output, session) {
          
      ns <- session$ns
      
      if (isTRUE(carte)) {
        output$graphique <- renderLeaflet({
          shiny::validate(
            need(graphique(), "Désolé, il n'y a pas assez de données pour afficher cette carte.")
          )
          graphique()
        })
      } else {
        output$graphique <- renderApexchart({
          graphique()
        })
      }
      
      if (isTRUE(download_all) & !is.null(data_all)){
        output$tableau <- DT::renderDT({
          dat <- data_all()
          tableau(dat)
        }) 
      } else {
        output$tableau <- DT::renderDT({
        dat <- data()
        tableau(dat)
        })
      }
      
      observeEvent(input$modal_tableau, {
        showModal(modalDialog(
          title = tagList(
            tags$span(id = ns("titre_modal")),
            tags$button(
              icon("close"), 
              class = "btn btn-default pull-right",
              style = "border: 0 none;",
              `data-dismiss` = "modal"
            )
          ),
          size = "l",
          easyClose = TRUE,
          footer = NULL,
          DT::DTOutput(outputId = ns("tableau")),
          tags$br(),
          downloadButton(
            outputId = ns("download_tableau_csv"), 
            label = "Télécharger le tableau en format csv", 
            class = "btn-block"
          ),
          # downloadButton(
          #   outputId = ns("download_tableau_xlsx"), 
          #   label = "Télécharger le tableau en format excel", 
          #   class = "btn-block"
          # ),
          tags$script(sprintf("ajoutTitre('%s');", ns("")))
        ))
      })
  
      
      
      filename_csv <-  function(){
        paste0("data-", Sys.Date(), ".csv")
      }
      filename_xlsx <- function() {
        paste0("data-", Sys.Date(), ".xlsx")
      }
      
      data_to_download <- reactive(
        if (isTRUE(download_all)) {
          if (!is.null(data_all)){
            data_all()
          } else {
            data()
          } 
        } else {
          data()
        }
      )

      output$download_tableau_csv <- downloadHandler(
        filename = filename_csv(),
        content = function(file) {
          data <- data_to_download()
          if (inherits(data, "datatables")) {
            data <- data$x$data
          }
          write.csv(data, file)
        }
      )
      
      output$download_tableau_xlsx <- downloadHandler(
        filename = filename_xlsx(),
        content = function(file) {
          data <- data_to_download()
          if (inherits(data, "datatables")) {
            data <- data$x$data
          }
          write.xlsx(data, file)
        }
      )
      
      }
    
)}
