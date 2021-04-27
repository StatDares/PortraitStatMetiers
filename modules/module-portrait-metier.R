
#  ------------------------------------------------------------------------
#
# Title : Module portrait metier
#    By : Dares
#  Date : 2020
#
#  ------------------------------------------------------------------------

phrase_intro_portrait <- tags$div(
  HTML(paste(tags$strong(tags$span(style="color:darkblue","Vous pouvez sélectionner un métier (à l'aide du menu déroulant et de la recherche par mots-clefs)")))))

portrait_metier_UI <- function(id, fap, selected_item=NULL) {
  ns <- NS(id)

  tagList(
    tags$h3("Portrait des m\u00e9tiers", class = "text-center"),
    tags$div(
      class = "container-fluid",
      tags$div(
        tags$h5(phrase_intro_portrait, class = "col-xs-12 col-sm-12 col-md-10 col-md-offset-1 col-lg-10 col-lg-offset-2"),
        class = "col-xs-12 col-sm-12 col-md-10 col-md-offset-1 col-lg-8 col-lg-offset-2",
        
        tags$br(),
        fluidRow(
          column(
            width = 10, offset = 2,
            selecteur_fap_UI(
              ns("fap"),
              arbre_fap = arbre_fap, 
              selection = fap
            )
          )
        ),
        tags$br(),
        
        if (version_prod) {
          navlistPanel(
            id = ns("nav_portrait"),
            selected = selected_item,
            widths = c(2, 10),
            panel_description_ui(ns("description")),
            panel_dynamique_ui(ns("dynamique")),
            panel_caracteristiques_ui(ns("caracteristiques")),
            panel_qualite_ui(ns("qualite")),
            panel_employeur_ui(ns("employeur")),
            panel_marche_travail_ui(ns("marche_travail")),
            panel_tensions_departement_ui(ns("tensions_departement"))
          )
        }
        else {
          navlistPanel(
            id = ns("nav_portrait"),
            selected = selected_item,
            widths = c(2, 10),
            panel_description_ui(ns("description")),
            panel_dynamique_ui(ns("dynamique")),
            panel_caracteristiques_ui(ns("caracteristiques")),
            panel_qualite_ui(ns("qualite")),
            panel_employeur_ui(ns("employeur")),
            panel_mobilites_ui(ns("mobilites")),
            panel_marche_travail_ui(ns("marche_travail")),
            panel_tensions_ui(ns("tensions")),
            panel_tensions_departement_ui(ns("tensions_departement"))
            
          )
        }
      )
    )
  )
}

onglets_portait = c("description", "dynamique", "caracteristiques", "qualit\u00e9", "employeurs", "mobilit\u00e9", "march\u00e9", "tension", "immigr\u00e9s")

portrait_metier_Server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      # Selecteur FAP ----
      
      r_fap <- selecteur_fap( 
        id = "fap",
        liste_mots_clefs = list_appellations,
        table_mots_clefs = appellations_recherche,
        table_libelles_fap = libs_fap
      )
      
      
      # Description ----
      panel_description("description", r_fap = r_fap)
    
      
      # Dynamique de l'emploi ----
      
      panel_dynamique("dynamique", r_fap = r_fap)
    
      
      # Caracteristiques des personnes en emploi ----
      
      panel_caracteristiques("caracteristiques", r_fap = r_fap)
      
      
      # Qualite de l'emploi ----
      
      panel_qualite("qualite", r_fap = r_fap)
      
      
      # Caractéristiques des employeurs ----
      
      panel_employeur("employeur", r_fap = r_fap)
      
      
      # Mobilite ----
      
      panel_mobilites("mobilites", r_fap = r_fap)
      
      
      # Marche du travail ----
      
      panel_marche_travail("marche_travail", r_fap = r_fap)
      
      
    
      # Tensions ----
      
      panel_tensions("tensions", r_fap = r_fap)
      
      
      # Tensions (par départements) ----
      
      panel_tensions_departement("tensions_departement", r_fap = r_fap)
      
      
      
    }
  )
}
