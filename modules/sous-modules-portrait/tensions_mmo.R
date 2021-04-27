#  ------------------------------------------------------------------------
#
# Title : Portrait metier : tensions ancienne version à partir des MMO
#    By : DARES
#  Date : 2020-07-17
#
#  ------------------------------------------------------------------------


# tensions ----------------------------------------------------------------

panel_tensions_ui <- function(id) {
  ns <- NS(id)
  tabPanel(
    title = "Tensions",
    value = "tension",
    tags$div(
      class = "box-ombrage",
      tags$h3("Tensions"),
      tags$hr(),
      box_graphique_UI(
        id = ns("evol-tensions"), 
        titre = "\u00c9volution des tensions sur le march\u00e9 du travail",
        source = "Dares - P\u00f4le emploi, donn\u00e9es trimestrielles en glissement annuel, traitement Dares.",
        ombrage = FALSE
      ),
      fluidRow(
        column(
          width = 6,
          box_graphique_UI(
            id = ns("entrees-pole-emploi"), 
            titre = "Entr\u00e9es \u00e0 P\u00f4le Emploi des demandeurs d\'emploi de cat\u00e9gories A, B, C",
            source = "Dares - P\u00f4le emploi, traitement Dares.",
            ombrage = FALSE
          )
        ),
        column(
          width = 6,
          box_graphique_UI(
            id = ns("sorties-pole-emploi"), 
            titre = "Sorties de P\u00f4le emploi des demandeurs d\'emploi de cat\u00e9gories A, B, C",
            source = "Dares - P\u00f4le emploi, traitement Dares.",
            ombrage = FALSE
          )
        )
      )
    ),
    retour_haut()
  )
}

panel_tensions <- function(id, r_fap) {
  moduleServer(
    id,
    function(input, output, session) {
      
      box_graphique( 
        id = "evol-tensions",
        data = reactive({
          filtre_fap(fap_data$graph_evol_tensions, c(r_fap(), "Ensemble"))
        }),
        graphique = reactive({
          dat <- filtre_fap(fap_data$graph_evol_tensions, c(r_fap(), "Ensemble"))
          graph_courbes(
            data = dat, 
            x = as.Date(paste0(date, "01"), format = "%Y%m%d"), y = tension * 1000, group = libelle, 
            format_y = ",", couleurs = c("#113263", "#ff887e"), #"#8e9594"),
          )
        })
      )
      
      box_graphique(
        id = "entrees-pole-emploi",
        data = reactive({
          filtre_fap(fap_data$graph_entrees_pole_emploi, c(r_fap(), "Ensemble"))
        }),
        graphique = reactive({
          dat <- filtre_fap(fap_data$graph_entrees_pole_emploi, c(r_fap(), "Ensemble"))
          graph_barres(
            data = dat,
            x =  type_entree, y = pourcentage / 100, group = libelle, 
            verticales = FALSE, couleurs = c("#113263", "#ff887e"), #"#8e9594"),
          ) %>% 
            ax_xaxis(
              categories = wrap_labels(dat$type_entree)
            )
        })
      )
      
      box_graphique( 
        id = "sorties-pole-emploi",
        data = reactive({
          filtre_fap(fap_data$graph_sorties_pole_emploi, c(r_fap(), "Ensemble"))
        }),
        graphique = reactive({
          dat <- filtre_fap(fap_data$graph_sorties_pole_emploi, c(r_fap(), "Ensemble"))
          graph_barres(
            data = dat,
            x =  type_sortie, y = pourcentage / 100, group = libelle, 
            verticales = FALSE, couleurs = c("#113263", "#ff887e"), #"#8e9594"),
          ) %>% 
            ax_xaxis(
              categories = wrap_labels(dat$type_sortie)
            )
        })
      )
      
    }
  )
}