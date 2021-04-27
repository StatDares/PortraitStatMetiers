
#  ------------------------------------------------------------------------
#
# Title : Portrait metier : mobilites
#    By : DARES
#  Date : 2020-07-17
#
#  ------------------------------------------------------------------------


panel_mobilites_ui <- function(id) {
  ns <- NS(id)
  tabPanel(
    title = "Mobilit\u00e9s",
    value = "mobilit\u00e9",
    tags$div(
      class = "box-ombrage",
      tags$h3("Mobilit\u00e9s"),
      tags$hr(),
      box_graphique_UI(
        id = ns("evol-mouv-fap"),
        titre = "\u00c9volution des mouvements de main d\'oeuvre", 
        source = NULL,
        ombrage = FALSE,
        height = "300px"
      ),
      box_graphique_UI(
        id = ns("evol-mouv-ens"), 
        titre = NULL,
        source = "DMMO-EMMO, Dares ; Insee, enqu\u00eates Emploi, donn\u00e9es liss\u00e9es par moyenne mobile d\'ordre 3, traitement Dares.",
        ombrage = FALSE,
        tableau = FALSE,
        height = "300px"
      ),
      box_graphique_UI(
        id = ns("part-cdd-embauche"), 
        titre = "Part des CDD dans les embauches",
        source = "DMMO-EMMO, Dares.",
        ombrage = FALSE
      ),
      box_graphique_UI(
        id = ns("type-sortie"), 
        titre = "Type de sorties du m\u00e9tier",
        source = "DMMO-EMMO, Dares.",
        ombrage = FALSE
      ),
      box_graphique_UI(
        id = ns("structure-anciennete"), 
        titre = "Anciennet\u00e9 dans l\'entreprise",
        source = "Source : enqu\u00eate Emploi, Insee, moyenne annuelle sur les ann\u00e9es 1982 \u00e0 1984 et 2017 \u00e0 2019, traitement Dares.",
        ombrage = FALSE
      )
    ),
    retour_haut()
  )
}


panel_mobilites <- function(id, r_fap) {
  moduleServer(
    id,
    function(input, output, session) {
      
      box_graphique(
        id = "evol-mouv-fap",
        data = reactive({
          filtre_fap(fap_data$graph_evol_mmo, c(r_fap(), "Ensemble"))
        }),
        graphique = reactive({
          dat <- filtre_fap(fap_data$graph_evol_mmo, r_fap())
          graph_courbes(
            data = dat,
            x = annee, y = taux, group = type_mmo, 
            format_y = ",", titre = unique(dat$libelle)
          ) %>% 
            ax_chart(
              group = "evol-mouve", id = "fap"
            ) %>% 
            ax_legend(show = FALSE)
        })
      )
      
      box_graphique(
        id = "evol-mouv-ens",
        data = reactive({
          filtre_fap(fap_data$graph_evol_mmo, "Ensemble")
        }),
        graphique = reactive({
          dat <- filtre_fap(fap_data$graph_evol_mmo, "Ensemble")
          graph_courbes(
            data = dat,
            x = annee, y = taux, group = type_mmo, 
            format_y = ",", titre = "Tous m\u00e9tiers"
          ) %>% 
            ax_chart(
              group = "evol-mouve", id = "ensemble"
            )
        })
      )
      
      box_graphique( 
        id = "part-cdd-embauche",
        data = reactive({
          filtre_fap(fap_data$graph_part_CDD_entrees, c(r_fap(), "Ensemble"))
        }),
        graphique = reactive({
          dat <- filtre_fap(fap_data$graph_part_CDD_entrees, c(r_fap(), "Ensemble"))
          graph_barres(
            data = dat, x = annee, y = part / 100, group = libelle,
            couleurs = c("#113263", "#ff887e"), #"#8e9594"),
          ) %>% 
            ax_tooltip(shared = TRUE)
        })
      )
      
      box_graphique(
        id = "type-sortie",
        data = reactive({
          filtre_fap(fap_data$graph_types_sorties, c(r_fap(), "Ensemble"))
        }),
        graphique = reactive({
          dat <- filtre_fap(fap_data$graph_types_sorties, c(r_fap(), "Ensemble"))
          graph_barres(
            data = dat,
            x = type_sortie, y = pourcentage / 100, group = libelle,
            verticales = FALSE, couleurs = c("#113263", "#ff887e"), #"#8e9594"),
          )
        })
      )
      
      box_graphique( 
        id = "structure-anciennete",
        data = reactive({
          filtre_fap(fap_data$graph_anciennete_3niv, r_fap())
        }),
        graphique = reactive({
          dat <- filtre_fap(fap_data$graph_anciennete_3niv, r_fap())
          graph_barres(
            data = dat,
            x = annee,
            y = pourcentage / 100, 
            group = anciennete, 
            verticales = FALSE,
            couleurs = scales::brewer_pal(palette = "Blues")(5)[-1]
          ) %>% 
            ax_chart(
              stacked = TRUE
            ) %>% 
            ax_xaxis(max = 1, tickAmount = 4)
        })
      )
      
    }
  )
}

