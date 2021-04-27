
#  ------------------------------------------------------------------------
#
# Title : Portrait metier : marche du travail
#    By : DARES
#  Date : 2020-07-17
#
#  ------------------------------------------------------------------------


panel_marche_travail_ui <- function(id) {
  ns <- NS(id)
  tabPanel(
    title = "Demandeurs d'emploi",
    value = "march\u00e9",
    tags$div(
      class = "box-ombrage",
      tags$h3("Demandeurs d'emploi"),
      tags$hr(),
      box_graphique_UI(
        id = ns("demandeurs-emploi"), 
        titre = "Demandeurs d\'emploi",
        sous_titre = uiOutput(outputId = ns("phrase_part_un_an_ou_plus"), inline = TRUE),
        source = "Dares - P\u00f4le Emploi, traitement Dares. Effectifs annuels moyens (moyenne des effectifs des quatre trimestres).",
        ombrage = FALSE
      ),
      fluidRow(
        column(
          width = 6,
          box_graphique_UI(
            id = ns("age-demandeurs-emploi"), 
            titre = "Structure par \u00e2ge des demandeurs d\'emploi de cat\u00e9gorie A en 2020",
            source = "Dares - P\u00f4le Emploi, traitement Dares. Effectifs annuels moyens (moyenne des effectifs des quatre trimestres).",
            ombrage = FALSE
          )
        ),
        column(
          width = 6,
          box_graphique_UI(
            id = ns("diplome-demandeurs-emploi"), 
            titre = "Niveau de dipl\u00f4me des demandeurs d\'emploi de cat\u00e9gorie A en 2020",
            source = "Dares - P\u00f4le Emploi, traitement Dares. Effectifs annuels moyens (moyenne des effectifs des quatre trimestres).",
            ombrage = FALSE
          )
        )
      ),
      box_graphique_UI(
            id = ns("part-cat-a-reg"),
            titre = "Part du m\u00e9tier parmi les demandeurs d\'emploi de cat\u00e9gorie A par r\u00e9gion en 2020",
            carte = TRUE, 
            source = "Dares - P\u00f4le Emploi, traitement Dares. Effectifs annuels moyens (moyenne des effectifs des quatre trimestres).",
            ombrage = FALSE
          ),
      box_graphique_UI(
            id = ns("part-cat-abc-reg"),
            titre = "Part du m\u00e9tier parmi les demandeurs d\'emploi de cat\u00e9gorie A, B et C par r\u00e9gion en 2020",
            carte = TRUE, 
            source = "Dares - P\u00f4le Emploi, traitement Dares. Effectifs annuels moyens (moyenne des effectifs des quatre trimestres).",
            ombrage = FALSE
          )
    ),
    retour_haut()
  )
}


panel_marche_travail <- function(id, r_fap) {
  moduleServer(
    id,
    function(input, output, session) {
      
      output$phrase_part_un_an_ou_plus <- renderUI({
        effectif_DE_A <- filtre_fap(fap_data$graph_serie_demandeurs_emploi_A_ABC, r_fap())
        part_un_an <- filtre_fap(fap_data$part_femmes_un_an_demandeurs_emploi_A, r_fap())
        part_femmes_DE_A <- filtre_fap(fap_data$part_femmes_un_an_demandeurs_emploi_A, r_fap())
        if (nrow(effectif_DE_A) >= 1 & nrow(part_un_an) >= 1) {
        effectif_DE_A <- effectif_DE_A[categorie == "Catégorie A"]
        effectif_DE_A <- effectif_DE_A[order(annee, decreasing = TRUE), c(effectifs)][1]
        part_un_an <- part_un_an[, c(part_defm_1an_ou_plus)][1]
        part_femmes_DE_A <- part_femmes_DE_A[, c(part_femmes_a)][1]
        tags$p("En 2020, le nombre de demandeurs d’emploi de catégorie A à la recherche d'un métier parmi les ",
               tolower(obtenir_libelle(r_fap())) ,"est de ", round(1000*effectif_DE_A),
               " personnes. Parmi eux, ",round(part_un_an), "% sont inscrits depuis un an ou plus et",
               round(part_femmes_DE_A), "% sont des femmes.")
        }
        })
      
      box_graphique( 
        id = "demandeurs-emploi",
        data = reactive({
          filtre_fap(fap_data$graph_serie_demandeurs_emploi_A_ABC, r_fap())
        }),
        graphique = reactive({
          dat <- filtre_fap(fap_data$graph_serie_demandeurs_emploi_A_ABC, r_fap())
          graph_courbes(
            data = dat, 
            x = annee, y = effectifs * 1000, group = categorie, 
            format_y = ",", epaisseur = c(3, 3)
          )
        })
      )
      
      box_graphique( 
        id = "age-demandeurs-emploi",
        data = reactive({
          filtre_fap(fap_data$graph_age_demandeur_emploi, c(r_fap(), "Ensemble"))
        }),
        graphique = reactive({
          dat <- filtre_fap(fap_data$graph_age_demandeur_emploi, c(r_fap(), "Ensemble"))
          graph_barres(
            data = dat,
            x = age, y = part / 100, group = libelle, 
            couleurs = c("#113263", "#ff887e"), #"#8e9594")
          ) #%>% 
            #ax_dataLabels(enabled = TRUE, formatter = format_num(".0%"))
        })
      )
      
      box_graphique(
        id = "diplome-demandeurs-emploi",
        data = reactive({
          filtre_fap(fap_data$graph_dipl_demandeurs_emploi, c(r_fap(), "Ensemble"))
        }),
        graphique = reactive({
          dat <- filtre_fap(fap_data$graph_dipl_demandeurs_emploi, c(r_fap(), "Ensemble"))
          ordonne_niv_dipl(dat)
          graph_radar(
            data = dat, 
            x = diplome, y = part / 100, group = libelle,
            couleurs = c("#113263", "#ff887e"), #"#8e9594")
          )
        })
      )
      
      box_graphique( 
        id = "part-cat-a-reg",
        data = reactive({
          filtre_fap(fap_data$input_carte2, r_fap())
        }),
        graphique = reactive({
          dat <- fap_data$input_carte2
          dat <- filtre_fap(dat, r_fap())
          choroplethe(dat, variable = "variable", val_absolue = "emp_fap", titre_legende = "Part en %")
        }),
        carte = TRUE
      )
      
      box_graphique(
        id = "part-cat-abc-reg",
        data = reactive({
          filtre_fap(fap_data$input_carte3, r_fap())
        }),
        graphique = reactive({
          dat <- fap_data$input_carte3
          dat <- filtre_fap(dat, r_fap())
          choroplethe(dat, variable = "variable", val_absolue = "emp_fap", titre_legende = "Part en %")
        }),
        carte = TRUE
      )
      
    }
  )
}
