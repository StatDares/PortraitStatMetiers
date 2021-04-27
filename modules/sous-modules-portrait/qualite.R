
#  ------------------------------------------------------------------------
#
# Title : Portrait metier : qualite de l'emploi
#    By : DARES
#  Date : 2020-07-17
#
#  ------------------------------------------------------------------------


panel_qualite_ui <- function(id) {
  ns <- NS(id)
  tabPanel(
    title = "Qualit\u00e9 de l\'emploi",
    value = "qualit\u00e9",
    tags$div(
      class = "box-ombrage",
      tags$h3("Qualit\u00e9 de l\'emploi"),
      tags$hr(),
      fluidRow(
        column(
          width = 6,
          box_graphique_UI(
            id = ns("structure-statut"), 
            titre = "\u00c9volution de la structure par statut d'emploi",
            sous_titre = uiOutput(outputId = ns("phrase_part_statut"), inline = TRUE),
            source = "enqu\u00eate Emploi, Insee, moyenne annuelle sur les ann\u00e9es 1982 \u00e0 1984 ou 2003 \u00e0 2005 et 2017 \u00e0 2019, traitement Dares.",
            ombrage = FALSE
          )
          
        ),
        column(
          width = 6,
          box_graphique_UI(
            id = ns("horaires-atypiques"),
            titre = HTML("Horaires de travail &laquo; atypiques &raquo;"), 
            sous_titre = uiOutput(outputId = ns("phrase_part_horaires_atypiques"), inline = TRUE),
            source = "enqu\u00eate Emploi, Insee, moyenne annuelle sur les ann\u00e9es 2017 \u00e0 2019, traitement Dares.",
            ombrage = FALSE
          )
        )
      ),
      fluidRow(
        column(
        width = 6,
          box_graphique_UI(
            id = ns("salaire-mensuel"), 
            titre = "Salaire mensuel net",
            sous_titre = uiOutput(outputId = ns("phrase_tranches_salaire"), inline = TRUE),
            source = "enqu\u00eate Emploi, Insee, moyenne annuelle sur les ann\u00e9es 2017 \u00e0 2019, traitement Dares. Champ : salari\u00e9 \u00e0 temps complet hors apprentis et stagiaires.",
            ombrage = FALSE
          )
        ),
        column(
          width = 6,
          box_graphique_UI(
            id = ns("salaire-median"), 
            titre = "\u00c9volution du salaire mensuel net m\u00e9dian",
            sous_titre = uiOutput(outputId = ns("phrase_part_salarie_hors_apprentis"), inline = TRUE),
            source = "enqu\u00eate Emploi, Insee, moyenne annuelle sur les ann\u00e9es 2003 \u00e0 2005 et 2017 \u00e0 2019, traitement Dares. Champ salari\u00e9 \u00e0 temps complet hors apprentis et stagiaires.",
            ombrage = FALSE
          )
        )
      ),
      fluidRow(
        box_graphique_UI(
            id = ns("temps-travail"),
            titre = HTML("Temps de travail et sous-emploi"), 
            sous_titre = uiOutput(outputId = ns("phrase_part_temps_travail"), inline = TRUE),
            source = "enqu\u00eate Emploi, Insee, moyenne annuelle sur les ann\u00e9es 2017 \u00e0 2019, traitement Dares. 
            Les personnes en sous-emploi au sens du BIT sont celles qui occupent un emploi à temps partiel, 
            souhaitent travailler plus d’heures sur une semaine donnée, et sont disponibles pour le faire, qu’elles recherchent un emploi ou non,
            ainsi que les personnes ayant involontairement travaillé moins que d’habitude.",
            ombrage = FALSE
        )
      )
    ),
    retour_haut()
  )
}



panel_qualite <- function(id, r_fap) {
  moduleServer(
    id,
    function(input, output, session) {
      
      output$phrase_part_statut <- renderUI({
        texte_statut <- c(
          "Apprentis" = " sont apprentis.",
          "Contrats à durée déterminée" = " ont un contrat à durée déterminée.",
          "Contrats ou emplois à durée indéterminée" = " ont un contrat ou emploi à durée indéterminée.",
          "Intérimaires" = " sont intérimaires.",
          "Non-salariés, chefs d'entreprise" = " sont non-salariés ou chefs d'entreprise."
        )
        structure_statut <- filtre_fap(fap_data$graph_tab5statut_3niv, r_fap())
        #structure_statut_deb <- structure_statut[annee == "2003-2005", c(statut,pourcentage)]
        structure_statut_fin <- structure_statut[annee == "2017-2019",]
        part_statut_2018_1 <- structure_statut_fin[order(pourcentage, decreasing = TRUE), c(pourcentage)][1]
        nom_statut_2018_1 <- structure_statut_fin[order(pourcentage, decreasing = TRUE), c(statut)][1]
        tags$p("En 2017-2019, ",tags$b(round(part_statut_2018_1), " %"),
               paste0(" des ", tolower(obtenir_libelle(r_fap())),
                      texte_statut[[nom_statut_2018_1]]))
        })
      
      box_graphique( 
        id = "structure-statut",
        data = reactive({
          filtre_fap(fap_data$graph_tab5statut_3niv, r_fap())
        }),
        graphique = reactive({
          dat <- filtre_fap(fap_data$graph_tab5statut_3niv, r_fap())
          graph_barres(
            data = dat,
            x = annee,
            y = pourcentage / 100, 
            group = statut, 
            verticales = FALSE,
            couleurs = scales::brewer_pal(palette = "Blues")(6)[-1]
          ) %>% 
            ax_chart(
              stacked = TRUE
            ) %>% 
            ax_xaxis(max = 1, tickAmount = 4)
        })
      )
      
      output$phrase_part_horaires_atypiques <- renderUI({
        hor_atyp <- filtre_fap(fap_data$graph_horaires_atypiques_3niv, r_fap())
        tra_dim <- hor_atyp[horaire_atypique == "Travail le dimanche", c(pourcentage)]
        tra_sam <- hor_atyp[horaire_atypique == "Travail le samedi", c(pourcentage)]
        tra_nuit <- hor_atyp[horaire_atypique == "Travail de nuit", c(pourcentage)]
        if (is.na(tra_sam)){tags$p("")} else {tags$p("Dans cette profession, ",
                      tags$b(round(tra_sam), " %")," des personnes travaillent le samedi, ",
                      tags$b(round(tra_dim), " %")," le dimanche, et ",
                      tags$b(round(tra_nuit), " %")," de nuit. ",
                      align="justify")}
      })
      box_graphique(
        id = "horaires-atypiques",
        data = reactive({
          filtre_fap(fap_data$graph_horaires_atypiques_3niv, c(r_fap(), "Ensemble"))
        }),
        graphique = reactive({
          dat <- filtre_fap(fap_data$graph_horaires_atypiques_3niv, c(r_fap(), "Ensemble"))
          graph_barres(
            data = dat[!is.na(pourcentage)],
            x = horaire_atypique,
            y = pourcentage / 100, 
            group = libelle,
            couleurs = c("#113263", "#ff887e"), #"#8e9594"),
            verticales = TRUE
          ) %>% 
            ax_dataLabels(
              enabled = TRUE, 
              formatter = format_num(".0%")
            ) %>% 
            ax_xaxis(labels = list(trim = TRUE))
        })
      )
      
      output$phrase_tranches_salaire <- renderUI({
        effectifs_2018 <- filtre_fap(effectifs, r_fap())
        effectifs_2018 <- effectifs_2018[annee == 2018,c(effectif)]
        effectifs_salaries_2018 <- filtre_fap(fap_data$effectifsalarie_hors_app_3niv, r_fap())
        effectifs_salaries_2018 <- effectifs_salaries_2018[,c(sumeffsal)]
        part_salarie_hors_app <- effectifs_salaries_2018/effectifs_2018*100
        sal_mensuel <- filtre_fap(fap_data$graph_salaires_3niv, r_fap())
        if (nrow(sal_mensuel) >= 1){
        sal_mensuel_1250 <- sal_mensuel[salaire == "Moins de 1250 €", c(pourcentage)]
        sal_mensuel_3000 <- sal_mensuel[salaire == "3000 € ou plus", c(pourcentage)]
        }
        if (nrow(sal_mensuel) >= 1){
          tags$p(
          "Parmi les salari\u00e9s \u00e0 temps complet hors apprentis de ce m\u00e9tier, dont la part est de ",
           paste0(round(part_salarie_hors_app), " % en 2017-2019,"),
           tags$b(round (sal_mensuel_1250)," %"),
               " d\u00e9clarent gagner moins de 1 250 € nets par mois, et ",
           tags$b(round (sal_mensuel_3000), " %"), " d\u00e9clarent gagner plus de 3 000 €.",
          align = "justify")
        } else {tags$p("")}
      })
      
      box_graphique( 
        id = "salaire-mensuel",
        data = reactive({
          dat <- filtre_fap(fap_data$graph_salaires_3niv, r_fap())
          ordonne_salaire(dat)
        }),
        graphique = reactive({
          dat <- filtre_fap(fap_data$graph_salaires_3niv, r_fap())
          ordonne_salaire(dat)
          graph_barres(
            data = dat,
            x = salaire,
            y = pourcentage / 100,
            group = libelle,
            couleurs = c("#113263", "#ff887e"), #"#8e9594"), 
            verticales = FALSE
          ) %>% 
            ax_dataLabels(
              enabled = TRUE, 
              formatter = format_num(".0%")
            ) %>% 
            ax_xaxis(
              categories = wrap_labels(dat$salaire)
            ) %>% 
            ax_legend(show = FALSE)
        })
      )
      
      output$phrase_part_salarie_hors_apprentis <- renderUI({
        effectifs_2018 <- filtre_fap(effectifs, r_fap())
        effectifs_2018 <- effectifs_2018[annee == 2018,c(effectif)]
        effectifs_salaries_2018 <- filtre_fap(fap_data$effectifsalarie_hors_app_3niv, r_fap())
        effectifs_salaries_2018 <- effectifs_salaries_2018[,c(sumeffsal)]
        sal_net_median <- filtre_fap(fap_data$salaire_median_3niv, r_fap())
        if (nrow(sal_net_median) >= 1){
          sal_net_median <- filtre_fap(fap_data$salaire_median_3niv, r_fap())
          sal_net_median_deb <- sal_net_median[annee == "2003-2005", c(salaire_median)]
          sal_net_median_fin <- sal_net_median[annee == "2017-2019", c(salaire_median)]
          
          duree_sal <- 2018 - 2004
          evolution_sal_med <- round(((sal_net_median_fin - sal_net_median_deb)/sal_net_median_deb) * 100)
          
          evolution_sal_med_annuelle <- ((sal_net_median_fin/sal_net_median_deb)^ (1 / duree_sal))-1
          evolution_sal_med_annuelle <- abs(round(evolution_sal_med_annuelle*100, 1))
          evolution_sal_med_annuelle_inflation <- (((sal_net_median_fin/inflation$base_1991[inflation$Annee == "2018"])/(sal_net_median_deb/inflation$base_1991[inflation$Annee == "2004"]))^ (1 / duree_sal))-1
          evolution_sal_med_annuelle_inflation_valeur_absolue <- abs(round(evolution_sal_med_annuelle_inflation*100, 1))
          
          verbe_sal_med <- if(evolution_sal_med >= 0) {"a augment\u00e9 de"} else {"a diminu\u00e9 de"}
          verbe_sal_med_inflation <- if(evolution_sal_med_annuelle_inflation >= 0) {"a augment\u00e9 de"} else {"a diminu\u00e9 de"}
        }
        part_salarie_hors_app <- effectifs_salaries_2018/effectifs_2018*100
        if (nrow(sal_net_median) >= 1){
        tags$p("Parmi les salari\u00e9s de ce m\u00e9tier,",
               "le salaire net médian \u00e0 temps complet s'élève \u00e0 ",
                tags$b(format(as.numeric(sal_net_median_fin), big.mark = " ", digits = 0, scientific = FALSE),
                      " €"),
                paste0(" par mois en 2017-2019. Leur salaire ",verbe_sal_med),
                paste0(round(abs(evolution_sal_med), 1), " % entre 2003-2005 et 2017-2019, soit en moyenne de "),
                paste0(format(evolution_sal_med_annuelle, decimal.mark = ","), " %"),
                "par an. En tenant compte de l'inflation, leur salaire ",
                verbe_sal_med_inflation, tags$b(format(evolution_sal_med_annuelle_inflation_valeur_absolue, decimal.mark = ",")," %")," en moyenne annuelle.",
               align="justify"
               )} else {tags$p("")}
      })
      
      box_graphique( 
        id = "salaire-median",
        data = reactive({
          filtre_fap(fap_data$salaire_median_3niv, c(r_fap(), "Ensemble"))
        }),
        graphique = reactive({
          dat <- filtre_fap(fap_data$salaire_median_3niv, c(r_fap(), "Ensemble"))
          graph_barres(
            data = dat,
            x = annee,
            y = salaire_median,
            group = libelle,
            couleurs = c("#113263", "#ff887e"), #"#8e9594"),
            format_y = "$,"
          )
        })
      )
      
      output$phrase_part_temps_travail <- renderUI({
        temps_travail <- filtre_fap(fap_data$tab6_temps_travail_3niv, r_fap())
        temps_complet_part <- temps_travail[temps_travail == "Temps complet travaillant plus de 40h par semaine", c(pourcentage)]
        temps_partiel_part <- temps_travail[temps_travail == "Temps partiel", c(pourcentage)]
        sous_emploi_part <- temps_travail[temps_travail == "dont sous-emploi", c(pourcentage)]
        if (is.na(temps_complet_part)){tags$p("")} else {tags$p("Dans cette profession, ",
                                                     tags$b(round(temps_complet_part), " %")," des personnes déclarent travailler à temps complet plus de 40 heures par semaine, et ",
                                                     tags$b(round(temps_partiel_part), " %")," à temps partiel, dont ",
                                                     tags$b(round(sous_emploi_part), " %")," en sous-emploi. ",
                                                     align="justify")}
      })
      
      box_graphique(
        id = "temps-travail",
        data = reactive({
          dat <- filtre_fap(fap_data$tab6_temps_travail_3niv, c(r_fap(), "Ensemble"))
          ordonne_temps_travail(dat)
        }),
        graphique = reactive({
          dat <- filtre_fap(fap_data$tab6_temps_travail_3niv, c(r_fap(), "Ensemble"))
          ordonne_temps_travail(dat)
          graph_barres(
            data = dat[!is.na(pourcentage)],
            x = temps_travail,
            y = pourcentage / 100, 
            group = libelle,
            couleurs = c("#113263", "#ff887e"), #"#8e9594"),
            verticales = TRUE
          ) %>% 
            ax_dataLabels(
              enabled = TRUE, 
              formatter = format_num(".0%")
            ) %>% 
            ax_xaxis(labels = list(trim = TRUE))
        })
      )
      
    }
  )
}
