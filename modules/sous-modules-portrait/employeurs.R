
#  ------------------------------------------------------------------------
#
# Title : Portrait metier : caracteristiques des employeurs
#    By : DARES
#  Date : 2020-07-17
#
#  ------------------------------------------------------------------------


panel_employeur_ui <- function(id) {
  ns <- NS(id)
  tabPanel(
    title = "Caract\u00e9ristiques des employeurs",
    value = "employeurs",
    tags$div(
      class = "box-ombrage",
      tags$h3("Caract\u00e9ristiques des employeurs"),
      tags$hr(),
      box_graphique_UI(
        id = ns("secteur-employeurs"),
        titre = "Principaux secteurs employeurs en 2017-2019", 
        sous_titre = uiOutput(outputId = ns("phrase_part_secteurs_employeurs"), inline = TRUE),
        source = "enqu\u00eates Emploi, Insee, moyenne annuelle sur les ann\u00e9es 2017 \u00e0 2019, traitement Dares.",
        ombrage = FALSE
      ),
      box_graphique_UI(
        id = ns("categories-employeurs"), 
        titre = "Cat\u00e9gorie d\'employeurs en 2017-2019 (champ salari\u00e9s)",
        sous_titre = uiOutput(outputId = ns("phrase_part_categories_employeurs"), inline = TRUE),
        source = "enqu\u00eates Emploi, Insee, moyenne annuelle sur les ann\u00e9es 2017 \u00e0 2019, traitement Dares.",
        ombrage = FALSE
      )
    ),
    retour_haut()
  )
}


panel_employeur <- function(id, r_fap) {
  moduleServer(
    id,
    function(input, output, session) {
      
      output$phrase_part_secteurs_employeurs <- renderUI({
        article_par_secteur_employeur <- c(
          "Agriculture, sylviculture et pêche" = "de l'", 
          "Industries extractives" = "des ", 
          "Fabrication de denrées alimentaires, de boissons et de produits à base de tabac" = "de la ",
          "Fabrication de textiles, industries de l'habillement, industrie du cuir et de la chaussure" = "de la ",
          "Travail du bois, industries du papier et imprimerie" = "du ", 
          "Cokéfaction et raffinage" = "de la ",
          "Industrie chimique" = "de l'",
          "Industrie pharmaceutique" = "de l'",
          "Fabrication de produits en caoutchouc et en plastique" = "de la ",
          "Métallurgie et fabrication de produits métalliques" = "de la ", 
          "Fabrication de produits informatiques, électroniques et optiques" = "de la ",
          "Fabrication d'équipements électriques" = "de la ",
          "Fabrication de machines et équipements n.c.a." = "de la ",
          "Fabrication de matériels de transport" = "de la ",
          "Autres industries manufacturières ; réparation et installation de machines et d'équipements" = "des ",
          "Production et distribution d'électricité, de gaz, de vapeur et d'air conditionné" = "de la ",
          "Production et distribution d'eau ; assainissement, gestion des déchets et dépollution" = "de la ",
          "Construction" = "de la ", 
          "Commerce ; réparation d'automobiles et de motocycles" = "du ",
          "Transports et entreposage" = "des ",
          "Hébergement et restauration" = "de l'",
          "Edition, audiovisuel et diffusion" = "de l'",
          "Télécommunications" = "des ",
          "Activités informatiques et services d'information"= "des ",
          "Activités financières et d'assurance" = "des ",
          "Activités immobilières" = "des ",
          "dont : loyers imputés des logements occupés par leur propriétaire" = "des ",
          "Activités juridiques, comptables, de gestion, d'architecture, d'ingénierie, etc." = "des ",
          "Recherche-développement scientifique" = "de la ",
          "Autres activités spécialisées, scientifiques et techniques" = "des ",
          "Activités de services administratifs et de soutien" = "des ",
          "Administration publique" = "de l'",
          "Enseignement" = "de l'",
          "Activités pour la santé humaine" = "des ",
          "Hébergement médico-social et social et action sociale sans hébergement" = "de l'",
          "Arts, spectacles et activités récréatives" = "des ",
          "Autres activités de services" = "des ",
          "Activités des ménages en tant qu'employeurs ou en tant que producteurs pour usage propre" = "des ",
          "Activités extra-territoriales" = "des ",
          "Autres secteurs" = "des "
        )
        sect_empl <- filtre_fap(fap_data$secteurs_employeurs_3niv, r_fap())
        if(NROW(sect_empl) == 0) {} else {
          sect_empl_1 <- sect_empl[1] 
          sect_empl_1_label <- sect_empl_1[, c(label_secteur)]
          sect_empl_1_part <- sect_empl_1[, c(pourcentage)]}
        if(NROW(sect_empl) == 0) {tags$p()} else {tags$p("Parmi les personnes qui exercent ce métier, ",
                                                         tags$b(round(sect_empl_1_part), " %"),paste0(" travaillent dans le secteur ",
                                                                article_par_secteur_employeur[[sect_empl_1_label]],
                                                                tolower(sect_empl_1_label), ". "))}
      })
      
      box_graphique(
        id = "secteur-employeurs",
        data = reactive({
          filtre_fap(fap_data$secteurs_employeurs_3niv, r_fap())
        }),
        graphique = reactive({
          dat <- filtre_fap(fap_data$secteurs_employeurs_3niv, r_fap())
          graph_barres(
            data = dat,
            x = label_secteur,
            y = pourcentage / 100,
            group = libelle,
            couleurs = c("#113263", "#ff887e"),
            verticales = FALSE
          ) %>% 
            ax_dataLabels(
              enabled = TRUE, 
              formatter = format_num(".0%")
            ) %>% 
            ax_xaxis(
              categories = wrap_labels(dat$label_secteur)
            )
        })
      )
      
      output$phrase_part_categories_employeurs <- renderUI({
        texte_par_categorie_employeur <- c(
          "Etablissements de moins de 10 salariés" = "travaillent dans le privé au sein d'un établissement de moins de 10 salariés", 
          "Etablissements de 10 à 50 salariés"     = "travaillent dans le privé au sein d'un établissement de 10 à 50 salariés",
          "Etablissements de 50 à 500 salariés"    = "travaillent dans le privé au sein d'un établissement de 50 à 500 salariés",
          "Etablissements de plus de 500 salariés" = "travaillent dans le privé au sein d'un établissement de plus de 500 salariés",
          "Etat, collectivités, hôpitaux publics"  = "travaillent pour l'Etat, les collectivités ou les hôpitaux publics",
          "Non Renseigné"                          = "ont une catégorie d'employeur non renseignée",
          "Particuliers"                           = "travaillent pour des particuliers",
          "Taille inconnue, hors particulier, Etat" = "travaillent dans le privé dans un établissement de taille inconnue"
        )
        cat_empl <- filtre_fap(fap_data$graph_categorie_employeur_3niv, r_fap())
        if(NROW(cat_empl) == 0) {} else {
        cat_empl_ordo <- cat_empl[order(pourcentage, decreasing=TRUE), ]
        cat_empl_1 <- head(cat_empl_ordo, n = 1)
        cat_empl_1_type <- cat_empl_1[, c(type_employeur)]
        cat_empl_1_part <- cat_empl_1[, c(pourcentage)]}
        if(NROW(cat_empl) == 0) {tags$p()} else {tags$p("Parmi les salariés de ce métier, ",
        tags$b(round(cat_empl_1_part), " %"), paste0(texte_par_categorie_employeur[[cat_empl_1_type]], ". "))}
      })
      
      box_graphique( 
        id = "categories-employeurs",
        data = reactive({
          filtre_fap(fap_data$graph_categorie_employeur_3niv, c(r_fap(), "Ensemble"))
        }),
        graphique = reactive({
          dat <- filtre_fap(fap_data$graph_categorie_employeur_3niv, c(r_fap(), "Ensemble"))
          dat$pourcentage[is.na(dat$pourcentage)] <- 0
          graph_radar(
            data = dat, 
            x = type_employeur, y = pourcentage / 100, group = libelle,
            couleurs = c("#113263", "#ff887e")
          )
        })
      )
      
    }
  )
}

