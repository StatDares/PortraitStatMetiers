
#  ------------------------------------------------------------------------
#
# Title : Portrait metier : caracteristiques des personnes en emploi
#    By : DARES
#  Date : 2020-07-17
#
#  ------------------------------------------------------------------------


panel_caracteristiques_ui <- function(id) {
  ns <- NS(id)
  tabPanel(
    title = "Caract\u00e9ristiques des personnes en emploi",
    value = "caracteristiques",
    tags$div(
      class = "box-ombrage",
      tags$h3("Caract\u00e9ristiques des personnes en emploi"),
      tags$hr(),
      fluidRow(
        column(
          width = 6,
          box_graphique_UI(
            id = ns("structure-age"),
            titre = "Structure par \u00e2ge en 2017-2019", 
            sous_titre = uiOutput(outputId = ns("phrase_struct_age"), inline = TRUE),
            source = "enqu\u00eates Emploi, Insee, moyenne annuelle sur les ann\u00e9es 2017 \u00e0 2019, traitement Dares.",
            ombrage = FALSE
          )
        ),
        column(
          width = 6,
          box_graphique_UI(
            id = ns("evol-age"), 
            titre = "\u00c9volution de la structure par \u00e2ge",
            sous_titre = uiOutput(outputId = ns("phrase_evol_struct_age"), inline = TRUE),
            source = "enqu\u00eates Emploi, Insee, moyenne annuelle sur les ann\u00e9es 1982 \u00e0 1984 et 2017 \u00e0 2019, traitement Dares.",
            ombrage = FALSE
          )
        )
      ),
      box_graphique_UI(
        id = ns("part-femme"), 
        titre = "Part des femmes dans l\'emploi", 
        sous_titre = uiOutput(outputId = ns("phrase_part_femmes"), inline = TRUE),
        source = "enqu\u00eates Emploi, Insee, donn\u00e9es liss\u00e9es par moyenne mobile d\'ordre 3, traitement Dares.",
        ombrage = FALSE
      ),
      box_graphique_UI(
            id = ns("evolution-diplome"), 
            titre = "Evolution du niveau de dipl\u00f4me", 
            sous_titre = uiOutput(outputId = ns("phrase_evol_diplome"), inline = TRUE),
            source = "enqu\u00eates Emploi, Insee, moyenne annuelle sur les ann\u00e9es 1982 \u00e0 1984 et 2017 \u00e0 2019, traitement Dares.",
            ombrage = FALSE
          
      ),
      box_graphique_UI(
            id = ns("niveau-diplome-30"), 
            titre = "Niveau de dipl\u00f4me des moins de 30 ans", 
            sous_titre = uiOutput(outputId = ns("phrase_diplome_trente"), inline = TRUE),
            source = "enqu\u00eates Emploi, Insee, moyenne annuelle sur les ann\u00e9es 2017 \u00e0 2019, traitement Dares.",
            ombrage = FALSE
      ),
      box_graphique_UI(
        id = ns("specialite-diplome"),
        titre = "Principales sp\u00e9cialit\u00e9s de  dipl\u00f4me des moins de 30 ans ayant achev\u00e9 leurs \u00e9tudes et qui exercent ce m\u00e9tier", 
        sous_titre = uiOutput(outputId = ns("phrase_spe_diplome_trente"), inline = TRUE),
        source = "enqu\u00eates Emploi, Insee, moyenne annuelle sur les ann\u00e9es 2017 \u00e0 2019, traitement Dares.",
        ombrage = FALSE
      ),
      box_graphique_UI(
        id = ns("part-empl-reg"),
        titre = "Part du m\u00e9tier dans l\'emploi r\u00e9gional",
        sous_titre = uiOutput(outputId = ns("phrase_part_metier_reg"), inline = TRUE),
        carte = TRUE, 
        source = "recensement de la population 2017, Insee, traitement Dares.",
        ombrage = FALSE, 
        height = "650px"
      ),
      box_graphique_UI(
        id = ns("part-reg-fap"),
        titre = "R\u00e9partition par r\u00e9gion de l\'emploi du m\u00e9tier",
        sous_titre = uiOutput(outputId = ns("phrase_part_reg_emploi_metier"), inline = TRUE),
        carte = TRUE, 
        source = "recensement de la population 2017, Insee, traitement Dares.",
        ombrage = FALSE, 
        height = "650px"
      ),
      conditionalPanel(
        condition = "output.is_not_prod",
        ns = ns,
        box_graphique_UI(
          id = ns("part-immi-fap"),
          titre = "R\u00e9partition de l\'emploi immigr\u00e9 selon le lieu de naissance",
          sous_titre = uiOutput(outputId = ns("phrase_part_pays_immigres"), inline = TRUE),
          carte = TRUE, 
          source = "recensement de la population 2017, Insee, traitement Dares.",
          ombrage = FALSE, 
          height = "650px"
        )
      )
    ),
    retour_haut()
  )
}


panel_caracteristiques <- function(id, r_fap) {
  moduleServer(
    id,
    function(input, output, session) {
      
      output$phrase_struct_age <- renderUI({
        part_age_2018_1 <- filtre_fap(fap_data$graph_age_2017_2019_3niv, r_fap())
        ajout_pourcentage(part_age_2018_1, "annee_2017_2019")
        part_age_2018_1_part <- part_age_2018_1[order(annee_2017_2019, decreasing = TRUE), c(pourcentage)][1]
        tranche_part_age_2018_1 <- part_age_2018_1[order(annee_2017_2019, decreasing = TRUE), c(tr_age)][1]
        tags$p(tags$b(round(part_age_2018_1_part*100), " %"),
               paste0(" des ", tolower(obtenir_libelle(r_fap())),
               " se situent dans la tranche d'âge ",
               tranche_part_age_2018_1, ". "))
      })
      
      box_graphique( 
        id = "structure-age",
        data = reactive({
          dat <- filtre_fap(fap_data$graph_age_2017_2019_3niv, c(r_fap(), "Ensemble"))
          ajout_pourcentage(dat, "annee_2017_2019", by = "fap")
        }),
        graphique = reactive({
          dat <- filtre_fap(fap_data$graph_age_2017_2019_3niv, c(r_fap(), "Ensemble"))
          ajout_pourcentage(dat, "annee_2017_2019", by = "fap")
          graph_barres(
            data = dat, 
            x = tr_age,
            y = pourcentage, 
            group = libelle,
            couleurs = c("#113263", "#ff887e")
          )
        })
      )
      
      output$phrase_evol_struct_age <- renderUI({
        if (nchar(r_fap()) == 3 & (r_fap() %notin% c("T2A","T2B"))) {
          # pour les fap 87 on a les séries depuis 1982-1984 (sauf pour T2A et T2B)
          annee_deb <- 1983
          annee_deb_explicite <- "1982-1984" 
        } else {
          # pour les fap 225 on a les séries depuis 2003-2005
          annee_deb <- 2004
          annee_deb_explicite <- "2003-2005"
        }
        structure_age <- filtre_fap(fap_data$graph_age_agrege_1982_1984_2017_2019_restructure_3niv, r_fap())
        structure_age_part_30_debut <- structure_age[age == "Moins de 30 ans" & annee == annee_deb_explicite, c(pourcentage)]
        structure_age_part_30_2018 <- structure_age[age == "Moins de 30 ans" & annee == "2017-2019", c(pourcentage)]
        structure_age_part_50_debut <- structure_age[age == "50 ans et plus" & annee == annee_deb_explicite, c(pourcentage)]
        structure_age_part_50_2018 <- structure_age[age == "50 ans et plus" & annee == "2017-2019", c(pourcentage)]
        tags$p("En 2017-2019, ",
                tags$b(round(structure_age_part_30_2018), " %"), 
                paste0(" des ",tolower(obtenir_libelle(r_fap())),
                " ont moins de 30 ans et "),
                tags$b(round(structure_age_part_50_2018), " %"),
                " ont plus de 50 ans. ")
      })
      
      box_graphique(
        id = "evol-age",
        data = reactive({
          dat <- fap_data$graph_age_agrege_1982_1984_2017_2019_restructure_3niv
          filtre_fap(dat, r_fap())
        }),
        graphique = reactive({
          dat <- fap_data$graph_age_agrege_1982_1984_2017_2019_restructure_3niv
          dat <- filtre_fap(dat, r_fap())
          graph_barres(
            data = dat,
            x = annee, y = pourcentage / 100, 
            group = age, 
            verticales = FALSE,
            couleurs = c("pantone_reflex_blueC", "pantone_bleu", "bleu1")
          ) %>% 
            ax_dataLabels(
              enabled = TRUE, 
              formatter = format_num(".0%")
            ) %>% 
            ax_chart(
              stacked = TRUE
            ) %>% 
            ax_xaxis(max = 1)
        })
      )
      
      output$phrase_part_femmes <- renderUI({
        part <- filtre_fap(fap_data$graph_part_femmes_3niv, r_fap())
        part <- part[order(annee, decreasing = TRUE), c(part)][1]
        tags$p(tags$b(round(part), "%"), "des", tolower(obtenir_libelle(r_fap())), "sont des femmes en 2018.")
      })
      
      box_graphique( 
        id = "part-femme",
        data = reactive({
          dat <- fap_data$graph_part_femmes_3niv
          filtre_fap(dat, c(r_fap(), "Ensemble"))
        }),
        graphique = reactive({
          dat <- fap_data$graph_part_femmes_3niv
          dat <- filtre_fap(dat, c(r_fap(), "Ensemble"))
          dat[, annee_num := gsub("-.*$", "", annee)]
          dat[, annee_num := (as.numeric(annee_num)+1)]
          graph_courbes(
            data = dat, x = as.Date(paste0(annee_num, "-01-01")),
            y = part / 100, group = libelle, 
            couleurs = c("#113263", "#ff887e")
          )
        })
      )
      
      output$phrase_evol_diplome <- renderUI({
        texte_par_diplome <- c(
        "Diplôme supérieur (bac +3 ou plus)" = "ont un diplôme supérieur à bac + 3",
        "Diplôme supérieur (bac + 3 ou plus)" = "ont un diplôme supérieur à bac + 3",
        "Bac + 2" = "ont un diplôme de niveau bac + 2",                                  
        "Bac, brevet professionnel ou équivalent" = "ont un diplôme de niveau Bac, brevet professionnel ou équivalent", 
        "CAP, BEP ou autre diplôme équivalent" = "ont un diplôme de niveau CAP, BEP ou équivalent",
        "Aucun diplôme, CEP ou brevet des collèges" = "n'ont aucun diplôme ou ont un diplôme de niveau CEP ou brevet des collèges",
        "En cours d'études initiales" = "sont en cours d'études initiales",
        "Inconnu" = "ont un diplôme de niveau inconnu"
        )
        part_diplome_2018 <- filtre_fap(fap_data$graph_dipl_restructure_3niv, r_fap())
        part_diplome_2018 <- part_diplome_2018[annee == "2017-2019"]
        part_diplome_2018_part <- part_diplome_2018[order(pourcentage, decreasing = TRUE), c(pourcentage)][1]
        part_diplome_2018_diplome <- part_diplome_2018[order(pourcentage, decreasing = TRUE), c(diplome)][1]
        tags$p(
          "En 2017-2019,", tags$b(paste0(round(part_diplome_2018_part*100), " %")),
          paste0(" des ", tolower(obtenir_libelle(r_fap()))," ",
                             texte_par_diplome[[part_diplome_2018_diplome]],". ")
        )
      })
      
      box_graphique( 
        id = "evolution-diplome",
        data = reactive({
          dat <- filtre_fap(fap_data$graph_dipl_restructure_3niv, r_fap())
          ordonne_niv_dipl(dat)
        }),
        graphique = reactive({
          dat <- filtre_fap(fap_data$graph_dipl_restructure_3niv, r_fap())
          ordonne_niv_dipl(dat)
          graph_radar(
            data = dat, 
            x = diplome, y = pourcentage, group = annee, 
            couleurs = c("#ff887e","#113263")
          )
        })
      )
      
      output$phrase_diplome_trente <- renderUI({
        texte_par_diplome <- c(
          "Diplôme supérieur (bac +3 ou plus)" = "ont un diplôme supérieur à bac + 3",
          "Diplôme supérieur (bac + 3 ou plus)" = "ont un diplôme supérieur à bac + 3",
          "Bac + 2" = "ont un diplôme de niveau bac + 2",                                  
          "Bac, brevet professionnel ou équivalent" = "ont un diplôme de niveau Bac, brevet professionnel ou équivalent", 
          "CAP, BEP ou autre diplôme équivalent" = "ont un diplôme de niveau CAP, BEP ou équivalent",
          "Aucun diplôme, CEP ou brevet des collèges" = "n'ont aucun diplôme ou ont un diplôme de niveau CEP ou brevet des collèges",
          "En cours d'études initiales" = "sont en cours d'études initiales",
          "Inconnu" = "ont un diplôme de niveau inconnu"
        )
        part_diplome_2018_moins30 <- filtre_fap(fap_data$graph_dipl_moins_30_3niv, r_fap())
        part_diplome_2018_moins30_part <- part_diplome_2018_moins30[order(pourcentage, decreasing = TRUE), c(pourcentage)][1]
        part_diplome_2018_moins30_diplome <- part_diplome_2018_moins30[order(pourcentage, decreasing = TRUE), c(diplome)][1]
        tags$p("Parmi les personnes de moins de 30 ans exerçant ce métier, ",
              tags$b(round(part_diplome_2018_moins30_part*100), " % "),
              paste0(texte_par_diplome[[part_diplome_2018_moins30_diplome]],". ")
        )
      })
      
      box_graphique(
        id = "niveau-diplome-30",
        data = reactive({
          dat <- filtre_fap(fap_data$graph_dipl_moins_30_3niv, c(r_fap(), "Ensemble"))
          ordonne_niv_dipl(dat)
        }),
        graphique = reactive({
          dat <- filtre_fap(fap_data$graph_dipl_moins_30_3niv, c(r_fap(), "Ensemble"))
          ordonne_niv_dipl(dat)
          graph_radar(
            data = dat, 
            x = diplome, y = pourcentage, group = libelle, 
            couleurs = c("#113263", "#ff887e")
          )
        })
      )
      
      output$phrase_spe_diplome_trente <- renderUI({
        spe_dipl <- filtre_fap(fap_data$specialite_diplome_3niv, r_fap())
        if(NROW(spe_dipl) == 0) {} else {
          spe_dipl_1 <- spe_dipl[1] 
          spe_dipl_1_label <- spe_dipl_1[, c(label_spedipl)]
          spe_dipl_1_part <- spe_dipl_1[, c(pourcentage)]}
        if(NROW(spe_dipl) == 0) {tags$p()} else {
          tags$p("Parmi les personnes de moins de 30 ans qui exercent ce métier, ",
                 tags$b(round(spe_dipl_1_part), " %"),paste0(" ont un diplôme spécialisé en ",
                 tolower(spe_dipl_1_label), ". "))}
      })
      
      box_graphique(
        id = "specialite-diplome",
        data = reactive({
          filtre_fap(fap_data$specialite_diplome_3niv, r_fap())
        }),
        graphique = reactive({
          dat <- filtre_fap(fap_data$specialite_diplome_3niv, r_fap())
          dat$pourcentage <- dat$pourcentage/100
          graph_barres(
            data = dat,
            x = label_spedipl,
            y = pourcentage,
            verticales = FALSE
          ) 
        })
      )
      
      output$phrase_part_metier_reg <- renderUI({
        partmetreg <- filtre_fap(fap_data$input_carte1, r_fap())
        if (nrow(partmetreg) >= 1) {
           partmetreg <- partmetreg[order(variable, decreasing = TRUE), ][1]
          tags$p("Les ", tolower(obtenir_libelle(r_fap())), " repr\u00e9sentent ", tags$b(round(partmetreg$variable), "%"),paste(" de l'emploi de la r\u00e9gion ", partmetreg$nom_reg,".",sep=""))
        }
      })
      
      box_graphique( 
        id = "part-empl-reg",
        data = reactive({
          filtre_fap(fap_data$input_carte1, r_fap())
        }),
        graphique = reactive({
          dat <- fap_data$input_carte1
          dat <- filtre_fap(dat, r_fap())
          choroplethe(dat, variable = "variable", niveau = "reg_dom", val_absolue = "emp_fap_reg", titre_legende = "Part en %")
        }),
        carte = TRUE
      )
      
      output$phrase_part_reg_emploi_metier <- renderUI({
        partregmet <- filtre_fap(fap_data$input_carte1, r_fap())
        if (nrow(partregmet) >= 1) {
          partregmet <- partregmet[order(pourcentage, decreasing = TRUE), ][1]
          tags$p(tags$b(round(partregmet$pourcentage * 100), "%"), "des ",tolower(obtenir_libelle(r_fap())), paste("travaillent en r\u00e9gion ",  partregmet$nom_reg, ".",sep=""))
        }
      })
      
      box_graphique(
        id = "part-reg-fap",
        data = reactive({
          filtre_fap(fap_data$input_carte1, r_fap())
        }),
        graphique = reactive({
          dat <- fap_data$input_carte1
          dat[,part:=100*pourcentage]
          dat <- filtre_fap(dat, r_fap())
          choroplethe(dat, variable = "part", niveau = "reg_dom", val_absolue = "emp_fap_reg2", titre_legende = "Part en %")
        }),
        carte = TRUE
      )
      output$is_not_prod = reactive({
        return(!version_prod)
      })
      outputOptions(output, "is_not_prod", suspendWhenHidden = FALSE)
      
      output$phrase_part_pays_immigres <- renderUI({
        part_origine <- filtre_fap(fap_data$carte_immigres, r_fap())
        origine_ens <- filtre_fap(fap_data$carte_immigres,"ENS")
        part <- part_origine[order(pourcentage_immigres, decreasing = TRUE), c(pourcentage_immigres)][1]
        origine <- part_origine[order(pourcentage_immigres, decreasing = TRUE), c(lieu_naissance)][1]
        part_origine_ens <- round(origine_ens[lieu_naissance == origine, c(pourcentage_immigres)])
        
        tags$p("Parmi les ", tolower(obtenir_libelle(r_fap())), " issus de l'immigration, ",
               tags$b(round(part), "%") , "sont originaires de '", origine, "' contre ",
               part_origine_ens," % tous m\u00e9tiers confondus.")
      })
      
      box_graphique(
        id = "part-immi-fap",
        data = reactive({
          filtre_fap(fap_data$carte_immigres, r_fap())
        }),
        graphique = reactive({
          dat <- fap_data$carte_immigres
          #dat[,part:=100*pourcentage_immigres]
          dat <- filtre_fap(dat, r_fap())
          choroplethe_monde(dat, variable = "pourcentage_immigres", val_absolue = "effectif", val_absolue_bis ="immigres",val_absolue_ter="non_immigres", titre_legende = "Part en %")
        }),
        carte = TRUE
      )
      
      
    }
  )
}
