e_ou_apostrophe <- function(word){
  if (tolower(substr(word, 1, 1)) %in% c("a", "e", "i", "o", "u")){
    e_ou_ap = "'"
  }
  else {e_ou_ap = "e "}
  return (paste0(e_ou_ap, word))
}

panel_description_ui <- function(id) {
  ns <- NS(id)
  tabPanel(
    title = "Description",
    value = "description",
    tags$div(
      class = "box-ombrage",
      tags$h4("Description de la catégorie"),
      #HTML("<h4 style='background:#ff887e'>Description de la catégorie</h4>"),
      tags$hr(),
      htmlOutput(outputId = ns("generalite")),
      tags$hr(),
      # uiOutput(outputId = ns("description"), style = "text-align: justify;"),
      # br(),
      uiOutput(outputId = ns("paragraphe")),
      br(),
      htmlOutput(outputId = ns("phrase_fap_rome")),
      br(),
      htmlOutput(outputId = ns("phrase_fap_pcs")),
      br(),
      htmlOutput(outputId = ns("liste_appellations")),
      conditionalPanel(
        condition = "output.niveau_fap == '87'",
        ns = ns,
        box_graphique_UI(
          id = ns("effectif2"), 
          titre = "R\u00e9partition par m\u00e9tiers d\u00e9taill\u00e9s (Fap 225)", 
          source = "enqu\u00eates Emploi, Insee, donn\u00e9es liss\u00e9es par moyenne mobile d\'ordre 3, traitement Dares. Par souci de simplification, on désigne dans le graphique les périodes 2003-2005 par 2004 et 2017-2019 par 2018.",
          ombrage = FALSE
        )
      )
    )
  )
}


panel_description <- function(id, r_fap) {
  moduleServer(
    id,
    function(input, output, session) {
      
      niveau_fap <- reactive({
        req(r_fap())
        if (nchar(r_fap()) == 5){
          return('225')
        } else {
          fap_225_de_fap87 = libs_fap[fap_87 == r_fap()]
          nb_fap225 = nrow(fap_225_de_fap87)
          if (nb_fap225 == 0){
            return('87_225')
          } else {
            return('87')}
        }
      })
      
      output$niveau_fap <- reactive({
        niveau_fap()
      })
      outputOptions(output, "niveau_fap", suspendWhenHidden = FALSE)
      
      fap_225_dominant <- reactive({
        if (niveau_fap() == '87_225'){
          return( libs_fap_225_aussi_87[fap_87 == r_fap(), fap])
        }
        else{
          return(r_fap())
        }
      })
      
      output$generalite <- renderUI({
        req(r_fap())
        code_fap = r_fap()
        libelle_fap = obtenir_libelle(r_fap())
        phrase_lien_fap = paste0("Le portrait statistique des métiers s'appuie sur ","<a href=https://dares.travail-emploi.gouv.fr/donnees/la-nomenclature-des-familles-professionnelles-fap-2009>la nomenclature des familles professionnelles.","</a>")
        niveau = "détaillé"
        phrase = paste0("Au niveau métier agrégé, elle appartient à la catégorie ", "<a href=?fap=", substr(code_fap, 1, 3), ">", obtenir_libelle(substr(code_fap, 1, 3)), "</a>")
        if (nchar(code_fap) == 3) {
          niveau = "famille de métiers"
          
          fap_225_de_fap87 = libs_fap[fap_87 == code_fap]
          nb_fap225 = nrow(fap_225_de_fap87)
        
          phrase = paste0( c(paste0("composée de ", nb_fap225, " métiers plus détaillés : <br> "), fap_225_de_fap87$libelle),
                          collapse = " <br>  &nbsp; - ")
          
          if (nb_fap225 == 0){
            phrase = "composée d'un seul métier détaillé."
              #"C'est également un métier détaillé qui forme un métier agrégé à lui tout seul."
          }
      }
        if (nchar(code_fap) == 3) {
        HTML(paste0(c(phrase_lien_fap," La catégorie '",libelle_fap,
          "' est une famille de métiers ",
          #". Son code est : ", code_fap,
         phrase),
          collapse = ""))} else {
          HTML(paste0(c(phrase_lien_fap," La catégorie '",libelle_fap,
                        "' est un métier ", niveau,
                        #". Son code est : ", code_fap,
                        ". <br>", phrase),
                      collapse = ""))}
      })
      
      output$phrase_fap_pcs <- renderUI({
        req(r_fap())
        liste_pcs <- filtre_fap(fap_data$FAP_PCS, fap_225_dominant())
        if (nchar(r_fap()) == 5){
          if (nrow(liste_pcs) == 1) {
            tags$p(paste("C\u00f4t\u00e9 pcs, elle correspond au code ",
                         liste_pcs$pcs, ":", liste_pcs$intitule_pcs)
            )
          }
          else {
            paste_pcs = apply(liste_pcs[, c('pcs', 'intitule_pcs')], 1, paste0, collapse=": ")
            phrase_intro = "C\u00f4t\u00e9 PCS, elle correspond aux cat\u00e9gories suivantes :"
            HTML(paste0(c(phrase_intro, paste_pcs), collapse = " <br>  &nbsp;"))
          }
        }
      })
      
      liste_rome <- reactive(
          filtre_fap(fap_data$fap225_rome, fap_225_dominant())
      )
      
      output$phrase_fap_rome <- renderUI({
        req(r_fap())
        if (niveau_fap() != "87"){
          if (sum(is.na(liste_rome()$rome) == 0)) {
            if (nrow(liste_rome()) == 1) {
              tags$p(paste("Cette cat\u00e9gorie de m\u00e9tiers correspond au code Rome",
                           liste_rome()$rome, ":", liste_rome()$intitule_rome)
              )
            }
            if (nrow(liste_rome()) > 1) {
              paste_rome = apply(liste_rome()[, c('rome', 'intitule_rome')], 1, paste0, collapse=": ")
              phrase_intro = "Cette cat\u00e9gorie de m\u00e9tiers correspond aux codes Rome suivants :"
              HTML(paste0(c(phrase_intro, paste_rome), collapse = " <br>  &nbsp;"))
            }
          }
          else{
            tags$p("Cette cat\u00e9gorie de m\u00e9tiers ne correspond \u00e0 aucun code Rome ",
                   " parce qu\'elle ne concerne pas les salari\u00e9s du priv\u00e9 qui",
                   " sont les seuls couverts par la nomenclature Rome",
                   " ou bien du fait de son caract\u00e8re particulier regroupant des m\u00e9tiers diff\u00e9rents"
            )
          }
        }
      })
      
      output$liste_appellations <- renderUI({
        if (nrow(liste_rome()) > 1) {
          if(niveau_fap() == "225"){
            liste_appellations = appellations_fap225[appellations_fap225$fap == r_fap(), "keywords"]
          } else {
            liste_appellations = appellations_fap87[appellations_fap87$fap == r_fap(), "keywords"]
          }
          liste_appellations = str_split(liste_appellations, ",", simplify= TRUE)
          phrase_intro = "Cette cat\u00e9gorie de m\u00e9tiers correspond aux appellations suivantes (source code rome) :"
          
          HTML(paste0(c(phrase_intro, liste_appellations), collapse = " <br>  &nbsp;"))
        }
      })
      
      # output$description <- renderUI({
      #   req(r_fap())
      #   md <- file.path("documentation/fap_md_utf8", paste0(toupper(r_fap()), ".md"))
      #   liste_pcs <- filtre_fap(fap_data$FAP_PCS, r_fap())
      #   if (!file.exists(md) & sum(is.na(liste_rome()$rome) != 0) & sum(is.na(liste_pcs$PCS)) != 0) {
      #     tags$i("D\u00e9sol\u00e9, pas de documentation pour cette famille de m\u00e9tier...")
      #   } else if (file.exists(md)) {
      #     includeMarkdown(path = md)
      #   } else {tags$i("")}
      # })
      
      output$paragraphe <- renderUI({
        req(r_fap())
        dat_eff <- filtre_fap(effectifs, r_fap())
        dat_eff_fin <- round(dat_eff[order(annee, decreasing=TRUE), c(effectif)][1], digits=0)
        
        texte_avec_diplome <- c(
          "Diplôme supérieur (bac +3 ou plus)" = "avec un diplôme supérieur à bac + 3",
          "Diplôme supérieur (bac + 3 ou plus)" = "avec un diplôme supérieur à bac + 3",
          "Bac + 2" = "avec un diplôme de niveau bac + 2",                                  
          "Bac, brevet professionnel ou équivalent" = "avec un diplôme de niveau Bac, brevet professionnel ou équivalent", 
          "CAP, BEP ou autre diplôme équivalent" = "avec un diplôme de niveau CAP, BEP ou équivalent",
          "Aucun diplôme, CEP ou brevet des collèges" = "sans aucun diplôme ou avec un diplôme de niveau CEP ou brevet des collèges",
          "En cours d'études initiales" = "en cours d'études initiales",
          "Inconnu" = "avec un diplôme de niveau inconnu"
        )
        
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
        
        texte_article_region <- c(
          "Auvergne-Rhône-Alpes" = " en ",
          "Bourgogne-Franche-Comté" = " en ",
          "Bretagne" = " en ",
          "Centre-Val de Loire" = " en ",
          "Corse" = " en ",
          "Grand Est" = " dans le ",
          "Guadeloupe" = "en ",
          "Guyane" = " en ",
          "Hauts-de-France" = " dans les ",
          "Île-de-France" = " en ",
          "La Réunion" = " à ",
          "Martinique" = " en ",
          "Mayotte" = " à ",
          "Normandie" = " en ",
          "Nouvelle-Aquitaine" = " en ",
          "Occitanie" = " en ",
          "Pays de la Loire" = " en ",
          "Provence-Alpes-Côte d'Azur" = " en " 
        )
        
        if (nchar(r_fap()) == 3 & (r_fap() %notin% c("T2A","T2B"))) {
          # pour les fap 87 on a les séries depuis 1982-1984 (sauf pour T2A et T2B)
          annee_deb <- 1983
          annee_deb_explicite <- "1982-1984" 
        } 
        else {
          # pour les fap 225 on a les séries depuis 2003-2005
          annee_deb <- 2004
          annee_deb_explicite <- "2003-2005"
        }
        
        eff_annee_deb <- dat_eff[annee == annee_deb, effectif]
        eff_annee_fin <- tail(dat_eff$effectif, n = 1)
        eff_annee_deb_arrondi <-  round(eff_annee_deb, digits=0)
        eff_annee_fin_arrondi <-  round(eff_annee_fin, digits=0)
        duree <- 2018 - annee_deb
        
        evolution <- round(((eff_annee_fin - eff_annee_deb)/eff_annee_deb) * 100)
        evolution_annuelle <- abs(round((((eff_annee_fin/eff_annee_deb)^ (1 / duree))-1)*100, 1))
        
        verbe_evol <- if(evolution >= 0) {"a augment\u00e9 de"} else {"a diminu\u00e9 de"}
        
        part_femmes_2018 <- filtre_fap(fap_data$graph_part_femmes_3niv, r_fap())
        part_femmes_2018 <- part_femmes_2018[order(annee, decreasing = TRUE), c(part)][1]
        
        part_age_2018_1 <- filtre_fap(fap_data$graph_age_2017_2019_3niv, r_fap())
        ajout_pourcentage(part_age_2018_1, "annee_2017_2019")
        
        part_age_2018_1_part <- part_age_2018_1[order(annee_2017_2019, decreasing = TRUE), c(pourcentage)][1]
        tranche_part_age_2018_1 <- part_age_2018_1[order(annee_2017_2019, decreasing = TRUE), c(tr_age)][1]
        
        structure_age <- filtre_fap(fap_data$graph_age_agrege_1982_1984_2017_2019_restructure_3niv, r_fap())
        structure_age_part_30_debut <- structure_age[age == "Moins de 30 ans" & annee == annee_deb_explicite, c(pourcentage)]
        structure_age_part_30_2018 <- structure_age[age == "Moins de 30 ans" & annee == "2017-2019", c(pourcentage)]
        structure_age_part_50_debut <- structure_age[age == "50 ans et plus" & annee == annee_deb_explicite, c(pourcentage)]
        structure_age_part_50_2018 <- structure_age[age == "50 ans et plus" & annee == "2017-2019", c(pourcentage)]
        
        part_diplome_2018 <- filtre_fap(fap_data$graph_dipl_restructure_3niv, r_fap())
        part_diplome_2018 <- part_diplome_2018[annee == "2017-2019"]
        part_diplome_2018_part <- part_diplome_2018[order(pourcentage, decreasing = TRUE), c(pourcentage)][1]
        part_diplome_2018_diplome <- part_diplome_2018[order(pourcentage, decreasing = TRUE), c(diplome)][1]
        part_diplome_2018_moins30 <- filtre_fap(fap_data$graph_dipl_moins_30_3niv, r_fap())
        part_diplome_2018_moins30_part <- part_diplome_2018_moins30[order(pourcentage, decreasing = TRUE), c(pourcentage)][1]
        part_diplome_2018_moins30_diplome <- part_diplome_2018_moins30[order(pourcentage, decreasing = TRUE), c(diplome)][1]
        
        partmetreg <- filtre_fap(fap_data$input_carte1, r_fap())
        partmetreg <- partmetreg[order(variable, decreasing = TRUE), ][1]
        partregmet <- filtre_fap(fap_data$input_carte1, r_fap())
        partregmet <- partregmet[order(pourcentage, decreasing = TRUE), ][1]
        
        hor_atyp <- filtre_fap(fap_data$graph_horaires_atypiques_3niv, r_fap())
        tra_dim <- hor_atyp[horaire_atypique == "Travail le dimanche", c(pourcentage)]
        tra_sam <- hor_atyp[horaire_atypique == "Travail le samedi", c(pourcentage)]
        tra_nuit <- hor_atyp[horaire_atypique == "Travail de nuit", c(pourcentage)]
        
        effectifs_2018 <- filtre_fap(effectifs, r_fap())
        effectifs_2018 <- effectifs_2018[annee == 2018,c(effectif)]
        effectifs_ens_2018 <- filtre_fap(effectifs, "ENS")
        effectifs_ens_2018 <- effectifs_ens_2018[annee == 2018,c(effectif)]
        part_emploi_national_2018 <- effectifs_2018/effectifs_ens_2018*100
        
        effectifs_salaries_2018 <- filtre_fap(fap_data$effectifsalarie_hors_app_3niv, r_fap())
        effectifs_salaries_2018 <- effectifs_salaries_2018[,c(sumeffsal)]
        part_salarie_hors_app <- effectifs_salaries_2018/effectifs_2018*100
        
        sal_mensuel <- filtre_fap(fap_data$graph_salaires_3niv, r_fap())
        sal_mensuel_1250 <- sal_mensuel[salaire == "Moins de 1250 €", c(pourcentage)]
        sal_mensuel_1250_1500 <- sal_mensuel[salaire == "De 1250 € à moins de 1500 €", c(pourcentage)]
        sal_mensuel_3000 <- sal_mensuel[salaire == "3000 € ou plus", c(pourcentage)]
        
        sal_net_median <- filtre_fap(fap_data$salaire_median_3niv, r_fap())
        if (nrow(sal_net_median) >= 1) 
        {
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
        
        cat_empl <- filtre_fap(fap_data$graph_categorie_employeur_3niv, r_fap())
        cat_empl_ordo <- cat_empl[order(pourcentage, decreasing=TRUE), ]
        cat_empl_1 <- head(cat_empl_ordo, n = 1)
        cat_empl_1_type <- cat_empl_1[, c(type_employeur)]
        cat_empl_1_part <- cat_empl_1[, c(pourcentage)]
        
        eff_annee_fin_arrondi_format <- format(as.numeric(eff_annee_fin_arrondi) * 1000,
                                               big.mark = " ", digits = 0, scientific = FALSE)
        eff_annee_deb_arrondi_format <- format(as.numeric(eff_annee_deb_arrondi) * 1000,
                                               big.mark = " ", digits = 0, scientific = FALSE)
        
        tension_nat = tensions[fap == r_fap() & niveau_geo == "nat", "categorie_tension"]
        if (nrow(tension_nat)>0) {tension_nat = sum(tension_nat)}
        if (is.numeric(tension_nat) == TRUE) 
          {
          niveau_tension_national <- switch(tension_nat, "rares", "faibles", "moyennes", "élevées", "très fortes")
        } 
        else { niveau_tension_national <- "" }
        
        
        if (nchar(r_fap()) == 3){ 
          tags$p("En 2017-2019, cette famille de métiers comprend :",
                  tags$br(),
                 tags$ul(
                   tags$li(HTML( str_replace_all(eff_annee_fin_arrondi_format, pattern=" ", "&nbsp;")),
                           " personnes"), 
                   tags$li(paste0(round(part_femmes_2018), " % de femmes")), 
                   tags$li(paste0(round(structure_age_part_30_2018), " % de personnes de moins de 30 ans et ",
                           round(structure_age_part_50_2018), " % de plus de 50 ans")),
                   tags$li(paste0(round(part_diplome_2018_part*100), " % d'entre elles ",
                                  texte_avec_diplome[[part_diplome_2018_diplome]],". "))
                 ),
                  "Les ",tolower(obtenir_libelle(r_fap()))," repr\u00e9sentent :",
                   tags$br(),
                 tags$ul(
                   tags$li(format(part_emploi_national_2018, decimal.mark = ",", digits=1, nsmall = 1),
                           " % de l'emploi national et ",
                           paste0(format(partmetreg$variable, decimal.mark = ",", digits=1, nsmall = 1),
                                  " % de l'emploi"),
                           paste0(texte_article_region[[partmetreg$nom_reg]],partmetreg$nom_reg,
                                  ", région où leur part est la plus importante")),
                   tags$li(paste0(round(partregmet$pourcentage * 100), " % d'entre eux travaillent",
                                  texte_article_region[[partregmet$nom_reg]],
                                  partregmet$nom_reg, ", région où ils sont les plus nombreux"))
                  ),
                  "Dans cette profession, ",
                  paste0(round(tra_sam), " % des personnes travaillent le samedi, "),
                  paste0(round(tra_dim), " % le dimanche, et "),
                  paste0(round(tra_nuit), " % de nuit. "),
                  tags$br(),tags$br(),
                  "Parmi les salari\u00e9s \u00e0 temps complet, ",
                  paste0(round (sal_mensuel_1250 + sal_mensuel_1250_1500),
                         " % d\u00e9clarent gagner moins de 1 500 € nets par mois, et "),
                  paste0(round (sal_mensuel_3000), " % d\u00e9clarent gagner plus de 3 000 €. "),
                  paste0("Le salaire net médian \u00e0 temps complet s'élève \u00e0 ",
                         format(as.numeric(sal_net_median_fin), big.mark = " ", digits = 0, scientific = FALSE),
                         " € par mois en 2017-2019. "),
                  tags$br(),tags$br(),
                  "Au niveau national, la tension et les difficultés de recrutement en 2019 chez les",
                  tolower(obtenir_libelle(r_fap())),
                  "sont",
                  paste0(niveau_tension_national, ". "),
                  align="justify" #,sep=
                  )} 
        else if (nchar(r_fap()) == 5 && is.numeric(tension_nat) == TRUE &&
                 nrow(sal_net_median) >= 1 && !is.na(tra_sam)) {
                           tags$p("En 2017-2019, ce métier comprend :",
                           tags$br(),
                           tags$ul(
                             tags$li(HTML( str_replace_all(eff_annee_fin_arrondi_format, pattern=" ", "&nbsp;")),
                                     " personnes"), 
                             tags$li(paste0(round(part_femmes_2018), " % de femmes")), 
                             tags$li(paste0(round(structure_age_part_30_2018), " % de personnes de moins de 30 ans et ",
                                            round(structure_age_part_50_2018), " % de plus de 50 ans")),
                             tags$li(paste0(round(part_diplome_2018_part*100), " % d'entre elles ",
                                            texte_avec_diplome[[part_diplome_2018_diplome]],". "))
                           ),
                           "Dans cette profession, ",
                           paste0(round(tra_sam), " % des personnes travaillent le samedi, "),
                           paste0(round(tra_dim), " % le dimanche, et "),
                           paste0(round(tra_nuit), " % de nuit. "),
                           tags$br(),tags$br(),
                           "Parmi les salari\u00e9s \u00e0 temps complet, ",
                           paste0(round (sal_mensuel_1250 + sal_mensuel_1250_1500),
                                  " % d\u00e9clarent gagner moins de 1 500 € nets par mois, et "),
                           paste0(round (sal_mensuel_3000), " % d\u00e9clarent gagner plus de 3 000 €. "),
                           paste0("Le salaire net médian \u00e0 temps complet s'élève \u00e0 ",
                                  format(as.numeric(sal_net_median_fin), big.mark = " ", digits = 0, scientific = FALSE),
                                  " € par mois en 2017-2019. "),
                           tags$br(),tags$br(),
                           "Au niveau national, la tension et les difficultés de recrutement en 2019 chez les",
                           tolower(obtenir_libelle(r_fap())),
                           "sont",
                           paste0(niveau_tension_national, ". "),
                           align="justify")
                  }
        else if (nchar(r_fap()) == 5 && is.numeric(tension_nat) == TRUE &&
                 nrow(sal_net_median) >= 1 && is.na(tra_sam) ){
                    tags$p("En 2017-2019, ce métier comprend :",
                           tags$br(),
                           tags$ul(
                             tags$li(HTML( str_replace_all(eff_annee_fin_arrondi_format, pattern=" ", "&nbsp;")),
                                     " personnes"), 
                             tags$li(paste0(round(part_femmes_2018), " % de femmes")), 
                             tags$li(paste0(round(structure_age_part_30_2018), " % de personnes de moins de 30 ans et ",
                                            round(structure_age_part_50_2018), " % de plus de 50 ans")),
                             tags$li(paste0(round(part_diplome_2018_part*100), " % d'entre elles ",
                                            texte_avec_diplome[[part_diplome_2018_diplome]],". "))
                           ),
                           "Dans cette profession, ",
                           paste0(round(tra_sam), " % des personnes travaillent le samedi, "),
                           paste0(round(tra_dim), " % le dimanche, et "),
                           paste0(round(tra_nuit), " % de nuit. "),
                           tags$br(),tags$br(),
                           "Parmi les salari\u00e9s \u00e0 temps complet, ",
                           paste0(round (sal_mensuel_1250 + sal_mensuel_1250_1500),
                                  " % d\u00e9clarent gagner moins de 1 500 € nets par mois, et "),
                           paste0(round (sal_mensuel_3000), " % d\u00e9clarent gagner plus de 3 000 €. "),
                           paste0("Le salaire net médian \u00e0 temps complet s'élève \u00e0 ",
                                  format(as.numeric(sal_net_median_fin), big.mark = " ", digits = 0, scientific = FALSE),
                                  " € par mois en 2017-2019. "),
                           tags$br(),tags$br(),
                           "Au niveau national, la tension et les difficultés de recrutement en 2019 chez les",
                           tolower(obtenir_libelle(r_fap())),
                           "sont",
                           paste0(niveau_tension_national, ". "),
                           align="justify")
                    
                }
        else if (nchar(r_fap()) == 5 && is.numeric(tension_nat) == FALSE &&
                 nrow(sal_net_median) >= 1 && !is.na(tra_sam)){
                      tags$p("En 2017-2019, ce métier comprend :",
                             tags$br(),
                             tags$ul(
                               tags$li(HTML( str_replace_all(eff_annee_fin_arrondi_format, pattern=" ", "&nbsp;")),
                                       " personnes"), 
                               tags$li(paste0(round(part_femmes_2018), " % de femmes")), 
                               tags$li(paste0(round(structure_age_part_30_2018), " % de personnes de moins de 30 ans et ",
                                              round(structure_age_part_50_2018), " % de plus de 50 ans")),
                               tags$li(paste0(round(part_diplome_2018_part*100), " % d'entre elles ",
                                              texte_avec_diplome[[part_diplome_2018_diplome]],". "))
                             ),
                             "Dans cette profession, ",
                             paste0(round(tra_sam), " % des personnes travaillent le samedi, "),
                             paste0(round(tra_dim), " % le dimanche, et "),
                             paste0(round(tra_nuit), " % de nuit. "),
                             tags$br(),tags$br(),
                             "Parmi les salari\u00e9s \u00e0 temps complet, ",
                             paste0(round (sal_mensuel_1250 + sal_mensuel_1250_1500),
                                    " % d\u00e9clarent gagner moins de 1 500 € nets par mois, et "),
                             paste0(round (sal_mensuel_3000), " % d\u00e9clarent gagner plus de 3 000 €. "),
                             paste0("Le salaire net médian \u00e0 temps complet s'élève \u00e0 ",
                                    format(as.numeric(sal_net_median_fin), big.mark = " ", digits = 0, scientific = FALSE),
                                    " € par mois en 2017-2019. "),
                             align="justify")
                    } 
        else if (nchar(r_fap()) == 5 && is.numeric(tension_nat) == TRUE &&
                 nrow(sal_net_median) < 1 && !is.na(tra_sam)){
                      tags$p("En 2017-2019, ce métier comprend :",
                             tags$br(),
                             tags$ul(
                               tags$li(HTML( str_replace_all(eff_annee_fin_arrondi_format, pattern=" ", "&nbsp;")),
                                       " personnes"), 
                               tags$li(paste0(round(part_femmes_2018), " % de femmes")), 
                               tags$li(paste0(round(structure_age_part_30_2018), " % de personnes de moins de 30 ans et ",
                                              round(structure_age_part_50_2018), " % de plus de 50 ans")),
                               tags$li(paste0(round(part_diplome_2018_part*100), " % d'entre elles ",
                                              texte_avec_diplome[[part_diplome_2018_diplome]],". "))
                             ),
                             "Dans cette profession, ",
                             paste0(round(tra_sam), " % des personnes travaillent le samedi, "),
                             paste0(round(tra_dim), " % le dimanche, et "),
                             paste0(round(tra_nuit), " % de nuit. "),
                             tags$br(),tags$br(),
                             "Au niveau national, la tension et les difficultés de recrutement en 2019 chez les",
                             tolower(obtenir_libelle(r_fap())),
                             "sont",
                             paste0(niveau_tension_national, ". "),
                             align="justify")} 
        else {
                               tags$p("En 2017-2019, ce métier comprend :",
                                      tags$br(),
                                      tags$ul(
                                        tags$li(HTML( str_replace_all(eff_annee_fin_arrondi_format, pattern=" ", "&nbsp;")),
                                                " personnes"), 
                                        tags$li(paste0(round(part_femmes_2018), " % de femmes")), 
                                        tags$li(paste0(round(structure_age_part_30_2018), " % de personnes de moins de 30 ans et ",
                                                       round(structure_age_part_50_2018), " % de plus de 50 ans")),
                                        tags$li(paste0(round(part_diplome_2018_part*100), " % d'entre elles ",
                                                       texte_avec_diplome[[part_diplome_2018_diplome]],". "))
                                      ),
                                      align="justify")
                             }
        })
      
      
      box_graphique(
        id = "effectif2",
        data = reactive({
          dat <- copy(effectifs)
          dat <- filtre_fap_detaillee(dat, r_fap())
          dat <- dat[nchar(fap) == 5]
          dat <- dat[annee >= 2004 ]
          ajout_cod_lib_fap_agregee(dat)
          ajout_pourcentage(dat,"effectif","annee")
          dat[,part := pourcentage]
          dat[,pourcentage:=NULL]
        }),
        graphique = reactive({
          dat <- copy(effectifs)
          dat <- filtre_fap_detaillee(dat, r_fap())
          dat <- dat[nchar(fap) == 5]
          dat <- dat[annee >= 2004 ]
          ajout_cod_lib_fap_agregee(dat)
          ajout_pourcentage(dat,"effectif","annee")
          dat[,part := pourcentage]
          dat[,pourcentage:=NULL]
          shiny::validate(
            need(dat$part, "Pas de donn\u00e9es \u00e0 visualiser pour cet indicateur")
          )
          dat[, annee_num := gsub("-.*$", "", annee)]
          dat[, annee_num := as.numeric(annee_num)]
          graph_courbes(
            data = dat, 
            x = as.Date(paste0(annee_num, "-01-01")),
            y = part, 
            group = libelle,
            aire = FALSE, 
            # couleurs = c("#113263", "#8e9594",)
            couleurs = scales::brewer_pal(palette = "Blues")(7)[-1],
            epaisseur = c(4, 4, 4, 4, 4, 4, 4)
          )
        })
      )
    
    }
  )
}


