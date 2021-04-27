
#  ------------------------------------------------------------------------
#
# Title : Portrait metier : tensions nouvelle version avec carte des départements
#    By : DARES
#  Date : 2020-07-17
#
#  ------------------------------------------------------------------------


panel_tensions_departement_ui <- function(id) {
  ns <- NS(id)
  tabPanel(
    title = "Tensions",
    value = "tensions_dep",
    class = "box-ombrage",
    conditionalPanel(
      condition = "output.niveau_tension_nat == 'indisponibles'",
      ns=ns,
      tags$div(
        tags$p("Ce métier ne donne pas lieu à une analyse des tensions. Celle-ci n'est pertinente que pour les métiers à forte dimension salariée")
      )
    ),
    conditionalPanel(
      condition = "output.niveau_tension_nat != 'indisponibles'",
      ns=ns,
      tags$div(
        tags$h3("Tensions"),
        tags$hr(),
        uiOutput(outputId = ns("tensions_dep_explications")),
        tags$h4("L'évolution des tensions"),
        box_graphique_UI(
          id = ns("tensions_evolution"),
          ombrage = FALSE,
          tableau = FALSE,
          height = "205px"
        ),
        tags$h4("Facteurs explicatifs des tensions"),
        box_graphique_UI(
          id = ns("tensions-radar"),
          sous_titre = "Plus la valeur est élevée, plus le facteur est susceptible d'expliquer la tension",
          ombrage = FALSE,
          tableau = FALSE,
          height = "405px"
        ),
        tags$h4("Carte des tensions"),
        selectInput(ns("select_geo_tension"),
                    "Niveau géographique",
                    choices = c("Région", "Département"),
                    multiple = FALSE),
        box_graphique_UI(
          id = ns("carte-dep-tensions"),
          titre = NULL,
          ombrage = FALSE,
          carte = TRUE,
          height = "505px"
        ),
        tags$p("Les zones grisées correspondent à des situations où les effectifs sont trop faibles pour mesurer la tension")
      )
    )
  )
}


panel_tensions_departement <- function(id, r_fap) {
  moduleServer(
    id,
    function(input, output, session) {
      
      niveau_geo_tension <- reactive({
        if (isTruthy(input$select_geo_tension)) {
          if (input$select_geo_tension == "Région") {
            "reg"
          }
          else if(input$select_geo_tension == "Département") {
            "dep"
            }
        }
      })
      
      output$niveau_tension_nat <- reactive({
        niveau_tension_nat()
      })
      outputOptions(output, "niveau_tension_nat", suspendWhenHidden = FALSE)
      
      niveau_tension_nat <- reactive({
        req(r_fap())
        tension_nat = tensions[fap == r_fap() & niveau_geo == "nat", "categorie_tension"]
        if (nrow(tension_nat)>0)
        {tension_nat = sum(tension_nat)
        switch(tension_nat, "rares", "faibles", "moyennes", "élevées", "très fortes")} else {"indisponibles"}
      })
      
      
      output$tensions_dep_explications <- renderUI({
          url <- a("Les tensions sur le marché du travail en 2019", 
                   href="https://dares.travail-emploi.gouv.fr/dares-etudes-et-statistiques/etudes-et-syntheses/dares-analyses-dares-indicateurs-dares-resultats/article/les-tensions-sur-le-marche-du-travail-en-2019")
          if (niveau_tension_nat() != "indisponibles"){
          tags$p(
                 "Au niveau national, la tension et les difficultés de recrutement en 2019 chez les",
          tolower(obtenir_libelle(r_fap())),
          "sont",
          paste0(niveau_tension_nat(),". "),
          HTML("<br>","<br>","Pour caractériser le déséquilibre entre les offres d’emploi émanant des entreprises et les demandes d’emploi
                en provenance des personnes en recherche d’emploi, la Dares et Pôle emploi ont élaboré un indicateur synthétique
                de tension, établi à fréquence annuelle depuis 2011 et décliné par métier, de l’échelon national au niveau départemental. 
                Cet indicateur prend en compte, pour chaque métier et dans chaque zone géographique, le niveau des difficultés de recrutement
                anticipées par les employeurs, les offres d’emploi rapportées au nombre de demandeurs d’emploi, et la facilité
                qu’ont les demandeurs d’emploi à sortir des listes de Pôle emploi. Une hausse de l’indicateur correspond à un accroissement
                des tensions.",
                "<br>","<br>","Cet indicateur synthétique est accompagné de six indicateurs complémentaires pour tenir compte des divers facteurs
                à l’origine des tensions. Ils permettent d’identifier les causes possibles des tensions
                et des difficultés de recrutement : fréquence élevée des besoins de recrutement, conditions de travail ou d’emploi peu attractives,
                manque de main-d’oeuvre disponible, décalage entre les compétences requises par les recruteurs et celles détenues
                par les personnes en recherche d’emploi, ou désajustement géographique entre la demande et l’offre de travail","<br>",
                "<br>","La carte interactive en bas de page vous permet de découvrir les disparités
                départementales et régionales de tension par métier
                et les indicateurs pour 13 régions, 100 départements et 186 métiers.","<br>",
                "<br>","Pour plus d'informations sur les tensions, vous pouvez recourir à la publication ci-contre&nbsp;:"
          ), tagList(url), align="justify")
          } else {tags$p(
          HTML("<br>","Pour un métier donné, les tensions peuvent être plus ou moins prononcées suivant les échelons géographiques. 
          Par exemple, si au niveau national, le métier d’ouvriers qualifiés de la peinture et de la finition du bâtiment est tendu en 2019, en Provence-Alpes-Côte d’Azur, 
          ce n’est le cas que dans deux départements sur six (Alpes-Maritimes et Var).","<br>",
          "Cette carte interactive vous permet de découvrir les disparités départementales et régionales de tension par métier
          et les indicateurs pour 13 régions, 100 départements et 186 métiers.","<br>",
                              "Pour plus d'informations sur les tensions, vous pouvez recourir à la publication ci-contre&nbsp;:"
          ), tagList(url))}
        })
      
      vars <- c("libelle", "non_durabilite_de_l_emploi", "conditions_de_travail_contraignantes", 
                "manque_de_main_d_oeuvre_disponible","intensite_d_embauches",
                "lien_formation_emploi",
                "inadequation_geographique"
      )
      
      vars_names = c(
        "Non-durabilité de l'emploi", "Conditions de travail contraignantes",
        "Manque de main d'oeuvre disponible", "Intensité d'embauches",
        "Lien formation emploi", "Inadequation géographique"
      )
      
      data_evol_tensions <- reactive({
        dat <- evol_tensions[fap %in% c(r_fap(),'ensemble'),]
        dat <- ajout_libelle(dat)
        return(dat)
      })
      
      box_graphique(
        id = "tensions_evolution",
        data = data_evol_tensions(),
        graphique = reactive({
          graph_courbes(
            data = data_evol_tensions(), x = annee,
            y = tension, group = libelle, 
            couleurs = c("#113263", "#ff887e"), #"#8e9594"),
            format_y = ""
          )
        })
      )
      
      box_graphique(
        id = "tensions-radar",
        data = NULL,
        graphique = reactive({
          dat <- tensions[niveau_geo == "nat" & fap == r_fap(),]
          dat <- dat[, ..vars]
          names(dat) <- c("libelle", vars_names)
          graph_radar(
            data = melt(dat, id="libelle"), 
            x = variable, y = value, group = libelle, 
            couleurs = c("#113263", "#ff887e"), #"#8e9594"),
            format_y = ""
          )
        })
      )
      
      data_tensions <- reactive({
        req(r_fap())
        req(niveau_geo_tension())
        dat <- tensions[fap %like% r_fap() & niveau_geo %like% niveau_geo_tension(), list(
          categorie_tension = round(mean(as.numeric(categorie_tension), na.rm = TRUE)),
          conditions_de_travail_contraignantes = round(mean(as.numeric(conditions_de_travail_contraignantes), na.rm = TRUE)),
          non_durabilite_de_l_emploi = round(mean(as.numeric(non_durabilite_de_l_emploi), na.rm = TRUE)),
          intensite_d_embauches = round(mean(as.numeric(intensite_d_embauches), na.rm = TRUE)),
          manque_de_main_d_oeuvre_disponible = round(mean(as.numeric(manque_de_main_d_oeuvre_disponible), na.rm = TRUE)),
          inadequation_geographique = round(mean(as.numeric(inadequation_geographique), na.rm = TRUE)),
          lien_formation_emploi = round(mean(as.numeric(lien_formation_emploi), na.rm = TRUE))
        ), by = eval(niveau_geo_tension())]
        dat$categorie_tension[is.na(dat$categorie_tension)] <- -1
        shiny::validate(
          need(!anyNA(dat$categorie_tension), "D\u00e9sol\u00e9, il n\'est pas possible d\'afficher cette carte.")
        )
        dat$categorie_tension[dat$categorie_tension == -1] <- NA
        dat[, categorie_tension_cat := ordered(
          factor(
            x = categorie_tension,
            levels = 1:5,
            labels = c("Pas de tension", "Faible tension", "Moyenne tension", "Tension", "Tension élevée")
          ),
          levels = c("Tension élevée", "Tension","Moyenne tension", "Faible tension", "Pas de tension")
        )]
        return(dat)
      })
      
      
      box_graphique( 
        id = "carte-dep-tensions",
        carte = TRUE,
        data = reactive({
          data_tensions()
        }),
        graphique = reactive({
          req(r_fap())
          req(niveau_geo_tension())
          dat_ <- data_tensions()
          niveau_geo_ = reactive({
            if (niveau_geo_tension() == "dep") {return("dep_dom")}
            return(niveau_geo_tension())
          })
          
          choroplethe(
            data = dat_,
            variable = "categorie_tension_cat",
            val_absolue = 'ad hoc tension',
            code_niveau = niveau_geo_tension(),
            niveau = niveau_geo_(),
            fun_pal = colorFactor,
            palette = "RdYlGn",
            titre_legende = "Niveau de Tension"
          )
        })
      )
      
    }
  )
}


