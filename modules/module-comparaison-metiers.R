
#  ------------------------------------------------------------------------
#
# Title : Module comparaison metiers
#    By : Dares
#  Date : 2020
#
#  ------------------------------------------------------------------------

phrase_intro <- tags$div(
  HTML(paste(tags$strong(tags$span(style="color:darkblue","Vous pouvez comparer un métier ou un domaine professionnel (menu déroulant de gauche) à un ou plusieurs métier(s) ou domaine(s) (menu déroulant de droite)")))))
if (version_prod == TRUE){
  phrase_intro <- tags$div(
    HTML(paste(tags$strong(tags$span(style="color:darkblue","Vous pouvez comparer un métier ou un domaine professionnel (menu déroulant de gauche) à un métier ou domaine (menu déroulant de droite)")))))
}

comparaison_metiers_UI <- function(id, fap, fap_compar) {
  ns <- NS(id)
  tagList(
    tags$div(
      class = "container-fluid",
      tags$div(
        class = "col-xs-12 col-sm-12 col-md-10 col-md-offset-1 col-lg-8 col-lg-offset-2",
        tags$h3("Comparaison de métiers", class = "text-center"),
        tags$h5(phrase_intro),
        tags$br(),
        fluidRow(
          column(
            width = 6, 
            selecteur_fap_UI(
              ns("fap_1"),
              arbre_fap = arbre_fap, 
              selection = fap,
              niveau = "87"
            )
          ),
          column(
            width = 6,
            selecteur_fap_UI(
              ns("fap_2"),
              arbre_fap = arbre_fap, 
              selection = fap_compar,
              niveau = "87",
              multiple = !version_prod
            )
          )
        ),
        tags$br(),
        tags$h4("Nombre de personnes en emploi en 2017-2019", class = "text-center"),
        uiOutput(outputId = ns("effectifs")),
        tags$br(),
        tags$h4("Dynamique de l'emploi", class = "text-center"),
        tags$br(),
        fluidRow(
          column(
            width = 6,
            tags$div(
              style = "padding: 3px 0 0 0; box-shadow: 0px 1px 22px -12px #607D8B; margin-bottom: 20px;",
              uiOutput(outputId = ns("titre_evol_1"), style = "padding: 15px 0 0 15px;"),
              apexchartOutput(outputId = ns("evol_1"), height = "100px")
            )
          ),
          column(
            width = 6,
            tags$div(
              style = "padding: 3px 0 0 0; box-shadow: 0px 1px 22px -12px #607D8B;",
              uiOutput(outputId = ns("titre_evol_2"), style = "padding: 15px 0 0 15px;"),
              apexchartOutput(outputId = ns("evol_2"), height = "100px")
            )
          )
        ),
        tags$br(), tags$br(),
        tags$h4("Part des femmes", class = "text-center"),
        fluidRow(
          column(
            width = 6,
            uiOutput(outputId = ns("part_femmes_1"))
          ),
          column(
            width = 6,
            uiOutput(outputId = ns("part_femmes_2"))
          )
        ),
        fluidRow(
          column(
            width = 6,align="center",
            uiOutput(outputId = ns("phrase_femmes_1"))
          ),
          column(
            width = 6,align="center",
            uiOutput(outputId = ns("phrase_femmes_2"))
          )
        ),
        tags$br(),
        tags$h4("Structure par âge en 2017-2019", class = "text-center"),
        apexchartOutput(outputId = ns("structure_age")),
        tags$br(),
        tags$h4("Salaire mensuel net", class = "text-center"),
        tags$h5("Les données sur les salaires n'apparaissent pas pour les métiers d'indépendants", class = "text-center"),
        apexchartOutput(outputId = ns("salaire"), height = "300px")
      )
    ),
    tags$br(), tags$br()
  )
}

comparaison_metiers_Server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      # Selecteur FAP ----
      
      r_fap_1 <- selecteur_fap(
        id = "fap_1",
        liste_mots_clefs = list_appellations,
        table_mots_clefs = appellations_recherche,
        table_libelles_fap = libs_fap
      )
      
      r_fap_2 <- selecteur_fap(
        id = "fap_2",
        liste_mots_clefs = list_appellations,
        table_mots_clefs = appellations_recherche,
        table_libelles_fap = libs_fap,
        multiple = !version_prod
      )

      datafap_1 <- reactive(filtre_fap(fap_data, r_fap_1()))
      datafap_2 <- reactive(filtre_fap(fap_data, r_fap_2()))
      ensemble <- filtre_fap(fap_data, "Ensemble")
      
      eff_1 <- reactive(filtre_fap(effectifs, r_fap_1()))
      eff_2 <- reactive(filtre_fap(effectifs, r_fap_2()))
      
      output$effectifs <- renderUI({
        comparaison_effectifs(
          r_fap_1() ,
          c(r_fap_2())
        )
      })
      
      output$titre_evol_1 <- renderUI({
        effectif_1 <- copy(eff_1())
        effectif_1[, evolution := (effectif - shift(effectif)) / shift(effectif)]
        derniere <- tail(effectif_1$evolution, 1)
        derniere <- round(derniere * 100)
        annee <- tail(effectif_1$annee, 1)
        if (derniere > 0) {
          derniere <- paste0("+", derniere, " %")
        } else {
          derniere <- paste0(derniere, " %")
        }
        tagList(
          tags$h4(derniere, "entre 2016-2018 et 2017-2019", style = "margin: 0; font-weight: bold;"),
          tags$h5("Evolution annuelle des effectifs (données en moyenne annuelle sur 3 ans de 1982-1984 à 2017-2019)")
        )
      })
      
      output$evol_1 <- renderApexchart({
        effectif_1 <- copy(eff_1())
        effectif_1[, evolution := (effectif - shift(effectif)) / shift(effectif)]
        apex(effectif_1[-1], aes(annee, evolution), type = "area",
             serie_name = "Evolution") %>% 
          ax_chart(
            sparkline = list(enabled = TRUE),
            fontFamily = "Arial, sans-serif",
            group = "comparaison-evolution-effectif",
            id = "comparaison-evolution-fap1"
          ) %>% 
          ax_colors("#113263") %>% 
          ax_tooltip(
            y = list(
              formatter = format_num(format = ".0%", locale = "fr-FR")
            )
          )
      })
      
      output$titre_evol_2 <- renderUI({
        effectif_2 <- copy(eff_2())
        effectif_2 <- as.data.table(effectif_2)
        effectif_2<-effectif_2[,.(fap = "Groupe",effectif=sum(effectif)), by = c("annee")]
        effectif_2[, evolution := (effectif - shift(effectif)) / shift(effectif)]
        derniere <- tail(effectif_2$evolution, 1)
        derniere <- round(derniere * 100)
        annee <- tail(effectif_2$annee, 1)
        if (derniere > 0) {
          derniere <- paste0("+", derniere, " %")
        } else {
          derniere <- paste0(derniere, " %")
        }
        tagList(
          tags$h4(derniere, "entre 2016-2018 et 2017-2019", style = "margin: 0; font-weight: bold;"),
          "Evolution annuelle des effectifs (données en moyenne annuelle sur 3 ans de 1982-1984 à 2017-2019)"
        )
      })
      
      output$evol_2 <- renderApexchart({
        effectif_2 <- copy(eff_2())
        effectif_2 <- as.data.table(effectif_2)
        effectif_2<-effectif_2[,.(fap = "Groupe",effectif=sum(effectif)), by = c("annee")]
        
        effectif_2[, evolution := (effectif - shift(effectif)) / shift(effectif)]
        apex(effectif_2[-1], aes(annee, evolution), type = "area") %>% 
          ax_chart(
            sparkline = list(enabled = TRUE),
            fontFamily = "Arial, sans-serif",
            group = "comparaison-evolution-effectif",
            id = "comparaison-evolution-fap2"
          ) %>% 
          ax_colors("#4e818f") %>% 
          ax_tooltip(
            y = list(
              formatter = format_num(format = ".0%", locale = "fr-FR")
            )
          )
      })
      
      
      # Dynamique de l'emploi ----
      output$part_femmes_1 <- renderUI({
        femmes <- datafap_1()$graph_part_femmes_3niv
        if (nrow(femmes) < 1) {
          femmes <- 0
        } else {
          femmes <- tail(femmes$part, 1)
        }
        femmes <- round(femmes)
        plot_waffle_icon(femmes, 100-femmes, color_1 = "#88cac0", color_2 = "#ff887e")
      })
      
      output$phrase_femmes_1 <- renderUI({
        femmes <- datafap_1()$graph_part_femmes_3niv
        if (nrow(femmes) < 1) {
          femmes <- 0
        } else {
          femmes <- tail(femmes$part, 1)
        }
        
        tags$h4(tags$b(round(femmes), "% de femmes"))
      })
      
      
      output$part_femmes_2 <- renderUI({
        femmes <- datafap_2()$graph_part_femmes_3niv
        #graph_part_femmes <-as.data.table(fap_data$graph_part_femmes)
        #femmes <- filtre_fap(graph_part_femmes_3niv, c(datafap_2()))
        femmes <- as.data.table(femmes)
        femmes[, annee_num := gsub("-.*$", "", annee)]
        femmes[, annee := (as.numeric(annee_num)+1)]
        femmes[, fap := tolower(fap)]
        #effectif_bis_2 <- filtre_fap(effectifs, c(fap_2()))
        effectif_bis_2 <-eff_2()
        effectif_bis_2 <- as.data.table(effectif_bis_2)
        effectif_bis_2[, fap := tolower(fap)]
        femmes_effectifs <- merge(femmes, effectif_bis_2, all.femmes = TRUE, by = c("annee","fap"))
        femmes_effectifs <- femmes_effectifs[,.(fap = "Groupe",part_sum=sum(part*effectif)/sum(effectif)), by = c("annee")]
        
        if (nrow(femmes_effectifs) < 1) {
          femmes_effectifs <- 0
        } else {
          femmes_effectifs <- tail(femmes_effectifs$part_sum, 1)
        }
        femmes_effectifs <- round(femmes_effectifs)
        plot_waffle_icon(femmes_effectifs, 100-femmes_effectifs, color_1 = "#88cac0", color_2 = "#ff887e")
      })
      
      output$phrase_femmes_2 <- renderUI({
        
        femmes <- datafap_2()$graph_part_femmes_3niv
        #graph_part_femmes <-as.data.table(fap_data$graph_part_femmes)
        #femmes <- filtre_fap(graph_part_femmes_3niv, c(datafap_2()))
        femmes <- as.data.table(femmes)
        femmes[, annee_num := gsub("-.*$", "", annee)]
        femmes[, annee := (as.numeric(annee_num)+1)]
        femmes[, fap := tolower(fap)]
        #effectif_bis_2 <- filtre_fap(effectifs, c(fap_2()))
        effectif_bis_2 <-eff_2()
        effectif_bis_2 <- as.data.table(effectif_bis_2)
        effectif_bis_2[, fap := tolower(fap)]
        femmes_effectifs <- merge(femmes, effectif_bis_2, all.femmes = TRUE, by = c("annee","fap"))
        femmes_effectifs <- femmes_effectifs[,.(fap = "Groupe",part_sum=sum(part*effectif)/sum(effectif)), by = c("annee")]
        
        if (nrow(femmes_effectifs) < 1) {
          femmes_effectifs <- 0
        } else {
          femmes_effectifs <- tail(femmes_effectifs$part_sum, 1)
        }
        femmes_effectifs <- round(femmes_effectifs)
        tags$h4(tags$b(round(femmes_effectifs), "% de femmes"))
      })
      
      # Structure par age ----
      
      output$structure_age <- renderApexchart({
        str_age_1 <- datafap_1()$graph_age_2017_2019_3niv
        str_age_1 <- as.data.table(str_age_1)
        str_age_1 <- str_age_1[, .(fap, tr_age, annee_2017_2019, libelle)]
        ajout_pourcentage(str_age_1,"annee_2017_2019",by="fap")
        str_age_1 <- str_age_1[, pourcentage := -1 * pourcentage]
        
        str_age_2 <- datafap_2()$graph_age_2017_2019_3niv
        str_age_2 <- as.data.table(str_age_2)
        if (version_prod == F) {
          
          str_age_2 <- str_age_2[,.(fap = "Groupe",
                                    annee_2017_2019_sum=sum(annee_2017_2019),
                                    libelle = "Groupe sélectionné"), 
                                 by = c("tr_age")]
          setnames(str_age_2, old = "annee_2017_2019_sum", new = "annee_2017_2019")
          ajout_pourcentage(str_age_2,"annee_2017_2019",by="fap")
        } else {
          str_age_2 <- str_age_2[, .(fap, tr_age, annee_2017_2019, libelle)]
          ajout_pourcentage(str_age_2,"annee_2017_2019",by="fap")
        }
      
        str_age <- rbind(str_age_1, str_age_2)#, str_age_ens)
        str_age <- str_age[order(-tr_age)]
        
        graph_barres_divergentes(
          data = str_age, 
          x = tr_age, y = pourcentage, group = libelle,
          couleurs = c("#113263", "#dcb233")#, "#8e9594")
        ) 
      })
      
      
      # Salaire net ----
      output$salaire <- renderApexchart({
        req(datafap_1())
        req(datafap_2())
        salaire_1 <- datafap_1()$graph_salaires_3niv
        salaire_1 <- salaire_1[, pourcentage_ := -1 * pourcentage]
        
        salaire_2 <- datafap_2()$graph_salaires_3niv
        salaire_2 <- as.data.table(salaire_2)
        if (version_prod == F) {
          salaire_2 <- salaire_2[,.(fap = "Groupe",
                                  milliers_sum=sum(milliers),libelle = "Groupe sélectionné"), 
                               by = c("salaire")]
          setnames(salaire_2, old = "milliers_sum", new = "milliers")
          ajout_pourcentage(salaire_2,"milliers",by="fap")
          salaire_1 <- salaire_1[, pourcentage_ := -1 * pourcentage][, pourcentage_ := 100 * pourcentage]
        }
        salaire_2 <- salaire_2[, pourcentage_ := pourcentage]
        
        
        salaires <- rbind(salaire_1, salaire_2)
        salaires <- ordonne_salaire(salaires)
        graph_barres_divergentes(
          data = salaires,
          x = salaire,
          y = pourcentage_ / 100, 
          group = libelle,
          auto_update = TRUE,
          couleurs = c("#113263", "#dcb233")#, "#8e9594")
        )
      })  
    }
  )
}


    # Fonctions graphiques avec icones ----------------------------------------
    
    
    
comparaison_effectifs <- function(fap_1, fap_2) {
  
  effectif_1 <- filtre_fap(effectifs, fap_1)
  effectif_1 <- as.data.table(effectif_1)
  effectif_2 <- filtre_fap(effectifs, c(fap_2))
  effectif_2 <- as.data.table(effectif_2)
  effectif_2<-effectif_2[,.(fap = "Groupe",effectif_sum=sum(effectif)), by = c("annee")]
  
  effectif_1 <- effectif_1$effectif
  effectif_2 <- effectif_2$effectif_sum
  
  effectifs_compares <- c(
    tail(effectif_1, 1),
    tail(effectif_2, 1)
  )
  
  # homme ou femme
    part_femme_1 <- filtre_fap(fap_data, fap_1)
    femmes <- part_femme_1$graph_part_femmes_3niv
    if (nrow(femmes) < 1) {
      femmes <- 0
    } else {
      femmes <- tail(femmes$part, 1)
    }
    if (femmes > 50){
      icon1 = "female"
      color_icon1 = "#88cac0"
      texteFH1 = ", majoritairement des femmes."
    } else {
      icon1 = "male"
      color_icon1 = "#ff887e"
      texteFH1 = ", majoritairement des hommes."
    }
    
    graph_part_femmes_3niv <-as.data.table(fap_data$graph_part_femmes_3niv)
    #graph_part_femmes[, .(V2, V3, V4)]
    #femmes<-datafap_2()$graph_part_femmes_3niv
    femmes <- filtre_fap(graph_part_femmes_3niv, c(fap_2))
    # femmes <- filtre_fap(graph_part_femmes_3niv, fap_2)
    femmes <- as.data.table(femmes)
    femmes[, annee_num := gsub("-.*$", "", annee)]
    femmes[, annee := (as.numeric(annee_num)+1)]
    femmes[, fap := tolower(fap)]
    effectif_bis_2 <- filtre_fap(effectifs, c(fap_2))
    effectif_bis_2 <- as.data.table(effectif_bis_2)
    effectif_bis_2[, fap := tolower(fap)]
    femmes_effectifs <- merge(femmes, effectif_bis_2, all.femmes = TRUE, by = c("annee","fap"))
    femmes_effectifs <- femmes_effectifs[,.(fap = "Groupe",part_sum=sum(part*effectif)/sum(effectif)), by = c("annee")]
    
    
    if (nrow(femmes) < 1) {
      femmes <- 0
    } else {
      femmes <- tail(femmes_effectifs$part_sum, 1)
    }
    if (femmes > 50){
      icon2 = "female"
      color_icon2 = "#88cac0"
      texteFH2 = ", majoritairement des femmes."
    } else {
      icon2 = "male"
      color_icon2 = "#ff887e"
      texteFH2 = ", majoritairement des hommes."
    }
  

  sizes <- scales::rescale(
    x = effectifs_compares, 
    from = c(0, max(scales::expand_range(effectifs_compares, mul = 0.2))),
    to = c(15, 280)
  )
  
  evolutions <- c(
    diff(tail(effectif_1, 2)) / tail(effectif_1, 1) * 100,
    diff(tail(effectif_2, 2)) / tail(effectif_2, 1) * 100
  )
  evolutions <- round(evolutions, 1)
  sizes_evolutions <- scales::rescale(
    x = evolutions, 
    from = c(0, max(scales::expand_range(evolutions, mul = 0.1))),
    to = c(15, 50)
  )
  sizes_evolutions <- round(sizes_evolutions)
  
  tags$div(
    style = "width: 100%;",
    tags$div(
      style = "display: grid;grid-template-columns: repeat(2, 1fr);grid-template-rows: 1fr;grid-column-gap: 0px;grid-row-gap: 0px;",
      style = "height: 300px",
      tags$div(
        style = "border-right: 1px solid #435363; position: relative;",
        tags$div(
          style = "text-align: right; position: absolute; bottom: 0; right: 30px; left: auto;",
          tagAppendAttributes(
            icon(icon1), 
            style = sprintf("color:%s;vertical-align: bottom; font-size: %spx;", color_icon1, round(sizes[1]))
          )
        )
      ),
      tags$div(
        style = "position: relative;",
        tags$div(
          style = "position: absolute; bottom: 0; left: 30px;",
          tagAppendAttributes(
            icon(icon2), 
            style = sprintf("color: %s; vertical-align: bottom; font-size: %spx;",color_icon2, round(sizes[2]))
          )
        )
      )
    ),
    tags$div(
      style = "display: grid;grid-template-columns: repeat(2, 1fr);grid-template-rows: 1fr;grid-column-gap: 0px;grid-row-gap: 0px;",
      tags$div(
        style = "text-align: right; padding-right: 15px;",
        tags$h4(
          paste(paste(format(as.numeric(effectifs_compares[1]) * 1000, big.mark = " ", digits = 0, scientific = FALSE),
          obtenir_libelle(fap_1)),texteFH1,sep="")
        )
      ),
      tags$div(
        tags$h4(
          paste(paste(format(as.numeric(effectifs_compares[2]) * 1000, big.mark = " ", digits = 0, scientific = FALSE), 
          if (length(fap_2) == 1){obtenir_libelle(fap_2)} else if (length(fap_2)>1) {"Groupe de Fap sélectionnées"}),
          texteFH2,sep="")
          )
      )
    )
  )
}


plot_waffle_icon <- function(freq_1, freq_2, color_1, color_2, icon_1 = "female", icon_2 = "male") {
  waffle <- htmltools::tags$div(
    class = "container-waffle-icons",
    htmltools::tags$div(
      shiny::icon(icon_1, class = "icon-waffle-1 icon-waffle"), ": Femmes |",
      shiny::icon(icon_2, class = "icon-waffle-2 icon-waffle"), ": Hommes"
    ),
    htmltools::tags$br(),
    lapply(
      X = seq_len(freq_1),
      FUN = function(i) {
        htmltools::tags$span(
          class = "icon-waffle-1 icon-waffle",
          shiny::icon(icon_1, class = "fa-2x") 
        )
      }
    ),
    lapply(
      X = seq_len(freq_2),
      FUN = function(i) {
        htmltools::tags$span(
          class = "icon-waffle-2 icon-waffle",
          shiny::icon(icon_2, class = "fa-2x")
        )
      }
    )
  )
  container_waffle_icons <- ".container-waffle-icons {width:370px;text-align:center;margin:0 auto;}"
  icon_waffle <- ".icon-waffle {width:14px;margin-bottom:5px;display:inline-block;}"
  icon_waffle_1 <- sprintf(".icon-waffle-1 {color: %s;}", color_1)
  icon_waffle_2 <- sprintf(".icon-waffle-2 {color: %s;}", color_2)
  waffle <- htmltools::tagList(
    htmltools::tags$style(
      container_waffle_icons,
      icon_waffle,
      icon_waffle_1,
      icon_waffle_2
    ),
    waffle
  )
  htmltools::browsable(waffle)
}








