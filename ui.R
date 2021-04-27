
#  ------------------------------------------------------------------------
#
# Title : Application Portraits statistiques des métiers - UI
#    By : Dares
#  Date : 2020
#
#  ------------------------------------------------------------------------


UI_dares <- function(request, page_affichee, selected_item, fap, fap_compar){
  
  navbarPage(
    id = "main_nav",
    selected = page_affichee,
    
    title = tagList(
    ),
    windowTitle = "Portraits statistiques des métiers", 
    theme = "css/theme-dares.min.css",
    collapsible = TRUE,
    header = tagList(
      
      tags$link(
        rel = "stylesheet",
        type = "text/css",
        href = "css/styles.css"
      ),
      tags$script(src = "js/utils.js"),
      chooseSliderSkin("Flat")
    ),
    tabPanel(
      title = "Portrait du métier", 
      value = "portrait",
      portrait_metier_UI("portrait", fap, selected_item)
    ),
    tabPanel(
      title = "Comparateur de métiers",
      value = "comparateur",
      comparaison_metiers_UI("comparaison", fap, fap_compar)
    ),

    navbarMenu(
      title = "Indicateurs",
      tabPanel(
        title = "Aperçu",
        value = "indicateurs",
        indicateurs_UI("indicateurs")
      ),
      "----",
      tabPanel(
        title = "Mixité ",
        value = "feminisation",
        feminisation_UI("feminisation")
      ),
      tabPanel(
        title = "Croissance emploi ",
        value = "croissance_emp",
        croissance_emp_UI("croissance_emp")
      ),
      tabPanel(
        title = "\u00c2ge ",
        value = "age",
        age_UI("age")
      ),
      tabPanel(
        title = "Temps partiel et sous-emploi",
        value = "tpsousempl",
        tpsousempl_UI("tpsousempl")
      ), 
      tabPanel(
        title = "CDD et interim",
        value = "cddinterim",
        cddinterim_UI("cddinterim")
      ), 
      tabPanel(
        title = "Tensions",
        value = "ind-tensions",
        ind_tensions_UI("ind_tensions")
      )
    ),
    tabPanel(
      title = "Documentation", 
      value = "documentation",
      documentation_UI("documentation")
    )
  )
}

UI_independant <- function(request, page_affichee, selected_item, fap, fap_compar){
  navbarPage(
    id = "main_nav",
    selected = page_affichee,
    title = tagList(
      tags$img(
        src = "images/logo-dares.png", 
        class = "visible-xs-inline",
        style = "height: 30px; margin-top: 5px;"
      ),
      tags$img(
        src = "images/logo-dares.png", 
        class = "hidden-xs",
        style = "height: 45px;"
      )
    ),
    windowTitle = "Portraits statistiques des métiers", 
    theme = "css/theme-dares.min.css",
    collapsible = TRUE,
    header = tagList(
      
      tags$link(
        rel = "stylesheet",
        type = "text/css",
        href = "css/styles.css"
      ),
      tags$script(src = "js/utils.js"),
      chooseSliderSkin("Flat")
    ),
    tabPanel(
      title = "Accueil", 
      value = "accueil",
      accueil_UI("accueil")
    ),
    tabPanel(
      title = "Portrait du métier", 
      value = "portrait",
      portrait_metier_UI("portrait", fap, selected_item)
    ),
    tabPanel(
      title = "Comparateur de métiers",
      value = "comparateur",
      comparaison_metiers_UI("comparaison", fap, fap_compar)
    ),
    
    navbarMenu(
      title = "Indicateurs",
      tabPanel(
        title = "Aperçu",
        value = "indicateurs",
        indicateurs_UI("indicateurs")
      ),
      "----",
      tabPanel(
        title = "Mixité ",
        value = "feminisation",
        feminisation_UI("feminisation")
      ),
      tabPanel(
        title = "Croissance emploi ",
        value = "croissance_emp",
        croissance_emp_UI("croissance_emp")
      ),
      tabPanel(
        title = "\u00c2ge ",
        value = "age",
        age_UI("age")
      ),
      tabPanel(
        title = "Temps partiel et sous-emploi",
        value = "tpsousempl",
        tpsousempl_UI("tpsousempl")
      ), 
      tabPanel(
        title = "CDD et interim",
        value = "cddinterim",
        cddinterim_UI("cddinterim")
      ), 
      tabPanel(
        title = "Tensions",
        value = "ind-tensions",
        ind_tensions_UI("ind_tensions")
      )
    ),
    tabPanel(
      title = "Documentation", 
      value = "documentation",
      documentation_UI("documentation")
    )
  )
}

UI <- function(request) {
  
  page_affichee <- "accueil"
  if (version_site_dares){
    page_affichee <- "portrait"
  }
  selected_item <- "description"
  
  ## parsing de l'url
  query <- parseQueryString(request$QUERY_STRING)
  # fap dans l'url
  if (is.null(query$fap)) {
    fap <- "A0Z" # défaut
  } else {
    fap <- query$fap # si FAP précisé dans l'URL
    page_affichee <- "portrait"
  }
  # fap_compar dans l'url
  if (is.null(query$fap_compar)) {
    fap_compar <- "L0Z" # défaut
  } else {
    fap_compar <- query$fap_compar # si FAP précisé dans l'URL
    page_affichee <- "comparateur"
  }
  # module pour la page ou l'indicateur affiché
  if (!(is.null(query$module))){
      module = query$module
      if (module %in% c(onglets, indicateurs_existants)){
        page_affichee <- module
      }
      if (module %in% onglets_portait){
        page_affichee <- "portrait"
        selected_item <- module
      }
    }

    if (version_site_dares){
      UI_dares(request, page_affichee, selected_item, fap, fap_compar)
    }
    else {
      UI_independant(request, page_affichee, selected_item, fap, fap_compar)
    }
}


# if (!version_site_dares) {
#   UI <- secure_app(UI, language="fr")
# } else {
#   UI
# }
