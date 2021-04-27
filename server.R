
#  ------------------------------------------------------------------------
#
# Title : Application Portraits statistiques des métiers - Server
#    By : Dares
#  Date : 2020
#
#  ------------------------------------------------------------------------

credentials <- data.frame(
  user = c(user_tension), # mandatory
  password = c(password_tension), # mandatory
  start = c("2019-04-15"), 
  expire = c(NA),
  admin = c(FALSE),
  comment = "Simple and secure authentification mechanism 
  for single ‘Shiny’ applications.",
  stringsAsFactors = FALSE
)


function(input, output, session) {

  # check_credentials returns a function to authenticate users
  res_auth <- secure_server(
    check_credentials = check_credentials(credentials)
  )
  output$auth_output <- renderPrint({
    reactiveValuesToList(res_auth)
  })
  
  if (!version_site_dares) {
    insertUI(
      selector = "body", 
      where = "beforeEnd",
      ui = bas_de_page("1.0"), 
      immediate = TRUE
    )
  }
  
  onBookmarked(function(url) {
    showModal(urlModal2(
      url = url, 
      title = "Lien vers cette page", 
      subtitle = "Ce lien enregistre l\'\u00e9tat actuel de cette application."
    ))
  })
  
  accueil_Server(id = "accueil", parent_session = session)
  
  portrait_metier_Server(id = "portrait")
  
  comparaison_metiers_Server(id = "comparaison")
  
  indicateurs_Server(id = "indicateurs", parent_session = session)
  
  feminisation_Server(id = "feminisation", parent_session = session)
  
  croissance_emp_Server(id = "croissance_emp", parent_session = session)
  
  age_Server(id = "age", parent_session = session)
  
  tpsousempl_Server(id = "tpsousempl", parent_session = session)
  
  cddinterim_Server(id = "cddinterim", parent_session = session)
  
  ind_tensions_Server(id = "ind_tensions", parent_session = session)
  
  setBookmarkExclude(c( 
    
    # selecteur de fap
      ## fap_nodes
      "portrait-fap-fap_nodes",
      "comparaison-fap_1-fap_nodes",
      "comparaison-fap_2-fap_nodes",
      ## btn_fap
      "comparaison-fap_2-btn_fap",
      "comparaison-fap_1-btn_fap_dropmenu",
      "portrait-fap-btn_fap_dropmenu",
      "portrait-fap-btn_fap",
      "comparaison-fap_2-btn_fap_dropmenu",
      "comparaison-fap_1-btn_fap",
      # recherche
      "portrait-fap-recherche",
      "comparaison-fap_2-recherche",
      "comparaison-fap_1-recherche",
      # effacer 
      "comparaison-fap_1-effacer",
      "comparaison-fap_2-effacer",
      "portrait-fap-effacer",
    
    ## les retour
    
    "age-retour_indicateur",
    "ind_tensions-retour_indicateur",
    "feminisation-retour_indicateur",
    "cddinterim-retour_indicateur",
    "tpsousempl-retour_indicateur",
    "croissance_emp-retour_indicateur",
    
    ##  les go_to
    
    "indicateurs-go_to_age",
    "indicateurs-go_to_tpsousempl",
    "indicateurs-go_to_doc",
    "accueil-go_to_metier",
    "indicateurs-go_to_tensions",
    "accueil-go_to_comparateur",
    "accueil-go_to_indicateurs",
    "indicateurs-go_to_croissance_emp",
    "indicateurs-go_to_cddinterim",
    "accueil-go_to_doc",
    "indicateurs-go_to_feminisation",
    
    ## effacer 
    
    ## les modal_tableau
    
    "portrait-caracteristiques-part-empl-reg-modal_tableau",
    "age-top-10-jeune-modal_tableau",
    "tpsousempl-top-10-temps-partiel-modal_tableau",
    "croissance_emp-top-10-augmentation-modal_tableau",
    "portrait-qualite-horaires-atypiques-modal_tableau",
    "portrait-marche_travail-diplome-demandeurs-emploi-modal_tableau",
    "portrait-mobilites-type-sortie-modal_tableau",
    "portrait-marche_travail-part-cat-abc-reg-modal_tableau",
    "portrait-description-effectif2-modal_tableau",
    "portrait-dynamique-effectif-modal_tableau",
    "feminisation-top-10-femme-modal_tableau",
    "cddinterim-top-10-cdd-modal_tableau",
    "feminisation-top-10-homme-modal_tableau",
    "portrait-tensions-evol-tensions-modal_tableau",
    "portrait-caracteristiques-evol-age-modal_tableau",
    "portrait-qualite-salaire-median-modal_tableau",
    "portrait-caracteristiques-specialite-diplome-modal_tableau",
    "portrait-qualite-structure-statut-modal_tableau",
    "tpsousempl-top-10-sous-emploi-modal_tableau",
    "ind_tensions-modal_tableau",
    "portrait-caracteristiques-part-femme-modal_tableau",
    "portrait-mobilites-evol-mouv-fap-modal_tableau",
    "portrait-tensions-entrees-pole-emploi-modal_tableau",
    "portrait-marche_travail-part-cat-a-reg-modal_tableau",
    "cddinterim-top-10-interim-modal_tableau",
    "age-top-10-senior-modal_tableau",
    "portrait-caracteristiques-niveau-diplome-30-modal_tableau",
    "portrait-caracteristiques-structure-age-modal_tableau",
    "portrait-tensions-sorties-pole-emploi-modal_tableau",
    "portrait-qualite-salaire-mensuel-modal_tableau",
    "portrait-caracteristiques-part-reg-fap-modal_tableau",
    "portrait-caracteristiques-evolution-diplome-modal_tableau",
    "portrait-employeur-secteur-employeurs-modal_tableau",
    "portrait-mobilites-part-cdd-embauche-modal_tableau",
    "portrait-marche_travail-age-demandeurs-emploi-modal_tableau",
    "portrait-tensions_departement-carte-dep-tensions-modal_tableau",
    "portrait-marche_travail-demandeurs-emploi-modal_tableau",
    "portrait-employeur-categories-employeurs-modal_tableau",
    "portrait-mobilites-structure-anciennete-modal_tableau",
    "portrait-caracteristiques-part-immigres-modal_tableau",
    "croissance_emp-top-10-baisse-modal_tableau",
    
    ## choix d'affichage
    
    "age-annee",
    "cddinterim-annee",
    "feminisation-annee",
    "tpsousempl-annee",
    "croissance_emp-annees_croissance",
    "croissance_emp-echelle"
  )
  
  )
}

### conservés dans l'url
# "main_nav",
# "portrait-fap",
# "portrait-nav_portrait",
# "comparaison-fap_1",
# "comparaison-fap_2",

# tous les ind_tensions
# portrait-tensions_departement-select_geo_tension


