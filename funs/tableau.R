
#' Affichage d'un tableau interactif
#'
#' @param data Un \code{data.table}.
#' @param noms_cols Noms des colonnes a afficher.
#' @param cols_percentage Noms des colonnes auxquelles appliquer un format pourcentage.
#'
#' @return un \code{htmlwidget}.
#' @export
#'
#' @examples
#' tableau(head(iris))
tableau <- function(data, noms_cols = NULL, cols_percentage = c("pourcentage", "part")) {
  
  if (!is.data.table(data))
    data <- as.data.table(data)
  
  cols_p <- intersect(names(data), cols_percentage)
  cols_num <- sapply(data, is.numeric)
  cols_num <- names(cols_num)[cols_num]
  cols_num <- setdiff(cols_num, c("annee", "date", cols_p))
  
  if ("tension" %in% names(data)) {
    data[, tension := tension * 1000]
    if ("date" %in% names(data)) {
      data[, date := as.Date(paste0(date, "01"), format = "%Y%m%d")]
    }
  }
  
  fr <- list(
    sProcessing = "Traitement en cours...", sSearch = "Rechercher&nbsp;:", 
    sLengthMenu = "Afficher _MENU_ &eacute;l&eacute;ments", 
    sInfo = "Affichage de l\'&eacute;l&eacute;ment _START_ &agrave; _END_ sur _TOTAL_ &eacute;l&eacute;ments", 
    sInfoEmpty = "Affichage de l\'&eacute;l&eacute;ment 0 &agrave; 0 sur 0 &eacute;l&eacute;ment", 
    sInfoFiltered = "(filtr&eacute; de _MAX_ &eacute;l&eacute;ments au total)", 
    sInfoPostFix = "", sLoadingRecords = "Chargement en cours...", 
    sZeroRecords = "Aucun &eacute;l&eacute;ment &agrave; afficher", 
    sEmptyTable = "Aucune donn&eacute;e disponible dans le tableau", 
    oPaginate = list(
      sFirst = "Premier", sPrevious = "Pr&eacute;c&eacute;dent", 
      sNext = "Suivant", sLast = "Dernier"
    ), 
    oAria = list(
      sSortAscending = ": activer pour trier la colonne par ordre croissant", 
      sSortDescending = ": activer pour trier la colonne par ordre d&eacute;croissant"
    )
  )
  data <- data[, .SD, .SDcols = grep(pattern = "v\\d{1,2}", x = names(data), value = TRUE, invert = TRUE)]
  if (is.null(noms_cols)) {
    data_noms <- copy(data)
    renomme_colonnes(data_noms)
    noms_cols <- colnames(data_noms)
  }
  if (!is.null(cols_p) && length(cols_p) > 0) {
    data[, (cols_p) := lapply(.SD, function(x) {
      if (max(x, na.rm = TRUE) > 1) {
        x / 100
      } else {
        x
      }
    }), .SDcols = cols_p]
  }
  
  DTout <- DT::datatable(
    data = data, 
    colnames = noms_cols,
    rownames = FALSE,
    class = "stripe hover compact",
    filter = "none", 
    options = list(
      scrollX = TRUE,
      searching = FALSE, 
      lengthChange = FALSE,
      language = fr
    )
  )
  
  if (!is.null(cols_p) && length(cols_p) > 0) {
    DTout <- DT::formatPercentage(
      table = DTout, columns = cols_p, 
      digits = 1, dec.mark = ",", mark = " "
    )
  }
  if (!is.null(cols_num) && length(cols_num) > 0) {
    DTout <- DT::formatRound(
      table = DTout, columns = cols_num, 
      digits = 0, mark = " "
    )
  }
  return(DTout)
}



renomme_colonnes <- function(x) {
  dico <- list(
    "fap" = "Code FAP", 
    "tr_age" = "tranche d\'\u00e2ge", 
    "annee_2012_2014" = "Ann\u00e9e 2012 - 2014", 
    "annee_2016_2018" = "Ann\u00e9e 2016 - 2018", 
    "age" = "\u00c2ge", 
    "annee" = "Ann\u00e9e", 
    "milliers" = "Milliers",
    "pourcentage" = "Pourcentage", 
    "total_fap" = "Total FAP",
    "part" = "Part",
    "anciennete" = "Anciennet\u00e9", 
    "type_employeur" = "Type d\'employeur", 
    "diplome" = "Dipl\u00f4me", 
    "annee_1982_1984" = "Ann\u00e9e 1982 - 1984", 
    "type_entree" = "Type d\'entr\u00e9e", 
    "type_mmo" = "Type de mouvement", 
    "taux" = "Taux", 
    "date" = "Date", 
    "tension" = "Tension", 
    "horaire_atypique" = "Horaires atypiques", 
    "salaire" = "Salaire",
    "categorie" = "Cat\u00e9gorie", 
    "effectifs" = "Effectifs",
    "type_sortie" = "Type de sortie",
    "code_reg" = "Code r\u00e9gion", 
    "emp_fap_reg" = "Emploi FAP r\u00e9gion", 
    "emp_reg" = "Emploi r\u00e9gion", 
    "variable" = "Variable", 
    "var_classe" = "Variable classe", 
    "nom_reg" = "Nom r\u00e9gion",
    "gid" = "Grid", 
    "emp_fap" = "Emploi FAP", 
    "emp_region" = "Emploi r\u00e9gion", 
    "salaire_median" = "Salaire m\u00e9dian",
    "secteur" = "Secteur", 
    "label_secteur" = "Label du secteur", 
    "tranche_dage" = "Tranche d\'\u00e2ge",
    "effectif" = "Effectif",
    "libelle" = "Libell\u00e9",
    "libelle_domaine" = "Libell\u00e9 du domaine",
    "code_fap_agregee" = "Code FAP agr\u00e9g\u00e9",
    "libelle_fap_agregee" = "Libell\u00e9 FAP agr\u00e9g\u00e9",
    "code_domaine" = "Code domaine",
    "dep" = "D\u00e9partement"
  )
  nom_vars <- names(dico)
  lib_vars <- unlist(dico, use.names = FALSE)
  var_renom <- intersect(nom_vars, names(x))
  if (length(var_renom) > 0) {
    setnames(x, var_renom, lib_vars[nom_vars %in% var_renom])
  }
  invisible(x)
}



