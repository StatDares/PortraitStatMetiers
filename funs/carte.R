

#' Fond de carte des regions francaises
#'
#' @return un objet au format \code{sf}.
#' @export
#'
#' @examples
#' 
#' fr_reg <- france_carte()
#' plot(sf::st_geometry(fr_reg))
#' 
#' 
france_carte <- function(niveau = c("reg", "dep", "reg_dom", "dep_dom")) {
  niveau <- match.arg(niveau)
  if (!requireNamespace("sf"))
    stop("france_carte: Vous avez besoin du package {sf} pour utiliser cette fonction!")
  obj <- paste0("fr_", niveau)
  if (exists(obj, where = .GlobalEnv)) {
    return(get(obj, envir = .GlobalEnv))
  }
  
  find <- function(where = ".", what) {
    where <- normalizePath(where, mustWork = FALSE)
    if (dir.exists(file.path(where, what))) {
      return(file.path(where, what))
    } else {
      if (identical(dirname(where), where))
        stop("Not found !")
      find(dirname(where), what)
    }
  }
  datas <- find(what = "datas")
  
  if (file.exists(file.path(datas, paste0(obj, ".rds")))) {
    return(readRDS(file = file.path(datas, paste0(obj, ".rds"))))
  } else {
    source(file = file.path(find(what = "data-raw/"), paste0(obj, ".R")), local = TRUE)
    return(get(obj))
  }
}


#' Carte choroplethe de France
#'
#' @param data Donnees a representer sur la carte.
#' @param variable Nom de la variable utilise pour les couleurs de la carte.
#' @param val_absolue Nom de la variable contenant la valeur absolue du chiffre represente (pout le tooltip).
#' @param code_reg Nom de la variable contenant les codes regions.
#' @param titre_legende Titre de la legende.
#' @param palette Nom de la palette de couleur a utiliser.
#'
#' @return un \code{htmlwidget}.
#' @export
#' 
#' @importFrom leaflet colorNumeric leaflet leafletOptions addPolygons
#'  highlightOptions addLegend labelFormat
#' @importFrom scales brewer_pal
#' @importFrom htmltools HTML htmlDependency
#' @importFrom stats as.formula
#'
#' @examples
#' 
#' fap_data <- lecture_fap("inputs/")
#' data_carte1 <- fap_data$input_carte1
#' data_carte1 <- filtre_fap(data_carte1, c("A0Z"))
#' 
#' choroplethe(data_carte1, variable = "variable", val_absolue = "emp_reg")
#' 
choroplethe <- function(data, variable,
                        val_absolue = NULL,
                        val_tension = NULL,
                        val_tension_bis = NULL,
                        val_tension_ter = NULL,
                        code_niveau = "code_reg",
                        niveau = c("reg", "dep", "reg_dom", "dep_dom"),
                        titre_legende = "", 
                        fun_pal = colorNumeric, 
                        palette = scales::brewer_pal(palette = "Blues")(8)) {
  if (identical(NROW(data), 0L))
    return(NULL)
  niveau <- match.arg(niveau)
  fr_niv <- france_carte(niveau)
  by_x = "REG"
  if (niveau %in% c("dep", "dep_dom")) {by_x = "DEP"}
  fr_niv <- merge(
    x = fr_niv,
    y = data,
    by.x = by_x,
    by.y = code_niveau
  )
  pal <- fun_pal(
    palette = palette, 
    domain = fr_niv[[variable]],
    na.color = "#848484"
  )
 
  if (val_absolue == 'ad hoc tension'){
    tooltip <- sprintf(
    "~lapply(
    X = paste0(
      \"<b>\", nom, \"</b><br/>Indicateur de tension : \", %s,
      \"<br/>Conditions de travail contraignantes: \", %s,
      \"<br/>Non-durabilit&eacute; de l'emploi : \", %s,
      \"<br/>Intensit&eacute; d'embauches : \", %s,
      \"<br/>Manque de main d'oeuvre disponible : \", %s,
      \"<br/>Inad&eacute;quation g&eacute;ographique : \", %s,
      \"<br/>Lien formation emploi : \", %s
    ),
    FUN = HTML
  )", variable,
    "conditions_de_travail_contraignantes",
    "non_durabilite_de_l_emploi",
    "intensite_d_embauches", 
    "manque_de_main_d_oeuvre_disponible",
    "inadequation_geographique",
    "lien_formation_emploi"
    )
  } else if (!is.null(val_absolue)) {
    tooltip <- sprintf(
      "~lapply(
      X = paste0(
        \"<b>\", nom, \"</b><br/>Part : \", round(%s, 1), \"%%\",
        \"<br/>Nombre : \", format(round(%s), big.mark = \" \")
      ),
      FUN = HTML
    )", variable, val_absolue
    )
  } else {
    tooltip <- sprintf(
      "~lapply(
      X = paste0(
        \"<b>\", nom, \"<br/>Indicateur de tension : \", %s,\"</b>\",
        \"<br/>Indicateur condition travail : \", %s,
        \"<br/>Indicateur dur&eacute;e emploi : \", %s,
        \"<br/>Indicateur intensit&eacute; d'embauches : \", %s,
        \"<br/>Indicateur main d'oeuvre : \", %s
      ),
      FUN = HTML
    )", variable, val_absolue, val_tension, val_tension_bis, val_tension_ter
    )
  }
  
  m <- leaflet(
    data = fr_niv,
    options = leafletOptions(minZoom = 5, maxZoom = 8)
  ) %>% 
    addPolygons(
      weight = 1, 
      # color = ~pal(variable),
      color = "#FFF",
      opacity = 1, 
      fillColor = as.formula(sprintf("~pal(%s)", variable)),
      fillOpacity = 1,
      highlight = highlightOptions(
        weight = 3,
        color = "#FAFAFA",
        fillOpacity = 0.7,
        bringToFront = TRUE
      ),
      label = as.formula(tooltip)
    )
  if (!is.null(val_absolue) && val_absolue!="ad hoc tension") {
    m <- m %>%
      addLegend(
        position = "bottomright",
        pal = pal, 
        values = as.formula(paste0("~", variable)),
        title = titre_legende,
        labFormat = labelFormat(suffix = "%", big.mark = " "),
        opacity = 1,
        bins = 5
      )
  } else {
    
    if (is.factor(fr_niv[[variable]])) {
      labs <- levels(fr_niv[[variable]])
    } else {
      labs <- unique(fr_niv[[variable]])
      labs <- labs[!is.na(labs)]
      labs <- labs[order(nchar(labs), labs)]
    }
    cols <- pal(labs)
    m <- m %>%
      addLegend(
        position = "bottomright",
        colors = cols, 
        labels = labs,
        values = as.formula(paste0("~", variable)),
        title = titre_legende, 
        opacity = 1, 
        na.label = "Donnée manquante"
      )
  }
  m$dependencies <- list(
    htmltools::htmlDependency(
      name = "leaflet-container",
      version = "0.1.0",
      src = "",
      package = "leaflet",
      all_files = FALSE,
      head = "<style>.leaflet-container {background: #fff;}</style>"
    )
  )
  return(m)
}


#' Fond de carte des pays ou continents du monde
#'
#' @return un objet au format \code{sf}.
#' @export
#'
#' @examples
#' 
#' fr_reg <- france_carte()
#' plot(sf::st_geometry(fr_reg))
#' 
#' 
monde_carte <- function(niveau = c("map")) {
  niveau <- match.arg(niveau)
  if (!requireNamespace("sf"))
    stop("monde_carte: Vous avez besoin du package {sf} pour utiliser cette fonction!")
  obj <- paste0("world_", niveau)
  if (exists(obj, where = .GlobalEnv)) {
    return(get(obj, envir = .GlobalEnv))
  }
  
  find <- function(where = ".", what) {
    where <- normalizePath(where, mustWork = FALSE)
    if (dir.exists(file.path(where, what))) {
      return(file.path(where, what))
    } else {
      if (identical(dirname(where), where))
        stop("Not found !")
      find(dirname(where), what)
    }
  }
  datas <- find(what = "datas")
  
  if (file.exists(file.path(datas, paste0(obj, ".rds")))) {
    return(readRDS(file = file.path(datas, paste0(obj, ".rds"))))
  } else {
    source(file = file.path(find(what = "data-raw/"), paste0(obj, ".R")), local = TRUE)
    return(get(obj))
  }
}

#' Carte choroplethe du Monde
#'
#' @param data Donnees a representer sur la carte.
#' @param variable Nom de la variable utilise pour les couleurs de la carte.
#' @param val_absolue Nom de la variable contenant la valeur absolue du chiffre represente (pout le tooltip).
#' @param code_pays Nom de la variable contenant les codes pays.
#' @param titre_legende Titre de la legende.
#' @param palette Nom de la palette de couleur a utiliser.
#'
#' @return un \code{htmlwidget}.
#' @export
#' 
#' @importFrom leaflet colorNumeric leaflet leafletOptions addPolygons
#'  highlightOptions addLegend labelFormat
#' @importFrom scales brewer_pal
#' @importFrom htmltools HTML htmlDependency
#' @importFrom stats as.formula
#'
#' @examples
#' 
#' fap_data <- lecture_fap("inputs/")
#' data_carte1 <- fap_data$input_carte1
#' data_carte1 <- filtre_fap(data_carte1, c("A0Z"))
#' 
#' choroplethe(data_carte1, variable = "variable", val_absolue = "emp_reg")
#' 
choroplethe_monde <- function(data, variable,
                        val_absolue = NULL,
                        val_absolue_bis = NULL,
                        val_absolue_ter = NULL,
                        code_niveau = "code_pays",
                        niveau = c("map"),
                        titre_legende = "", 
                        fun_pal = colorNumeric, 
                        palette = scales::brewer_pal(palette = "Blues")(8)) {
  if (identical(NROW(data), 0L))
    return(NULL)
  niveau <- match.arg(niveau)
  world_niv <- monde_carte(niveau)
  world_niv <- mutate(world_niv, id = base::replace(id, id == "Afrique" , "Autres pays d'Afrique"))
  world_niv <- mutate(world_niv, id = base::replace(id, id == "Algérie", "Algérie"))                                        
  world_niv <- mutate(world_niv, id = base::replace(id, id == "Amériques", "Total Amerique, Oceanie"))                                   
  world_niv <- mutate(world_niv, id = base::replace(id, id == "Asie", "Autres pays d'Asie"))                                 
  world_niv <- mutate(world_niv, id = base::replace(id, id == "Cambodge, Laos, Vietnam", "Cambodge, Laos, Vietnam"))                    
  world_niv <- mutate(world_niv, id = base::replace(id, id == "Chine", "Chine"))                            
  world_niv <- mutate(world_niv, id = base::replace(id, id == "Espagne", "Espagne"))                                  
  world_niv <- mutate(world_niv, id = base::replace(id, id == "Europe hors Union Européenne", "Autres pays d'Europe"))                
  world_niv <- mutate(world_niv, id = base::replace(id, id == "France", "France"))
  world_niv <- mutate(world_niv, id = base::replace(id, id == "Italie", "Italie"))                                
  world_niv <- mutate(world_niv, id = base::replace(id, id == "Maroc", "Maroc"))                              
  world_niv <- mutate(world_niv, id = base::replace(id, id == "Océanie", "Total Amerique, Oceanie"))                                   
  world_niv <- mutate(world_niv, id = base::replace(id, id == "Portugal", "Portugal"))                                  
  world_niv <- mutate(world_niv, id = base::replace(id, id == "Tunisie", "Tunisie"))                                 
  world_niv <- mutate(world_niv, id = base::replace(id, id == "Turquie", "Turquie")) 
  world_niv <- mutate(world_niv, id = base::replace(id, id == "Union européenne hors Espagne, Italie et Portugal", "Autres pays de l'UE28"))
  
  
  by_x = "id"
  world_niv <- merge(
    x = world,
    y = data,
    by.x = by_x,
    by.y = "id",
    allow.cartesian = TRUE
  )
  pal <- fun_pal(
    palette = palette, 
    domain = world_niv[[variable]],
    na.color = "#848484"
  )
  
  
  tooltip <- sprintf(
    "~lapply(
      X = paste0(
        \"<b>\", nom, \"</b><br/>Part : \", round(%s, 1), \"%%\",
        \"<br/>Nombre : \", format(round(%s), big.mark = \" \"),
        \"<br/>Nombre total de travailleurs immigr&eacute;s du m&eacute;tier : \", format(round(%s), big.mark = \" \"),
        \"<br/>Nombre total de travailleurs non-immigr&eacute;s du m&eacute;tier : \", format(round(%s), big.mark = \" \")
      ),
      FUN = HTML
    )", variable, val_absolue, val_absolue_bis, val_absolue_ter
  )
  
  
  m <- leaflet(
    data = world_niv,
    options = leafletOptions(minZoom = 1.2, maxZoom = 5)
  ) %>% 
    addPolygons(
      weight = 1, 
      # color = ~pal(variable),
      color = "#FFF",
      opacity = 1, 
      fillColor = as.formula(sprintf("~pal(%s)", variable)),
      fillOpacity = 1,
      highlight = highlightOptions(
        weight = 3,
        color = "#FAFAFA",
        fillOpacity = 0.7,
        bringToFront = TRUE
      ),
      label = as.formula(tooltip)
    )
  
    m <- m %>%
      addLegend(
        position = "bottomright",
        pal = pal, 
        values = as.formula(paste0("~", variable)),
        title = titre_legende,
        labFormat = labelFormat(suffix = "%", big.mark = " "),
        opacity = 1,
        bins = 5
      )

  m$dependencies <- list(
    htmltools::htmlDependency(
      name = "leaflet-container",
      version = "0.1.0",
      src = "",
      package = "leaflet",
      all_files = FALSE,
      head = "<style>.leaflet-container {background: #fff;}</style>"
    )
  )
  return(m)
}


