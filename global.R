
#  ------------------------------------------------------------------------
#
# Title : Application Portraits statistiques des métiers - Global
#    By : Dares
#  Date : 2020
#
#  ------------------------------------------------------------------------

version_prod <- TRUE
version_site_dares <- TRUE

# Packages ----------------------------------------------------------------

library(shiny)
library(data.table)
library(apexcharter) # remotes::install_github("dreamRs/apexcharter")
library(leaflet)
library(htmltools)
library(DT)
library(rlang)
library(scales)
library(shinyWidgets)
library(shinytreeview) # remotes::install_github("dreamRs/shinytreeview")
library(stringr)
library(shinymanager) # pour l'authentification

library(dplyr)

# options(OutDec = ",") # cause un bug dans la legende des cartes

enableBookmarking("url")
source("credits.R")


# Fonctions ---------------------------------------------------------------

invisible(lapply(
  X = list.files(path = "funs", full.names = TRUE),
  FUN = source, encoding = "UTF-8"
))



# Modules -----------------------------------------------------------------

invisible(lapply(
  X = list.files(path = "modules/", full.names = TRUE, recursive=TRUE),
  FUN = source, encoding = "UTF-8"
))


# Datas -------------------------------------------------------------------

# /!\ Pour mettre a jour les donnees : scripts/00_alimentation_data_app.R


# Donnees des indicateurs
fap_data <- readRDS(file = "datas/fap_data.rds")


# Effectif par code FAP
effectifs <- readRDS(file = "datas/effectifs.rds")

# Fond de carte
fr_reg <- readRDS(file = "datas/cartes/fr_reg.rds")
fr_reg_dom <- readRDS(file = "datas/cartes/fr_reg_dom.rds")
fr_dep <- readRDS(file = "datas/cartes/fr_dep.rds")
fr_dep_dom <- readRDS(file = "datas/cartes/fr_dep_dom.rds")

world <- readRDS(file = "datas/cartes/world_map.rds")



# Mots-clefs FAP 87
# keywords_fap87 <- readRDS(file = "datas/keywords_fap87.rds")
# keywords_fap225 <- readRDS("datas/keywords_fap225.rds")
# 
# keywords <- rbind(keywords_fap87, keywords_fap225)
# list_keywords <- unique(unlist(strsplit(keywords$keywords, split = ",")))

# TODO: on pourrait avoir un RDS qui fait déjà tout ça
appellations_fap87 <- readRDS(file = "datas/appellations_fap87.rds")
appellations_fap225 <- readRDS("datas/appellations_fap225.rds")

appellations_recherche <- rbind(appellations_fap87, appellations_fap225)
appellations_recherche <- appellations_recherche[, list(
  keywords = unlist(strsplit(keywords, split = ","))
), by = fap]

list_appellations <- unique(unlist(strsplit(appellations_recherche$keywords, split = ",")))
list_appellations <- sort(list_appellations)

# Table libelle FAP
libs_fap_ <- libelles_fap()
libs_fap_225_aussi_87 <- libs_fap_[(fap_detaillee == 1 & N == 1)]
libs_fap <- libs_fap_[!(fap_detaillee == 1 & N == 1)]
arbre_fap <- readRDS("datas/arbre_fap.rds")


# Table tensions
tensions <- readRDS(file = "datas/tensions.rds")
tensions[which(niveau_geo=="dep" & dep %in% c("75","77","78","91","92","93","94","95")),
         c("categorie_tension","conditions_de_travail_contraignantes",
           "inadequation_geographique","intensite_d_embauches",
           "lien_formation_emploi","manque_de_main_d_oeuvre_disponible",
           "non_durabilite_de_l_emploi","tension")] <- NA
libs_fap_tensions = libs_fap[libs_fap$fap %in% tensions$fap | nchar(as.character(libs_fap$fap)) == 1]
arbre_fap_tensions <- readRDS("datas/arbre_fap_tensions.rds")

evol_tensions = readRDS(file = "datas/evol_tensions.rds")


#  noms des menus -------------------------------------------------------------------
# Mettre à jour en cas de changement ! 
# TODO : faire un test...

onglets <- c("accueil", "portrait", "comparateur", "indicateurs", "documentation")
indicateurs_existants <- c("indicateurs", "feminisation", "croissance_emp", "age", "tpsousempl", "cddinterim")
onglets_portait <- c("description", "dynamique", "caracteristiques", "qualité", "employeurs", "mobilité", "marché", "tension")


# Inflations
inflation <- readRDS(file = "datas/inflation.rds")
