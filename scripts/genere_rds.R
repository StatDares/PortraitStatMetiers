
#  ------------------------------------------------------------------------
#
# Title : Alimentation donnees application
#    By : Victor
#  Date : 2020-02-10
#
#  ------------------------------------------------------------------------




# Packages ----------------------------------------------------------------

library(data.table)
library(janitor)
library(sf)

# Fonctions ---------------------------------------------------------------

invisible(lapply(
  X = list.files(path = "funs", full.names = TRUE),
  FUN = source, encoding = "UTF-8"
))



# Donnees FAP -------------------------------------------------------------


fap_data <- lecture_fap("inputs/")
fap_data <- ajout_libelle(fap_data)
saveRDS(object = fap_data, file = "datas/fap_data.rds")



# Donnees effectifs -------------------------------------------------------


effectifs <- lecture_effectif("inputs/effectifs_3niv.csv")
effectifs <- ajout_libelle(effectifs)

saveRDS(object = effectifs, file = "datas/effectifs.rds")


# Donnees tensions departements -------------------------------------------

## pour des raisons pratiques un code en python tension2screenjob.py doit tourner pour générer le csv appelé ici
tensions <- read.csv("inputs/tensions/tensions.csv", na = "Inf", encoding = "UTF-8")
tensions <- clean_names(tensions)

# ajoute le nom des département
nom_dep = readRDS(file = "datas/cartes/fr_dep_dom.rds")
st_geometry(nom_dep) <- NULL
colnames(nom_dep) = c("dep", "departement")
tensions <- merge(tensions, nom_dep, by = "dep", all.x = TRUE)

# ajoute la régions des département
fr_dep = readRDS(file = "datas/cartes/fr_dep.rds")
reg_du_dep = fr_dep[, c('DEP', 'reg_dep')]
colnames(reg_du_dep) = c("dep", "region_du_dep")
tensions <- merge(tensions, reg_du_dep, by = "dep", all.x = TRUE)

# ajoute le nom des régions
nom_reg = readRDS(file = "datas/cartes/fr_reg.rds")
st_geometry(nom_reg) <- NULL
colnames(nom_reg) = c("reg", "region")
tensions <- merge(tensions, nom_reg, by = "reg", all.x = TRUE)

tensions <-as.data.table(tensions)
# on met les DROM avec les départements et on les supprimes des régiones
tensions <- tensions[is.na(reg) | reg >= 6,]
# amélioration
tensions <- tensions[niveau_geo == "reg ", niveau_geo := "reg"]

tensions[which(niveau_geo=="dep" & region_du_dep == "11"),
         c("categorie_tension","conditions_de_travail_contraignantes",
           "inadequation_geographique","intensite_d_embauches",
           "lien_formation_emploi","manque_de_main_d_oeuvre_disponible",
           "non_durabilite_de_l_emploi","tension")] <- NA

tensions <- ajout_libelle(tensions)

saveRDS(object = tensions, file = "datas/tensions.rds")


evol_tensions <- read.csv("inputs/tensions/evol_tensions.csv", na = "Inf", encoding = "UTF-8")
evol_tensions <- clean_names(evol_tensions)

annee = c(2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019)
tension	= c(-0.005, 0.118, 0.018, -0.159, -0.248, -0.159, 0.053, 0.363, 0.494)
evol_tensions_ensemble <- data.frame(tension, annee)
evol_tensions_ensemble$fap <- 'ensemble'

evol_tensions <- rbind(evol_tensions_ensemble, evol_tensions)
evol_tensions <-as.data.table(evol_tensions)
saveRDS(object = evol_tensions, file = "datas/evol_tensions.rds")

# Keywords FAP 87 ---------------------------------------------------------

keywords_fap87 <- fread("inputs/keywords/keywords_fap87.csv", encoding = "UTF-8")
setnames(keywords_fap87, c("fap", "keywords"))
saveRDS(object = keywords_fap87, file = "datas/keywords_fap87.rds")

appellations_fap87 <- fread("inputs/keywords/appellations_fap87.csv", encoding = "UTF-8")
setnames(appellations_fap87, c("fap", "keywords"))
saveRDS(object = appellations_fap87, file = "datas/appellations_fap87.rds")


# Keywords FAP 225 --------------------------------------------------------

keywords_fap225 <- fread("inputs/keywords/keywords_fap225.csv", encoding = "UTF-8")
setnames(keywords_fap225, c("fap", "keywords"))
saveRDS(object = keywords_fap225, file = "datas/keywords_fap225.rds")

appellations_fap225 <- fread("inputs/keywords/appellations_fap225.csv", encoding = "UTF-8")
setnames(appellations_fap225, c("fap", "keywords"))
saveRDS(object = appellations_fap225, file = "datas/appellations_fap225.rds")

# Appellation Rome --------------------------------------------------------
appellations <- fread("inputs/keywords/appellations_rome.csv", encoding = "UTF-8")
setnames(appellations, c("rome", "appellations"))

saveRDS(object = appellations, file = "datas/appellations.rds")


#inflation Insee
inflation <- read.csv("inputs/inflation.csv")
saveRDS(object = inflation, file = "datas/inflation.rds")

