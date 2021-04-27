
#  ------------------------------------------------------------------------
#
# Title : Selecteur FAP
#  Date : 2020-05-15
#
# On doit faire tourner le debut de global.R avant de faire tourner ce programme
#  ------------------------------------------------------------------------


source("global.R", chdir = TRUE)



# Fonction pour construire la liste des choix necessaire a treeviewInput
construire_choix_fap <- function(table_libelles_fap, niveau = c("225", "87")) {
  
  niveau <- match.arg(niveau)
  
  fap22 <- unique(table_libelles_fap[domaine == 1, c(fap)])
  fap87 <- unique(table_libelles_fap[fap_agregee == 1, c(fap)])
  fap225 <- unique(table_libelles_fap[fap_detaillee == 1, c(fap)])
  
  choix_fap <- lapply(
    X = fap22,
    FUN = function(f22) {
      list(
        text = table_libelles_fap[fap == f22, c(libelle)],
        id = table_libelles_fap[fap == f22, c(fap)],
        selectable = FALSE,
        nodes = lapply(
          X = grep(pattern = paste0("^", f22), x = fap87, value = TRUE),
          FUN = function(f87) {
            res <- list(
              text = table_libelles_fap[fap == f87, c(libelle)],
              id = table_libelles_fap[fap == f87, c(fap)]
            )
            if (identical(niveau, "225") & table_libelles_fap[fap == f87, c(N)] > 1) {
              res$nodes <- lapply(
                X = grep(pattern = paste0("^", f87), x = fap225, value = TRUE),
                FUN = function(f225) {
                  list(
                    text = table_libelles_fap[fap == f225, c(libelle)],
                    id = table_libelles_fap[fap == f225, c(fap)]
                  )
                }
              )
            }
            res
          }
        )
      )
    }
  )
}

libs_fap <- libelles_fap()
libs_fap <- libs_fap[!(fap_detaillee == 1 & N == 1)]
libs_fap <- libs_fap[!libelle == "Tous métiers"]
arbre_fap <- construire_choix_fap(libs_fap, "225")
saveRDS(arbre_fap, file = "datas/arbre_fap.rds")

arbre_fap_tension <- construire_choix_fap(libs_fap_tensions, "225")
saveRDS(arbre_fap_tension, file = "datas/arbre_fap_tensions.rds")




