# Packages ----------------------------------------------------------------

library(CARTElette)
library(sf)
library(dplyr)
library(rmapshaper)



# correspondance region et departement 
reg_dep <- data.frame(reg_dep = c("11","11","11","11","11","11","11","11",
                                  "24","24","24","24","24","24",
                                  "27","27","27","27","27","27","27","27",
                                  "28","28","28","28","28",
                                  "32","32","32","32","32",
                                  "44","44","44","44","44","44","44","44","44","44",
                                  "52","52","52","52","52",
                                  "53","53","53","53",
                                  "75","75","75","75","75","75","75","75","75","75","75","75",
                                  "76","76","76","76","76","76","76","76","76","76","76","76","76",
                                  "84","84","84","84","84","84","84","84","84","84","84","84",
                                  "93","93","93","93","93","93",
                                  "94","94",
                                  "97","97","97","97"),
                      DEP  = c("75","77","78","91","92","93","94","95",
                               "18","28","36","37","41","45",
                               "21","25","39","58","70","71","89","90",
                               "14","27","50","61","76",
                               "02","59","60","62","80",
                               "08","10","51","52","54","55","57","67","68","88",
                               "44","49","53","72","85",
                               "22","29","35","56",
                               "16","17","19","23","24","33","40","47","64","79","86","87",
                               "09","11","12","30","31","32","34","46","48","65","66","81","82",
                               "01","03","07","15","26","38","42","43","63","69","73","74",
                               "04","05","06","13","83","84",
                               "2A","2B",
                               "971","972","973","974")
)
# Datas -------------------------------------------------------------------

fr_reg_dom <- CARTElette::charger_carte(nivsupra = "REG")
fr_reg <- filter(fr_reg_dom, !nom %in% c("Guadeloupe", "Martinique", "Guyane", "La R\u00e9union", "Mayotte"))

fr_dep_dom <- CARTElette::charger_carte(nivsupra = "DEP")
fr_dep_dom <- merge(fr_dep_dom, reg_dep)
fr_dep <- filter(fr_dep_dom, !nom %in% c("Guadeloupe", "Martinique", "Guyane", "La R\u00e9union", "Mayotte"))

plot(st_geometry(fr_dep))
# Simplification
plot(st_geometry(ms_simplify(fr_dep)))

fr_dep <- ms_simplify(fr_dep)
# Changement projection
fr_dep <- st_transform(fr_dep, crs = 4326)
saveRDS(fr_dep, file = "datas/cartes/fr_dep.rds")

fr_dep_dom <- ms_simplify(fr_dep_dom)
# Changement projection
fr_dep_dom <- st_transform(fr_dep_dom, crs = 4326)
saveRDS(fr_dep_dom, file = "datas/cartes/fr_dep_dom.rds")

fr_reg <- ms_simplify(fr_reg)
# Changement projection
fr_reg <- st_transform(fr_reg, crs = 4326)
saveRDS(fr_reg, file = "datas/cartes/fr_reg.rds")

fr_reg_dom <- ms_simplify(fr_reg_dom)
# Changement projection
fr_reg_dom <- st_transform(fr_reg_dom, crs = 4326)
saveRDS(fr_reg_dom, file = "datas/cartes/fr_reg_dom.rds")


#### Fonde de carte monde

world <- sf::st_read("inputs/immigration/carte_du_monde.geojson")
world$id <- as.character(world$id)
world$id[ world$id == "Afrique"] <- "Autres pays d'Afrique"
world$id[ world$id == "Algérie"] <- "Algérie"                                        
world$id[ world$id == "Amériques"] <- "Total Amerique, Oceanie"                                  
world$id[ world$id == "Asie"] <- "Autres pays d'Asie"                               
world$id[ world$id == "Cambodge, Laos, Vietnam"] <- "Cambodge, Laos, Vietnam"                  
world$id[ world$id == "Chine"] <- "Chine"                          
world$id[ world$id == "Espagne"] <- "Espagne"                                  
world$id[ world$id == "Europe hors Union Européenne"] <- "Autres pays d'Europe"               
world$id[ world$id == "France"] <- "France"
world$id[ world$id == "Italie"] <- "Italie"                                
world$id[ world$id == "Maroc"] <- "Maroc"                            
world$id[ world$id == "Océanie"] <- "Total Amerique, Oceanie"                                  
world$id[ world$id == "Portugal"] <- "Portugal"                                
world$id[ world$id == "Tunisie"] <- "Tunisie"                                
world$id[ world$id == "Turquie"] <- "Turquie"
world$id[ world$id == "Union européenne hors Espagne, Italie et Portugal"] <- "Autres pays de l'UE28"
world$nom <- world$id
world$nom  <- factor(world$nom)
map <- st_transform(world, crs= 4326)
plot(st_geometry(map))
map <- ms_simplify(map, keep=0.22)
plot(st_geometry(map))
saveRDS(map, file = "datas/cartes/world_map.rds")








