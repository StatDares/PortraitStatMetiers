# Projet ScreenJob

Le projet ScreenJob est un projet de datavisualisation des données de l'ouvrage Portraits Statistiques des métiers.

Cette publication dresse un portrait statistique des métiers de 1982 à 2019 selon de multiples dimensions : dynamique de l'emploi, caractéristiques des personnes en emploi, qualité de l'emploi, caractéristiques des employeurs, mobilité professionnelle, marché du travail. 85 regroupements de métiers et 225 métiers détaillés, constitués à partir de la nomenclature des familles professionnelles (FAP 2009), sont ainsi décrits sous la forme de tableaux, cartes et graphiques, assortis d'un bref commentaire.

La nomenclature des familles professionnelles (FAP) a été constituée par la Dares pour les besoins de l'analyse de l'emploi et du marché du travail par métiers. Elle opère un rapprochement entre le répertoire opérationnel des métiers et des emplois (Rome) utilisé par Pôle emploi pour classer les offres et demandes d'emploi, et les professions et catégories socioprofessionnelles (PCS) utilisées par l'Insee dans ses enquêtes.

Les données mobilisées pour dresser le portrait statistique de chaque regroupement de métiers sont issues de plusieurs sources : les enquêtes Emploi et le recensement de la population de l'Insee, la déclaration mensuelle des mouvements de main-d'oeuvre et l'enquête trimestrielle sur les mouvements de main-d'oeuvre (DMMO-EMMO) de la Dares, et les données de gestion de Pôle emploi sur les offres et les demandes d'emploi.

Outre une présentation des sources, puis des concepts et indicateurs sur lesquels s'appuient ces portraits statistiques dans la partie documentation, ce site est constituée d'un ensemble de modules thématiques par domaine professionnel et par métier. 


## Objectifs du projet

* Description : pour les nouvelles éditions des Portraits statistiques des métiers, simplifier la présentation de la publication Portraits Statistiques des Métiers qui contient de nombreuses informations et beaucoup de fiches métiers (87 FAP), pour la rendre plus ergonomique, plus parlante, plus facile d’accès aux utilisateurs
* Livrable associé : un site internet avec un outil de recherche par mots clés en tapant un libellé de profession et qui nous renvoie vers la FAP concernée. Et des outils de datavisualisation affichant tableaux et graphiques sur les thèmes, champs et années disponibles. 


## Auteurs

Alexis Eidelman, Christophe Michel, Département Dameth (ex DMQ) de la Dares et DreamRs.

Contacts : Christophe Michel et Alexis Eidelman


## Logiciel libre

Cette application a été réalisée avec le logiciel R (https://cran.r-project.org/) et les packages R suivant :

* [{shiny}](https://shiny.rstudio.com/) : Web Application Framework for R
* [{data.table}](https://github.com/Rdatatable/data.table) : Extension of `data.frame`
* [{leaflet}](https://github.com/rstudio/leaflet) : Create Interactive Web Maps with the JavaScript 'Leaflet' Library
* [{shinyWidgets}](https://github.com/dreamRs/shinyWidgets) : Custom Inputs Widgets for Shiny
* [{apexcharter}](https://github.com/dreamRs/apexcharter) : Create Interactive Chart with the JavaScript 'ApexCharts' Library
* [{CARTElette}](https://github.com/antuki/CARTElette) : Création de couches cartographiques à partir du code officiel géographique (COG) et des couches IGN
* [{fresh}](https://github.com/dreamRs/fresh) : Create Custom 'Bootstrap' Themes to Use in 'Shiny'
* [{DT}](https://github.com/rstudio/DT) : A Wrapper of the JavaScript Library 'DataTables'
* [{shinytreeview}](https://github.com/dreamRs/shinytreeview) : Hierarchical tree structures input for Shiny applications

