La datavisualisation est disponible [ici](https://dares.travail-emploi.gouv.fr/donnees/portraits-statistiques-des-metiers).

# Portrait Statistique des Métiers

Les infirmiers sont-ils nombreux à avoir des horaires de travail atypiques ? Le nombre de cuisiniers demandeurs d’emploi baisse-t-il ces dernières années ? Quel est le salaire moyen des caissiers ?

Le portrait statistique des métiers fournit des cartes, des graphiques, des tableaux et des commentaires pour comprendre les caractéristiques d’un métier. Pour chacun des 225 métiers, découvrez : l’évolution de l’emploi, le salaire moyen, le niveau de diplôme, la qualité de l’emploi (contrats courts ou CDI, horaires de travail atypiques, …), la structure par âge, la part des femmes, la répartition géographique, le nombre de demandeurs d’emploi, les déséquilibres entre les offres et les demandes d’emploi par région, les caractéristiques des employeurs, etc.

Les données mobilisées pour dresser le portrait statistique de chaque regroupement de métiers sont issues de plusieurs sources : les enquêtes Emploi et le recensement de la population de l'Insee et les données de gestion de Pôle emploi sur les offres et les demandes d'emploi.

Outre une présentation des sources, puis des concepts et indicateurs sur lesquels s'appuient ces portraits statistiques dans la partie documentation, ce site est constituée d'un ensemble de modules thématiques par domaine professionnel et par métier. 

Pour toute remarque ou suggestion, il est possible d'utiliser le [formulaire de contact de la Dares](https://dares.travail-emploi.gouv.fr/contact)

## Architecture

Le repertoire a la structure d'une application [{shiny}](https://shiny.rstudio.com/).

* Les données initiales sont en format csv dans le dossiers /inputs
* les fichiers R du dossier /scripts permettent de les transformer en format rds utilisé par R.
* le code R est ensuite organisé comme suit :
  * les fichiers principaux (global.R, ui.R, server.R) sont à la racine /
  * ils appellent des fonctions-outils sont rangées dans le dossiers /funs
  * et les fichiers du dossiers /modules correspondant à chaque partie de la visualisation
  * eux-mêmes appellent les documents en format markdown du dossier /documentation

## Librairies utilisées

Cette application a été réalisée avec le logiciel [R](https://cran.r-project.org/) et les packages R suivant :

* [{shiny}](https://shiny.rstudio.com/) : Web Application Framework for R
* [{data.table}](https://github.com/Rdatatable/data.table) : Extension of `data.frame`
* [{leaflet}](https://github.com/rstudio/leaflet) : Create Interactive Web Maps with the JavaScript 'Leaflet' Library
* [{shinyWidgets}](https://github.com/dreamRs/shinyWidgets) : Custom Inputs Widgets for Shiny
* [{apexcharter}](https://github.com/dreamRs/apexcharter) : Create Interactive Chart with the JavaScript 'ApexCharts' Library
* [{CARTElette}](https://github.com/antuki/CARTElette) : Création de couches cartographiques à partir du code officiel géographique (COG) et des couches IGN
* [{fresh}](https://github.com/dreamRs/fresh) : Create Custom 'Bootstrap' Themes to Use in 'Shiny'
* [{DT}](https://github.com/rstudio/DT) : A Wrapper of the JavaScript Library 'DataTables'
* [{shinytreeview}](https://github.com/dreamRs/shinytreeview) : Hierarchical tree structures input for Shiny applications

