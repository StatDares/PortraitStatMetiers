
#  ------------------------------------------------------------------------
#
# Title : Module indicateur tensions
#    By : Dares
#  Date : 2020
#
#  ------------------------------------------------------------------------


ind_tensions_UI <- function(id) {
  ns <- NS(id)
  
  fap225 <- libs_fap_tensions[fap_detaillee == 1]
  fap87 <- libs_fap_tensions[fap_agregee == 1]
  fap22 <- libs_fap_tensions[domaine == 1]
  fap87bis <- fap87
  fap225bis <- fap225
  fap225_87 <- fap87
  fap87_22 <- fap22
  
  tagList(
    tags$div(
      class = "container-fluid",
      tags$div(
        class = "col-xs-12 col-sm-12 col-md-10 col-md-offset-1 col-lg-8 col-lg-offset-2",
        tags$h3("Tensions", class = "text-center"),
        
        tags$br(),

        
        
        #############################################
        ######## choix du niveau géographique #######
        #############################################
        
        fluidRow(
          column(
            width = 4,
            pickerInput(
              inputId = ns("niveau_geo"),
              label = "Niveau géographique :", 
              choices = c('National', 'Régional', "Départemental"),
              selected = 'National',
              width = "100%"
            )
          ),
          column(
            width = 4,
            conditionalPanel(
              condition = "input.niveau_geo == 'Régional'",
              ns = ns,
              pickerInput(
                inputId = ns("niveau_geo_selection_reg"),
                label = "Sélectionner :",
                choices = c("une région", "toutes"),
                selected = "une région",
                multiple = FALSE,
                width = "100%"
              )
            ),
            conditionalPanel(
              condition="input.niveau_geo =='Départemental'",
              ns = ns,
              pickerInput(
                inputId = ns("niveau_geo_selection_dep"),
                label = "Mode de sélection :",
                choices = c("par département", "par région"),
                selected = "par département",
                multiple = FALSE,
                # options = pickerOptions(
                #   title = "Sélectionnez un département",
                #   selectAllText = "Tous",
                #   deselectAllText = "Tout effacer",
                #   actionsBox = TRUE
                # ),
                width = "100%"
              )            
            )
          ),
          column(
            width = 4,
            conditionalPanel(
              condition="input.niveau_geo_selection_dep == 'par département' && input.niveau_geo =='Départemental'",
              ns = ns,
              pickerInput(
                inputId = ns("choix_departement"),
                label = "Département :",
                choices = setNames(
                  fr_dep_dom$DEP,
                  fr_dep_dom$nom
                ),
                selected = fr_dep$DEP[1],
                multiple = FALSE,
                options = pickerOptions(
                  title = "Sélectionnez un département",
                  selectAllText = "Tous",
                  deselectAllText = "Tout effacer",
                  actionsBox = TRUE
                ),
                width = "100%"
              )            
            ),
            conditionalPanel(
              condition=paste0(
                "(input.niveau_geo_selection_dep == 'par région' && input.niveau_geo =='Départemental')",
                " || ",
                "(input.niveau_geo_selection_reg == 'une région' && input.niveau_geo =='Régional')"),
              ns = ns,
              pickerInput(
                inputId = ns("choix_region"),
                label = "Région :",
                choices = setNames(
                  fr_reg$REG,
                  fr_reg$nom
                ),
                selected = fr_reg$REG[2],
                multiple = FALSE,
                options = pickerOptions(
                  title = "Sélectionnez une Région",
                  maxOptions = 1
                ),
                width = "100%"
              )
            )
          )
        ),
        
        #############################################
        ########### choix du niveau métier ##########
        #############################################
        fluidRow(
          column(
            width = 4,
            pickerInput(
              inputId = ns("niveau_fap"),
              label = "Niveau de famille professionnelle :", 
              choices = c('Métiers agrégés (77 catégories)',
                          "Métiers (186 catégories)"),
              selected = 'Métiers (186 catégories)',
              width = "100%"
            )
          ),
          column(
            width = 4,
            pickerInput(
              inputId = ns("type_selection"),
              label = "Mode de sélection :", 
              choices = c('par domaine professionnel', 'par métier agrégé', 'tous'),
              selected = 'par domaine professionnel',
              width = "100%"
            )
          ),
          column(
            width = 4,
            conditionalPanel(
              condition="input.type_selection == 'par domaine professionnel'",
              ns=ns,
              pickerInput(
                inputId = ns("choix_fap22"),
                label = "Domaine professionnel :",
                choices = setNames(
                  fap87_22$fap,
                  fap87_22$libelle
                ),
                selected = fap87_22$fap[1],
                multiple = TRUE,
                options = pickerOptions(
                  title = "Sélectionner un domaine professionnel",
                  maxOptions = 1
                ),
                width = "100%"
              )
            ),
            conditionalPanel(
              condition="input.type_selection == 'par métier agrégé'",
              ns=ns,
              pickerInput(
                inputId = ns("choix_fap87"),
                label = "métier agrégé",
                choices = setNames(
                  fap87$fap,
                  fap87$libelle
                ),
                selected = fap87$fap[1],
                multiple = TRUE,
                options = pickerOptions(
                  title = "Sélectionnez une famille agrégée",
                  maxOptions = 1
                ),
                width = "100%"
              )
            ),
            
            conditionalPanel(
              condition="input.type_selection=='par métier simple'",
              ns = ns,
              pickerInput(
                inputId = ns("choix_fap225"),
                label = "Métier :",
                choices = setNames(
                  fap225$fap,
                  fap225$libelle
                ),
                selected = fap225$fap[1],
                multiple = TRUE,
                options = pickerOptions(
                  title = "Sélectionnez un métier",
                  maxOptions = 1
                ),
                width = "100%"
              )
            )
          )
        ),
        uiOutput(outputId = ns("phrase_dep_ile_de_france"), class = "h4"),
        tags$hr(),
        tags$p("Les effectifs affichés sont les effectifs établis à partir de l'enquête emploi 2016, répartis par région et département selon la répartion observée dans le recensement 2016."),
        tags$p("Les valeurs manquantes correspondent à des situations où les effectifs sont trop faibles pour mesurer la tension"),
        tags$p(HTML("<br>","Pour un métier donné, les tensions peuvent être plus ou moins prononcées suivant les échelons géographiques. 
          Par exemple, si au niveau national, le métier d’ouvriers qualifiés de la peinture et de la finition du bâtiment est tendu en 2019, en Provence-Alpes-Côte d’Azur, 
          ce n’est le cas que dans deux départements sur six (Alpes-Maritimes et Var).","<br>",
                    "Ce tableau interactif vous permet de découvrir les indicateurs de tension par métiers au niveau national, départemental ou régional. 
                    Il permet également de faire une sélection parmi 186 métiers au niveau détaillé ou 77 métiers agrégés.","<br>","<br>",
                    "Pour plus d'informations sur les tensions, vous pouvez recourir à la publication ci-contre&nbsp;:"
        ), tagList(a("Les tensions sur le marché du travail en 2019", 
                               href="https://dares.travail-emploi.gouv.fr/dares-etudes-et-statistiques/etudes-et-syntheses/dares-analyses-dares-indicateurs-dares-resultats/article/les-tensions-sur-le-marche-du-travail-en-2019"))),
        tagList(
          actionButton(
            inputId = ns("modal_tableau"), 
            label = NULL, 
            icon = icon("table"),
            style = "position: absolute; top: 10px; right: 5px;",
            class = "btn-sm"
          ),
          tags$hr()
        ),
        
        tags$div(
          class = "box-ombrage",
          tags$h5("Indicateurs tensions"),
          DT::DTOutput(outputId = ns("tableau"), height = "800px", width = "850px"),
          downloadButton(
            outputId = ns("download_tableau"), 
            label = "Télécharger les données affichées", 
            class = "btn-block"
          )
        ),
      
        actionButton(ns("retour_indicateur"), "Retour à l'aperçu"),
        retour_haut()
      )
    )
  )
}

ind_tensions_Server <- function(id, parent_session) {
  moduleServer(
    id,
    function(input, output, session) {

      
      observeEvent(input$retour_indicateur, {
        updateNavbarPage(
          session = parent_session, 
          inputId = "main_nav", 
          selected = "indicateurs"
        )
      })
      
      # si on veut choisir la fap 225 après la fap 87
      # observeEvent(input$fap87bis, {
      #   fap225 <- libs_fap[fap_detaillee == 1]
      #   fap225bis <- fap225[fap225$fap_87 == input$fap87bis, ]
      #   updatePickerInput(session = session, inputId = "fap225bis",
      #                    choices = setNames(fap225bis$fap, fap225bis$libelle)
      #                    )
      # }, ignoreInit = TRUE)
      output$phrase_dep_ile_de_france <- renderUI({
        if (input$choix_departement %in% c("91","92","75","77","93","94","95","78")) {
          HTML(paste0("</strong>Les indicateurs de tension ne sont pas présentés dans les d\u00e9partements de la r\u00e9gion \u00cele-de-France, car, plus qu'ailleurs et principalement à Paris, les offres d'emploi s'adressent aux demandeurs d’emploi de l'ensemble de la région ce qui empêche l'analyse des tensions au niveau départemental.</strong>"))
        }
      })
      
      observeEvent(input$niveau_fap, {
        choices_ = c('par domaine professionnel', 'par métier agrégé', 'tous')
        selected_ = 'par métier agrégé'
        if (input$niveau_fap == "Métiers (186 catégories)") {
          choices_ = c('par domaine professionnel', 'par métier agrégé', "par métier simple", "tous")
          selected_ = 'par métier simple'
        }
        if (input$type_selection == "par domaine professionnel"){
          selected_ = "par domaine professionnel"
        }
       
        updatePickerInput(session = session, inputId = "type_selection",
                          choices = choices_,
                          selected = selected_
        )
      }, ignoreInit = TRUE)
      
      observeEvent(input$choix_fap22,{
        selected22_ = input$choix_fap22[1]
        fap87_22 <- libs_fap_tensions[domaine == 1]
        updatePickerInput(session = session, inputId = "choix_fap22",
                          choices = setNames(
                            fap87_22$fap,
                            fap87_22$libelle
                          ),
                          selected = selected22_
        )
          }, ignoreInit = TRUE)
      
      observeEvent(input$choix_fap22,{
        
        fap87 <- libs_fap_tensions[fap_agregee == 1]
        fap87 <- as.data.table(fap87)
        selected87_ <- fap87[substr(fap,1,1) == input$choix_fap22[1]][1,fap]
        
        updatePickerInput(session = session, inputId = "choix_fap87",
                          choices = setNames(
                            fap87$fap,
                            fap87$libelle
                          ),
                          selected = selected87_
        )
      }, ignoreInit = TRUE)
      
      observeEvent(input$choix_fap22,{
        
        fap225 <- libs_fap_tensions[fap_detaillee == 1]
        fap225 <- as.data.table(fap225)
        selected225_ <- fap225[substr(fap,1,1) == input$choix_fap22[1]][1,fap]
        
        updatePickerInput(session = session, inputId = "choix_fap225",
                          choices = setNames(
                            fap225$fap,
                            fap225$libelle
                          ),
                          selected = selected225_
        )
      }, ignoreInit = TRUE)
      
      observeEvent(input$choix_fap87,{
        
        fap225 <- libs_fap_tensions[fap_detaillee == 1]
        fap225 <- as.data.table(fap225)
        fap22 <- libs_fap_tensions[domaine == 1]
        fap22 <- as.data.table(fap22)
        selected225_ <- fap225[substr(fap,1,3) == input$choix_fap87[1]][1,fap]
        selected22_ <- fap22[substr(fap,1,1) == substr(input$choix_fap87[1],1,1)][1,fap]
        
        updatePickerInput(session = session, inputId = "choix_fap225",
                          choices = setNames(
                            fap225$fap,
                            fap225$libelle
                          ),
                          selected = selected225_
        )
        
        updatePickerInput(session = session, inputId = "choix_fap22",
                          choices = setNames(
                            fap22$fap,
                            fap22$libelle
                          ),
                          selected = selected22_
        )
      }, ignoreInit = TRUE)
      
      observeEvent(input$choix_fap225,{
        
        fap87 <- libs_fap_tensions[fap_agregee == 1]
        fap87 <- as.data.table(fap87)
        fap22 <- libs_fap_tensions[domaine == 1]
        fap22 <- as.data.table(fap22)
        selected87_ <- fap87[substr(fap,1,3) == substr(input$choix_fap225[1],1,3)][1,fap]
        selected22_ <- fap22[substr(fap,1,1) == substr(input$choix_fap225[1],1,1)][1,fap]
        
        updatePickerInput(session = session, inputId = "choix_fap87",
                          choices = setNames(
                            fap87$fap,
                            fap87$libelle
                          ),
                          selected = selected87_
        )
        
        updatePickerInput(session = session, inputId = "choix_fap22",
                          choices = setNames(
                            fap22$fap,
                            fap22$libelle
                          ),
                          selected = selected22_
        )
      }, ignoreInit = TRUE)
      
      
      # si on veut choisir le département après la région
      # observeEvent(input$choix_region, {
      #   fr_dep_bis <- fr_dep[fr_dep$reg_dep == input$choix_region, ]
      #   updatePickerInput(session = session, inputId = "choix_departement_en_region",
      #                     choices = setNames(fr_dep_bis$DEP, dep_bis$nom)
      #                     )
      # }, ignoreInit = TRUE)
      
            
       niveau_geo_tension <- reactive({
         if (isTruthy(input$niveau_geo)) {
           if (input$niveau_geo == "Régional") {"reg"}
           else if(input$niveau_geo == "Départemental") {"dep"}
           else if(input$niveau_geo == "National") {"nat"}
         }
       })
  
       
       
      vars <- c("emploi_moyen", "tension", "categorie_tension", "non_durabilite_de_l_emploi", "conditions_de_travail_contraignantes", 
                "manque_de_main_d_oeuvre_disponible","intensite_d_embauches",
                "lien_formation_emploi",
                "inadequation_geographique"
                )
      vars_names = c(
        "Emploi moyen",
        "Tensions", "Catégorie de tension",
        "Non-durabilité de l'emploi", "Conditions de travail contraignantes",
        "Manque de main d'oeuvre disponible", "Intensité d'embauches",
        "Lien formation emploi", "Inadequation géographique"
      )
      
      colnames_ <- reactive({
        metier = "Métier"
        if (input$niveau_fap == 'Métiers agrégés (77 catégories)'){
          metier = "Métier agrégé"
        }
        columns = c(metier, vars_names)
        if (niveau_geo_tension() == 'dep'){
          columns = c(metier, "Département", vars_names)
        }
        if (niveau_geo_tension() == 'reg'){
          columns = c(metier, "Région", vars_names)
        }
        columns
      })
      
      data_tension_reactive <-  reactiveValues()
      
      observeEvent(input$fap225bis, {
        data_tension_reactive$last_pick <- "fap225bis"
      }) 
      observeEvent(input$fap87bis, {
        data_tension_reactive$last_pick <- "fap87bis"
      }) 
      observeEvent(input$fap87, {
        data_tension_reactive$last_pick <- "fap87"
      }) 
      observeEvent(input$fap22, {
        data_tension_reactive$last_pick <- "fap22"
      }) 
      observeEvent(input$fap225, {
        data_tension_reactive$last_pick <- "fap225"
      })
      observeEvent(input$toutfap225, {
        data_tension_reactive$last_pick <- "toutfap225"
      })
      observeEvent(input$toutfap87, {
        data_tension_reactive$last_pick <- "toutfap87"
      })
      observeEvent(input$fap225_87, {
        data_tension_reactive$last_pick <- "fap225_87"
      })
      observeEvent(input$fap87_22, {
        data_tension_reactive$last_pick <- "fap87_22"
      })
      
      tensions_r <- reactive({
        tensions_ <- copy(tensions)
        if (niveau_geo_tension() %in% c("reg","dep","nat")){
          tensions_geo <- tensions_[niveau_geo == niveau_geo_tension()]
        }
        
        if (niveau_geo_tension() == "reg" & input$niveau_geo_selection_reg == 'une région'){
          tensions_geo <- tensions_geo[reg %in% input$choix_region]
         }
        if (niveau_geo_tension() == "dep"){
          if (input$niveau_geo_selection_dep == 'par région'){
            tensions_geo <- tensions_geo[region_du_dep %in% input$choix_region]
          } else if (input$niveau_geo_selection_dep == 'par département') {
             tensions_geo <- tensions_geo[dep %in% input$choix_departement]
          }
        }
        
        #tensions_geo_toutfap87 <- tensions_geo
        if (input$niveau_fap == 'Métiers agrégés (77 catégories)'){
          tensions_geo_fap <- tensions_geo[niveau_fap == "fap 87"]
        } else {
          tensions_geo_fap <- tensions_geo[niveau_fap == "fap 225"]
        }
         
        if (input$type_selection == 'par domaine professionnel'){
          tensions_geo_fap <-tensions_geo_fap[fap %like% paste0("^", input$choix_fap22)]
        }
        if (input$type_selection == 'par métier agrégé'){
          tensions_geo_fap <-tensions_geo_fap[fap %like% paste0("^", input$choix_fap87)]
        }
        if (input$type_selection == "par métier simple"){
          tensions_geo_fap <-tensions_geo_fap[fap %like% paste0("^", input$choix_fap225)]
        }
        
        # if (!is.null(data_tension_reactive$last_pick) && data_tension_reactive$last_pick == "fap225"){
        #   tensions_geo <- tensions_geo[fap %like% paste0("^", input$fap225)]
        # } else if (!is.null(data_tension_reactive$last_pick) && data_tension_reactive$last_pick == "fap87"){
        #   tensions_geo <- tensions_geo[fap %like% paste0("^", input$fap87)]
        # } else if (!is.null(data_tension_reactive$last_pick) && data_tension_reactive$last_pick == "fap22"){
        #     tensions_geo <- tensions_geo[fap %like% paste0("^", input$fap22)]
        # } else if (!is.null(data_tension_reactive$last_pick) && data_tension_reactive$last_pick == "fap87bis"){
        #     tensions_geo <- tensions_geo[fap %like% paste0("^", input$fap87bis)]
        # } else if (!is.null(data_tension_reactive$last_pick) && data_tension_reactive$last_pick == "fap225bis") {
        #     tensions_geo <- tensions_geo[fap %like% paste0("^", input$fap225bis)]
        # } else if (!is.null(data_tension_reactive$last_pick) && data_tension_reactive$last_pick == "toutfap87" ) {
        #    tensions_geo <- tensions_geo_toutfap87
        # } else if (!is.null(data_tension_reactive$last_pick) && data_tension_reactive$last_pick == "toutfap225" ) {
        #    tensions_geo <- tensions_geo_toutfap225
        # } else if (!is.null(data_tension_reactive$last_pick) && data_tension_reactive$last_pick == "fap225_87" ) {
        #   
        #   tensions_geo <- tensions_geo[fap %like% paste0("^", input$fap225_87) & niveau_fap == "fap 225"] 
        # } else if (!is.null(data_tension_reactive$last_pick) && data_tension_reactive$last_pick == "fap87_22" ) {
        #   
        #   tensions_geo <- tensions_geo[fap %like% paste0("^", input$fap87_22) & niveau_fap == "fap 87"] 
        # }
        
        columns = c("fap", "libelle", vars)
        if (niveau_geo_tension() == 'dep'){
          columns = c("fap", "libelle", "departement", vars)
        }
        if (niveau_geo_tension() == 'reg'){
          columns = c("fap", "libelle", "region", vars)
        }
        
        tensions_geo_fap <- tensions_geo_fap[, .SD, .SDcols = columns]
        tensions_geo_fap[, (vars) := lapply(.SD, as.character), .SDcols = vars]
        tensions_geo_fap[, (vars) := lapply(.SD, as.numeric), .SDcols = vars]
        tensions_geo_fap <- tensions_geo_fap[libelle != fap] # Pour retirer les cas de Fap87 = fap225
        setcolorder(tensions_geo_fap, columns)
        tensions_geo_fap[, fap := NULL]
        
        tensions_geo_fap[, tension := round(tension, 2)]
        
        return(tensions_geo_fap)
      })
      
      output$tableau <- DT::renderDT({
        DT::datatable(
          data = tensions_r(), 
          rownames = FALSE,
          height = "800px",
          colnames = colnames_(),
          filter = "none", 
          fillContainer = TRUE,
          options = list(
            autoWidth = TRUE,
            columnDefs = (list(list(width = '85px', targets = '_all'))),
            initComplete = JS("
                        function(settings, json) {
                          $(this.api().table().header()).css({
                          'font-size': '10px',
                          });
                        }
                    "),
            scrollX = FALSE,
            scrollY = FALSE,
            searching = FALSE, 
            paginate = FALSE,
            info = FALSE,
            lengthChange = FALSE
            # ,language = fr
          ),width = '850px'
        ) %>%
          formatStyle(
            columns = vars,
            textAlign = "center",
            backgroundColor = styleEqual(
              1:5, rev(scales::brewer_pal(palette = "RdYlGn")(5))
            ),
            color = styleEqual(
              1:5, c("#FFF", "#000", "#000", "#000", "#FFF")
            )
          ) %>% 
          formatRound(c("emploi_moyen"),mark=" ",digits = 0) %>%
          formatRound(c("tension"), dec.mark = ",") %>%  
          formatStyle(columns = colnames(.$x$data), `font-size` = '10px')
      })
      
      observeEvent(input$modal_tableau, {
        showModal(modalDialog(
          title = tagList(
            tags$span(id = ns("titre_modal")),
            tags$button(
              icon("close"), 
              class = "btn btn-default pull-right",
              style = "border: 0 none;",
              `data-dismiss` = "modal"
            )
          ),
          size = "l",
          easyClose = TRUE,
          footer = NULL,
          DT::DTOutput(outputId = ns("tableau")),
          tags$br(),
          downloadButton(
            outputId = ns("download_tableau"), 
            label = "Télécharger le tableau", 
            class = "btn-block"
          ),
          tags$script(sprintf("ajoutTitre('%s');", ns("")))
        ))
      })
      
      output$download_tableau <- downloadHandler(
        filename = function() {
          paste0("data-tensions-", Sys.Date(), ".xlsx")
        },
        content = function(file) {
          
          data <- tensions_r()
          names(data) <- colnames_()
          if (inherits(data, "datatables")) {
            data <- data$x$data
          }
          
          writexl::write_xlsx(data, file)
        }
      )
    }
  )
  
}
    
    
