library(shiny)

source("modules/mod_questionnaire_patient.R")
source("utils.R")
kine_choices <- c(
  # APS
  "JP_APS" = "Deneuville JP (Atelier Physio Sport)",
  "Leif_APS" = "Leif Steen (Atelier Physio Sport)",
  "Audrey_APS" = "Audrey Raillard (Atelier Physio Sport)",
  "Martin_APS" = "Martin Meyer (Atelier Physio Sport)",
  "Mathilde_APS" = "Mathilde Rousseau (Atelier Physio Sport)",
  "Adrien_APS" = "Adrien Harlé (Atelier Physio Sport)",
  "Alexandra_APS" = "Alexandra Cervantes (Atelier Physio Sport)",
  "Juliette_APS" = "Juliette Cavalier (Atelier Physio Sport)",
  "Victor_APS" = "Victor Lebrault (Atelier Physio Sport)",
  "Romain_APS" = "Romain Deguine (Atelier Physio Sport)",
  "Colonier_APS" = "Colonier Sylvain (Atelier Physio Sport)",
  "Clemence_APS" = "Clémence Hamel (Atelier Physio Sport)",
  "Axel_APS" = "Axel Kremmer (Atelier Physio Sport)",
  "Antoine_APS" = "Antoine Massuleau (Atelier Physio Sport)",
  "Anthony_APS" = "Anthony Demont (Atelier Physio Sport)",
  "Marie_APS" = "Marie Akrich (Atelier Physio Sport)",
  # MSV
  "Clement_MSV" = "Clément Perrin (Maison de santé Villejuif)",
  "Joseph_MSV" = "Joseph Verrier (Maison de santé Villejuif)",
  "Romain_MSV" = "Romain Artico (Maison de santé Villejuif)",
  "Brice_MSV" = "Brice Leite George (Maison de santé Villejuif)",
  "Olivier_MSV" = "Olivier Daudier (Maison de santé Villejuif)"
)

get_cabinet_from_kine <- function(kine_id) {
  if (grepl("_APS$", kine_id)) return("APS")
  if (grepl("_MSV$", kine_id)) return("MSV")
  return("GENERIC")
}

get_logo_for_cabinet <- function(cabinet) {
  if (cabinet == "APS") return("logo_aps.jpg")
  if (cabinet == "MSV") return("logo_mspv.png")
  return("logo.png")
}

get_bg_color_for_cabinet <- function(cabinet) {
  if (cabinet == "APS") return("#FFD7A6")
  if (cabinet == "MSV") return("#F1F3F0")
  return("#F0F7FE")
}

format_identite <- function(patient) {
  taille <- patient$taille
  poids <- patient$poids
  imc <- round(as.numeric(poids) / (as.numeric(taille)/100)^2, 1)
  statut <- tolower(patient$statut)
  metier <- patient$metier
  pcs <- gsub("^PCS", "", substr(patient$pcs, 1, 5))
  
  if (statut == "étudiant") {
    affichage_metier <- "Étudiant"
  } else if (statut %in% c("actif", "retraité")) {
    affichage_metier <- paste0(metier, " (PCS", pcs, " - ", statut, ")")
  } else {
    affichage_metier <- metier
  }
  
  HTML(paste0(
    "<div style='background:white; padding:20px; border-radius:10px; box-shadow:0 2px 5px rgba(0,0,0,0.1); font-size:18px; max-width:1000px; margin-left:0;'>",
    "<h4 style='font-weight:bold;'>Identité</h4>",
    "<div style='margin-bottom:8px;'>Taille : <b>", taille, " cm</b></div>",
    "<div style='margin-bottom:8px;'>Poids : <b>", poids, " kg</b></div>",
    "<div style='margin-bottom:8px;'>IMC : <b>", imc, "</b></div>",
    "<div style='margin-bottom:8px;'>Métier : <b>", affichage_metier, "</b></div>",
    "</div>"
  ))
}
encart_titles <- c(
  "Antécédent médicaux", "Traitement antérieur", "Drapeaux rouges",
  "Évaluation multiparamétrique", "Chimique versus mécanique", "Narration de l'anamnèse",
  "Bilan postural", "Perte de mouvement de la colonne", "Bilan de hanche/épaule",
  "Bilan du bassin", "Autre test", "Bilan neurologique", "Bilan musculaire",
  "Imagerie", "Mouvements répétés", "Diagnostique final", "Prescirption",
  "Sánce de suvit", "Suivit injection", "Conclusion ou ré-adressage"
)

format_encart <- function(title) {
  HTML(paste0(
    "<div class='encart-box'>",
    "<h4 style='font-weight:bold;'>", title, "</h4>",
    "<p style='color:#999;'>Contenu à compléter...</p>",
    "</div>"
  ))
}

ui <- fluidPage(uiOutput("main_ui"))

server <- function(input, output, session) {
  mode <- reactiveVal("kine")
  logged_in <- reactiveVal(FALSE)
  current_logo <- reactiveVal("logo.png")
  current_bg <- reactiveVal("#F0F7FE")
  
  observe({
    query <- parseQueryString(session$clientData$url_search)
    if (!is.null(query$mode) && query$mode == "patient") {
      mode("patient")
    } else {
      mode("kine")
    }
  })
  
  output$main_ui <- renderUI({
    bg_color <- current_bg()
    logo_path <- current_logo()
    
    tagList(
      tags$head(tags$style(HTML(paste0("
        body { background-color: ", bg_color, "; }
        .login-logo { height: 160px; max-width: 420px; object-fit: contain; margin: 20px auto; display: block; }
        .login-box { background-color: white; padding: 40px; border-radius: 16px; box-shadow: 0 4px 10px rgba(0, 0, 0, 0.1); max-width: 400px; margin: 0 auto; text-align: center; }
        .centered-container { display: flex; flex-direction: column; justify-content: center; align-items: center; height: 100vh; }
        .nav-tabs > li > a { color: black !important; }
      ")))),
      
      if (mode() == "patient") {
        tagList(
          tags$head(tags$style(HTML(".login-logo { display: none; }"))),  # Masque tout logo venant de app.R
          mod_questionnaire_patient_ui("qs_patient")
        )
      }
      else if (!logged_in()) {
        div(class = "centered-container",
            img(src = logo_path, class = "login-logo"),
            div(class = "login-box",
                div(class = "login-title", "Accès Kiné"),
                selectizeInput("kine_login", "Choisissez votre identifiant kiné",
                               choices = setNames(c("", names(kine_choices)), c("", kine_choices)),
                               selected = NULL,
                               options = list(placeholder = 'Sélectionnez ou tapez votre nom')),
                actionButton("btn_login", "🔓 Connexion")
            )
        )
      } else {
        tagList(
          img(src = logo_path, class = "login-logo"),
          div(style = "width: 100%; display: flex; flex-direction: column; padding: 20px;",
              uiOutput("kine_interface")
          )
        )
      }
    )
  })
  
  observeEvent(input$btn_login, {
    if (input$kine_login != "") {
      logged_in(TRUE)
      cabinet <- get_cabinet_from_kine(input$kine_login)
      current_logo(get_logo_for_cabinet(cabinet))
      current_bg(get_bg_color_for_cabinet(cabinet))
      
      output$kine_interface <- renderUI({
        tabsetPanel(
          tabPanel("🔍 Rechercher un patient",
                   textInput("search_nom", "Nom"),
                   textInput("search_prenom", "Prénom"),
                   textInput("search_naissance", "Date de naissance (JJ/MM/AAAA)"),
                   actionButton("btn_search", "🔍 Rechercher"),
                   htmlOutput("search_status"),
                   uiOutput("patient_dashboard")
          ),
          tabPanel("📨 Générer lien patient",
                   tags$p("Envoyez ce lien au patient pour remplir le questionnaire à la maison."),
                   verbatimTextOutput("patient_link")
          )
        )
      })
      
      output$patient_link <- renderText({
        paste0(session$clientData$url_protocol, "//",
               session$clientData$url_hostname, ":",
               session$clientData$url_port,
               session$clientData$url_pathname, "?mode=patient&kine=", input$kine_login)
      })
    }
  })
  
  observe({
    if (mode() == "patient") {
      mod_questionnaire_patient_server("qs_patient")
    }
  })
  
  observeEvent(input$btn_search, {
    req(input$search_nom, input$search_prenom, input$search_naissance)
    
    hash <- tryCatch({
      generate_hash(input$search_nom, input$search_prenom, input$search_naissance)
    }, error = function(e) {
      return(NA)
    })
    
    if (is.na(hash)) {
      output$search_status <- renderUI({
        HTML('<span style="color:red; font-weight:bold;">❌ Format de date invalide. Utilisez JJ/MM/AAAA</span>')
      })
      output$patient_dashboard <- renderUI({ NULL })
    } else {
      if (file.exists("data/patients.csv")) {
        df <- read.csv2("data/patients.csv", stringsAsFactors = FALSE)
        if (hash %in% df$hash_id) {
          output$search_status <- renderUI({
            HTML('<span style="color:green; font-weight:bold;">✅ Patient retrouvé !</span>')
          })
          patient_data <- df[df$hash_id == hash, ]
          output$patient_dashboard <- renderUI({
            tagList(
              tags$head(tags$style(HTML(".encarts-container {
                display: flex;
                flex-wrap: wrap;
                gap: 20px;
                margin-top: 20px;
              }
              .encart-box {
                background: white;
                padding: 20px;
                border-radius: 10px;
                box-shadow: 0 2px 5px rgba(0,0,0,0.1);
                font-size: 18px;
                flex: 1 1 calc(33% - 20px);
                min-width: 300px;
                max-width: 100%;
              }"))),
              HTML(format_identite(patient_data)),
              div(class = "encarts-container",
                  lapply(encart_titles, format_encart)
              )
            )
          })
        } else {
          output$search_status <- renderUI({
            HTML('<span style="color:red; font-weight:bold;">❌ Patient non trouvé</span>')
          })
          output$patient_dashboard <- renderUI({ NULL })
        }
      }
    }
  })
}

shinyApp(ui, server)



