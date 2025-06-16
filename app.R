library(shiny)
library(shinyjs)

source("modules/mod_questionnaire_patient.R")
source("utils.R")

# --- Données Kinés ---
# ---------- TITRES DES ENCARTS ----------
encart_titles <- c(
  "Antécédent médicaux", "Traitement antérieur", "Drapeaux rouges",
  "Évaluation multiparamétrique", "Chimique versus mécanique", "Narration de l'anamnèse",
  "Bilan postural", "Perte de mouvement de la colonne", "Bilan de hanche/épaule",
  "Bilan du bassin", "Autre test", "Bilan neurologique", "Bilan musculaire",
  "Imagerie", "Mouvements répétés", "Diagnostique final", "Prescirption",
  "Sánce de suvit", "Suivit injection", "Conclusion ou ré-adressage"
)


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

# ---------- FONCTIONS DRAPEAUX ROUGES ----------
get_neuro_alert_level <- function(force_mi, force_ms, sens_mi, sens_ms) {
  force <- force_mi == "Oui" || force_ms == "Oui"
  sens  <- sens_mi == "Oui" || sens_ms == "Oui"
  if (force && sens) return("rouge")
  else if (force || sens) return("orange")
  else return("vert")
}

format_drapeau_neuro <- function(data) {
  level <- get_neuro_alert_level(
    data$neuro_force_mi, data$neuro_force_ms,
    data$neuro_sens_mi, data$neuro_sens_ms
  )
  
  couleur_txt <- switch(level, "vert" = "#4CAF50", "orange" = "#FF9800", "rouge" = "#F44336")
  icone <- switch(level, "vert" = "🟢", "orange" = "🟠", "rouge" = "🔴")
  
  details_list <- list(
    list(label = "Force MI", value = data$neuro_force_mi, ok = data$neuro_force_mi == "Non"),
    list(label = "Force MS", value = data$neuro_force_ms, ok = data$neuro_force_ms == "Non"),
    list(label = "Sensibilité MI", value = data$neuro_sens_mi, ok = data$neuro_sens_mi == "Non"),
    list(label = "Sensibilité MS", value = data$neuro_sens_ms, ok = data$neuro_sens_ms == "Non")
  )
  
  tagList(
    div(style = "margin-bottom:10px;",
        actionLink("toggle_dr_neuro", label = paste0(icone, " Drapeau rouge : neurologie"),
                   style = paste0("color:", couleur_txt, "; font-weight: bold; font-size: 18px; text-decoration: none;"))),
    hidden(div(id = "details_dr_neuro",
               tags$ul(lapply(details_list, function(item) {
                 col <- if (item$ok) "#4CAF50" else "#F44336"
                 tags$li(span(style = paste0("color:", col, "; font-weight:bold;"), paste(item$label, ": ", item$value)))
               }))
    ))
  )
}


format_drapeau_general <- function(data) {
  anorexie <- ifelse(is.null(data$general_anorexie) || is.na(data$general_anorexie), "Non", data$general_anorexie)
  asthenie <- ifelse(is.null(data$general_asthenie) || is.na(data$general_asthenie), "Non", data$general_asthenie)
  amaigrissement <- ifelse(is.null(data$general_amaigrissement) || is.na(data$general_amaigrissement), "Non", data$general_amaigrissement)
  
  general_alert <- any(c(anorexie, asthenie, amaigrissement) == "Oui")
  level <- if (general_alert) "rouge" else "vert"
  couleur_txt <- if (level == "rouge") "#F44336" else "#4CAF50"
  icone <- if (level == "rouge") "🔴" else "🟢"
  
  details <- list(
    list(label = "Anorexie", value = anorexie, ok = anorexie == "Non"),
    list(label = "Asthénie", value = asthenie, ok = asthenie == "Non"),
    list(label = "Amaigrissement", value = amaigrissement, ok = amaigrissement == "Non")
  )
  
  eq_val <- suppressWarnings(as.numeric(data$eq5d_vas))
  eq_val <- ifelse(is.na(eq_val) || eq_val < 0 || eq_val > 100, 50, eq_val)
  eq_col <- colorRampPalette(c("#F44336", "#FF9800", "#4CAF50"))(101)[round(eq_val) + 1]
  
  tagList(
    div(style = "margin-bottom:10px;",
        actionLink("toggle_dr_general", label = paste0(icone, " Drapeau rouge : état général"),
                   style = paste0("color:", couleur_txt, "; font-weight: bold; font-size: 18px; text-decoration: none;"))),
    hidden(div(id = "details_dr_general",
               tags$ul(lapply(details, function(item) {
                 col <- if (item$ok) "#4CAF50" else "#F44336"
                 tags$li(span(style = paste0("color:", col, "; font-weight:bold;"),
                              paste(item$label, ": ", item$value)))
               })),
               div(style = paste0("margin-top:10px; font-weight:bold; color:", eq_col),
                   paste("Auto-évaluation de la santé (EQ-VAS) :", eq_val))
    ))
  )
}


format_drapeau_infection <- function(data) {
  antibio <- ifelse(is.null(data$antibio_3mois) || is.na(data$antibio_3mois), "Non", data$antibio_3mois)
  urinaire <- ifelse(is.null(data$infection_urinaire_3mois) || is.na(data$infection_urinaire_3mois), "Non", data$infection_urinaire_3mois)
  
  antibio_color <- if (antibio == "Oui") "#F44336" else "#4CAF50"
  antibio_icon <- if (antibio == "Oui") "🔴" else "🟢"
  
  urinaire_color <- if (urinaire == "Oui") "#F44336" else "#4CAF50"
  urinaire_icon <- if (urinaire == "Oui") "🔴" else "🟢"
  
  tagList(
    tags$div(style = paste0("margin-bottom: 6px; font-weight:bold; color:", antibio_color),
             paste0(antibio_icon, " Prise d'antibiotiques (3 mois) : ", antibio)),
    tags$div(style = paste0("margin-bottom: 6px; font-weight:bold; color:", urinaire_color),
             paste0(urinaire_icon, " Infection urinaire (3 mois) : ", urinaire))
  )
}


# ---------- FORMAT ENCARTS GÉNÉRIQUES ----------
format_encart <- function(title) {
  HTML(paste0(
    "<div class='encart-box'>",
    "<h4 style='font-weight:bold;'>", title, "</h4>",
    "<p style='color:#999;'>Contenu à compléter...</p>",
    "</div>"
  ))
}

# ---------- INTERFACE ----------
ui <- fluidPage(
  useShinyjs(),
  uiOutput("main_ui")
)

# ---------- SERVER ----------
server <- function(input, output, session) {
  observe({
    query <- parseQueryString(session$clientData$url_search)
    if (!is.null(query$mode) && query$mode == 'patient') {
      mode('patient')
      updateTabsetPanel(session, 'main_nav', selected = 'questionnaire_patient')
    }
    if (!is.null(query$kine)) {
      print(paste('Kiné sélectionné :', query$kine))
    }
  })
  observe({
    query <- parseQueryString(session$clientData$url_search)
    if (!is.null(query$mode) && query$mode == 'patient') {
      updateTabsetPanel(session, 'main_nav', selected = 'questionnaire_patient')
    }
  })
  mode <- reactiveVal("kine")
  logged_in <- reactiveVal(FALSE)
  current_logo <- reactiveVal("logo.png")
  current_bg <- reactiveVal("#F0F7FE")
  
  observeEvent(input$toggle_dr_neuro, {
    toggle(id = "details_dr_neuro", anim = TRUE)
  })
  
  observeEvent(input$toggle_dr_general, {
    toggle(id = "details_dr_general", anim = TRUE)
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
                  div(class = "encart-box",
                      tags$h4("Drapeaux rouges"),
                      format_drapeau_neuro(patient_data),
                      format_drapeau_general(patient_data),
                      format_drapeau_infection(patient_data)
                  ),
                  lapply(encart_titles[encart_titles != "Drapeaux rouges"], format_encart)
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




