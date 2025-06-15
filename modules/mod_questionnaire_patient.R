library(shiny)
library(digest)
library(stringi)
library(magrittr)
source("utils.R")
# Nettoyage des chaînes
txt_clean <- function(txt) {
  txt %>% 
    tolower() %>% 
    stringi::stri_trans_general("Latin-ASCII") %>% 
    gsub("[^a-z0-9]", "", .)
}

# Génération du hash
generate_hash <- function(nom, prenom, naissance) {
  nom_clean <- txt_clean(nom)
  prenom_clean <- txt_clean(prenom)
  
  if (!grepl("^\\d{2}/\\d{2}/\\d{4}$", naissance)) {
    stop("⚠️ Format de date invalide. Utilisez JJ/MM/AAAA, ex: 23/10/1990")
  }
  
  naissance_clean <- gsub("[^0-9]", "", naissance)
  raw <- paste0(nom_clean, prenom_clean, naissance_clean, "Spine2025")
  digest::digest(raw, algo = "sha256")
}

mod_questionnaire_patient_ui <- function(id) {
  ns <- NS(id)
  query <- parseQueryString(getDefaultReactiveDomain()$clientData$url_search)
  kine_id <- query[["kine"]]
  
  # Déduction du cabinet et des assets associés
  cabinet <- get_cabinet_from_kine(kine_id)
  logo_src <- get_logo_for_cabinet(cabinet)
  fond_color <- get_bg_color_for_cabinet(cabinet)
  
  tagList(
    tags$head(
      tags$style(HTML(paste0(
        "body { background-color: ", fond_color, "; margin: 0; padding: 0; overflow-x: hidden; }",
        ".questionnaire-wrapper { display: flex; justify-content: center; padding: 30px; }",
        ".questionnaire-container { width: 100%; max-width: 1400px; }",
        ".title-logo { display: none; }"
      )))
    ),
    div(class = "questionnaire-wrapper",
        div(class = "questionnaire-container",
            div(style = "text-align:center; margin-bottom: 30px;",
                img(src = logo_src, height = "120px")
            ),
            fluidRow(
              column(12,
                     fluidRow(
                       column(4, textInput(ns("nom"), "Nom")),
                       column(4, textInput(ns("prenom"), "Prénom")),
                       column(4, textInput(ns("naissance"), "Date de naissance (JJ/MM/AAAA)"))
                     ),
                     fluidRow(
                       column(4, numericInput(ns("taille"), "Taille (cm)", value = NA, min = 30, max = 300)),
                       column(4, numericInput(ns("poids"), "Poids (kg)", value = NA, min = 30, max = 300)),
                       column(4, radioButtons(ns("sexe"), "Sexe", choices = c("Homme", "Femme")))
                     ),
                     radioButtons(ns("statut"), "Statut professionnel", choices = c("Étudiant", "Actif", "Retraité")),
                     conditionalPanel(
                       condition = sprintf("input['%s'] == 'Actif' || input['%s'] == 'Retraité'", ns("statut"), ns("statut")),
                       textInput(ns("metier"), "Métier ou ancien métier"),
                       selectInput(ns("pcs"), "Catégorie socio-professionnelle", 
                                   choices = c(
                                     "PCS1 - Agriculteurs exploitants / Agricultrices exploitantes",
                                     "PCS2 - Artisans / Artisanes, commerçants / commerçantes et chefs / cheffes d'entreprise",
                                     "PCS3 - Cadres et professions intellectuelles supérieures",
                                     "PCS4 - Professions intermédiaires",
                                     "PCS5 - Employés / Employées",
                                     "PCS6 - Ouvriers / Ouvrières"
                                   ))
                     ),
                     radioButtons(ns("chirurgie_rachis"), "Avez-vous déjà été opéré du dos, des lombaires ou des cervicales ?", choices = c("Non", "Oui")),
                     conditionalPanel(
                       condition = sprintf("input['%s'] == 'Oui'", ns("chirurgie_rachis")),
                       selectInput(ns("type_chirurgie"), "Type de chirurgie", 
                                   choices = c("Discectomie (hernie discale)", "Arthrodèse", "Prothèse de disque", "Autre")),
                       textInput(ns("date_chirurgie"), "Date de la chirurgie (JJ/MM/AAAA)")
                     ),
                     h4("Traitements antérieurs"),
                     p("Ici indiquez les informations sur les traitements que vous avez eus pour l'épisode en cours."),
                     radioButtons(ns("trait_medoc"), "Médicaments", choices = c("Non", "Oui")),
                     conditionalPanel(
                       condition = sprintf("input['%s'] == 'Oui'", ns("trait_medoc")),
                       radioButtons(ns("pgic_medoc"), "Effet des médicaments (PGIC)", 
                                    choices = c(
                                      "Pas de changement ou c’est devenu pire", 
                                      "Presque pareil, pratiquement pas d’amélioration",
                                      "Un peu mieux mais pas de changement notable",
                                      "Plutôt mieux mais le changement ne fait pas de réelle différence",
                                      "Mieux, le changement est modéré mais notable",
                                      "Mieux avec sans aucun doute une amélioration réelle qui fait la différence",
                                      "Nettement mieux, une amélioration considérable qui fait toute la différence"
                                    ))
                     ),
                     radioButtons(ns("trait_kine"), "Kinésithérapie", choices = c("Non", "Oui")),
                     conditionalPanel(
                       condition = sprintf("input['%s'] == 'Oui'", ns("trait_kine")),
                       radioButtons(ns("pgic_kine"), "Effet de la kiné (PGIC)", 
                                    choices = c(
                                      "Pas de changement ou c’est devenu pire", 
                                      "Presque pareil, pratiquement pas d’amélioration",
                                      "Un peu mieux mais pas de changement notable",
                                      "Plutôt mieux mais le changement ne fait pas de réelle différence",
                                      "Mieux, le changement est modéré mais notable",
                                      "Mieux avec sans aucun doute une amélioration réelle qui fait la différence",
                                      "Nettement mieux, une amélioration considérable qui fait toute la différence"
                                    ))
                     ),
                     radioButtons(ns("trait_infiltration"), "Infiltrations", choices = c("Non", "Oui")),
                     conditionalPanel(
                       condition = sprintf("input['%s'] == 'Oui'", ns("trait_infiltration")),
                       radioButtons(ns("pgic_infiltration"), "Effet des infiltrations (PGIC)", 
                                    choices = c(
                                      "Pas de changement ou c’est devenu pire", 
                                      "Presque pareil, pratiquement pas d’amélioration",
                                      "Un peu mieux mais pas de changement notable",
                                      "Plutôt mieux mais le changement ne fait pas de réelle différence",
                                      "Mieux, le changement est modéré mais notable",
                                      "Mieux avec sans aucun doute une amélioration réelle qui fait la différence",
                                      "Nettement mieux, une amélioration considérable qui fait toute la différence"
                                    ))
                     ),
                     radioButtons(ns("trait_osteo"), "Ostéopathie / Thérapie manuelle", choices = c("Non", "Oui")),
                     conditionalPanel(
                       condition = sprintf("input['%s'] == 'Oui'", ns("trait_osteo")),
                       radioButtons(ns("pgic_osteo"), "Effet de l'ostéopathie (PGIC)", 
                                    choices = c(
                                      "Pas de changement ou c’est devenu pire", 
                                      "Presque pareil, pratiquement pas d’amélioration",
                                      "Un peu mieux mais pas de changement notable",
                                      "Plutôt mieux mais le changement ne fait pas de réelle différence",
                                      "Mieux, le changement est modéré mais notable",
                                      "Mieux avec sans aucun doute une amélioration réelle qui fait la différence",
                                      "Nettement mieux, une amélioration considérable qui fait toute la différence"
                                    ))
                     ),
                     actionButton(ns("save_btn"), "💕 Sauvegarder"),
                     htmlOutput(ns("save_status"))
              )
            )
        )
    )
  )
}

mod_questionnaire_patient_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$save_btn, {
      output$save_status <- renderUI({
        HTML("<span style='color:green; font-weight:bold;'>✅ Données sauvegardées (simulé)</span>")
      })
    })
  })
}
