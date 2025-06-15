library(shiny)
library(digest)
library(stringi)
library(magrittr)

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

# UI
mod_questionnaire_patient_ui <- function(id) {
  ns <- NS(id)
  query <- parseQueryString(getDefaultReactiveDomain()$clientData$url_search)
  kine_id <- query[["kine"]]
  
  fond_color <- if (grepl("_APS$", kine_id)) "#FFCFA1" else "#EAF0EB"
  logo_src <- if (grepl("_APS$", kine_id)) "logo_aps.jpg" else "logo_mspv.png"
  
  tagList(
    fluidPage(
      style = paste0("overflow-y: auto; background-color: ", fond_color, ";"),
      div(style = "text-align:center; margin: 20px auto 10px auto;",
          img(src = logo_src, height = "120px")
      ),
      fluidRow(
        column(10, offset = 1,
               wellPanel(
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
                                choices = c("Pas de changement ou c’est devenu pire", "Presque pareil, difficilement perceptible", 
                                            "Presque pareil, mais perceptible", "Amélioration sensible", "Amélioration significative", 
                                            "Beaucoup mieux, une nette différence", "Guéri / résolu"))
                 ),
                 
                 radioButtons(ns("trait_kine"), "Kinésithérapie", choices = c("Non", "Oui")),
                 conditionalPanel(
                   condition = sprintf("input['%s'] == 'Oui'", ns("trait_kine")),
                   radioButtons(ns("pgic_kine"), "Effet de la kiné (PGIC)", 
                                choices = c("Pas de changement ou c’est devenu pire", "Presque pareil, difficilement perceptible", 
                                            "Presque pareil, mais perceptible", "Amélioration sensible", "Amélioration significative", 
                                            "Beaucoup mieux, une nette différence", "Guéri / résolu"))
                 ),
                 
                 radioButtons(ns("trait_infiltration"), "Infiltrations", choices = c("Non", "Oui")),
                 conditionalPanel(
                   condition = sprintf("input['%s'] == 'Oui'", ns("trait_infiltration")),
                   radioButtons(ns("pgic_infiltration"), "Effet des infiltrations (PGIC)", 
                                choices = c("Pas de changement ou c’est devenu pire", "Presque pareil, difficilement perceptible", 
                                            "Presque pareil, mais perceptible", "Amélioration sensible", "Amélioration significative", 
                                            "Beaucoup mieux, une nette différence", "Guéri / résolu"))
                 ),
                 
                 radioButtons(ns("trait_osteo"), "Ostéopathie / Thérapie manuelle", choices = c("Non", "Oui")),
                 conditionalPanel(
                   condition = sprintf("input['%s'] == 'Oui'", ns("trait_osteo")),
                   radioButtons(ns("pgic_osteo"), "Effet de l'ostéopathie (PGIC)", 
                                choices = c("Pas de changement ou c’est devenu pire", "Presque pareil, difficilement perceptible", 
                                            "Presque pareil, mais perceptible", "Amélioration sensible", "Amélioration significative", 
                                            "Beaucoup mieux, une nette différence", "Guéri / résolu"))
                 ),
                 
                 actionButton(ns("save_btn"), "💕 Sauvegarder"),
                 htmlOutput(ns("save_status"))
               )
        )
      )
    )
  )
}

