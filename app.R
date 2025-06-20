library(shiny)
library(shinyjs)
library(scales)
library(fmsb)
library(eq5d)

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
  situation <- patient$situation_pro
  
  # On gère selon le statut
  if (statut == "étudiant") {
    affichage_metier <- "Étudiant"
  } else if (statut == "retraité") {
    affichage_metier <- paste0(metier, " (PCS", pcs, " - ", statut, ")")
  } else if (statut == "actif") {
    affichage_metier <- paste0(metier, " (PCS", pcs, " - ", statut, ") - Situation : ", situation)
  } else {
    affichage_metier <- metier
  }
  
  sport_html <- ""
  if (!is.null(patient$sport_pratique) && patient$sport_pratique == "Oui") {
    sport_nom <- patient$sport
    frequence <- patient$frequence_semaine
    duree <- patient$duree_entrainement
    effet <- patient$duree_effet_douleur
    
    sport_html <- paste0(
      "<div style='margin-bottom:8px;'>Sport pratiqué : <b>", sport_nom,
      " (", frequence, " fois, ", duree, " par semaine)</b></div>"
    )
    
    if (!is.null(effet) && effet != "") {
      sport_html <- paste0(
        sport_html,
        "<div style='margin-bottom:8px;'>Effet de la douleur sur la pratique : <b>", effet, "</b></div>"
      )
    }
  } else {
    sport_html <- "<div style='margin-bottom:8px;'>🏃 Sport pratiqué : <b>Aucun</b></div>"
  }
  
  
  HTML(paste0(
    HTML(paste0(
      "<div style='margin-bottom:8px;'>Taille : <b>", taille, " cm</b></div>",
      "<div style='margin-bottom:8px;'>Poids : <b>", poids, " kg</b></div>",
      "<div style='margin-bottom:8px;'>IMC : <b>", imc, "</b></div>",
      "<div style='margin-bottom:8px;'>Métier : <b>", affichage_metier, "</b></div>",
      sport_html
      
    ))
    
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
  
  couleur_txt <- switch(level,
                        "vert" = "#4CAF50",
                        "orange" = "#FF9800",
                        "rouge" = "#F44336")
  icone <- switch(level,
                  "vert" = "🟢",
                  "orange" = "🟠",
                  "rouge" = "🔴")
  
  details_list <- list(
    list(label = "Force MI", value = data$neuro_force_mi, ok = data$neuro_force_mi == "Non"),
    list(label = "Force MS", value = data$neuro_force_ms, ok = data$neuro_force_ms == "Non"),
    list(label = "Sensibilité MI", value = data$neuro_sens_mi, ok = data$neuro_sens_mi == "Non"),
    list(label = "Sensibilité MS", value = data$neuro_sens_ms, ok = data$neuro_sens_ms == "Non")
  )
  
  tagList(
    div(style = "margin-bottom:10px;",
        actionLink("toggle_dr_neuro",
                   label = paste0(icone, " Drapeau rouge : neurologie"),
                   style = paste0("color:", couleur_txt, "; font-weight: bold; font-size: 18px; text-decoration: none;"))),
    hidden(div(id = "details_dr_neuro",
               tags$ul(lapply(details_list, function(item) {
                 col <- if (item$ok) "#4CAF50" else "#F44336"
                 tags$li(span(style = paste0("color:", col, "; font-weight:bold;"),
                              paste(item$label, ": ", item$value)))
               }))
    ))
  )
}

format_drapeau_infection <- function(data) {
  antibio <- ifelse(is.null(data$antibio_3mois) || is.na(data$antibio_3mois), "Non", data$antibio_3mois)
  urinaire <- ifelse(is.null(data$infection_urinaire_3mois) || is.na(data$infection_urinaire_3mois), "Non", data$infection_urinaire_3mois)
  chirurgie <- ifelse(is.null(data$chir_recent) || is.na(data$chir_recent), "Non", data$chir_recent)
  details <- ifelse(is.null(data$chir_recent_details), "", paste0(" (", data$chir_recent_details, ")"))
  
  rouge <- antibio == "Oui" || urinaire == "Oui" || chirurgie == "Oui"
  couleur_txt <- if (rouge) "#F44336" else "#4CAF50"
  icone <- if (rouge) "🔴" else "🟢"
  
  items <- list(
    list(label = "Prise d'antibiotiques (3 mois)", value = antibio, ok = antibio == "Non"),
    list(label = "Infection urinaire (3 mois)", value = urinaire, ok = urinaire == "Non"),
    list(label = "Chirurgie récente", value = paste0(chirurgie, details), ok = chirurgie == "Non")
  )
  
  tagList(
    div(style = "margin-bottom:10px;",
        actionLink("toggle_dr_infection", label = paste0(icone, " Drapeau rouge : risques infectieux"),
                   style = paste0("color:", couleur_txt, "; font-weight: bold; font-size: 18px; text-decoration: none;"))),
    hidden(div(id = "details_dr_infection",
               tags$ul(lapply(items, function(item) {
                 col <- if (item$ok) "#4CAF50" else "#F44336"
                 tags$li(span(style = paste0("color:", col, "; font-weight:bold;"),
                              paste(item$label, ": ", item$value)))
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
  
  eq_val <- suppressWarnings(as.numeric(data$eq_vas))
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
  chirurgie <- ifelse(is.null(data$chir_recent) || is.na(data$chir_recent), "Non", data$chir_recent)
  details <- ifelse(chirurgie == "Oui" && !is.null(data$chir_recent_details) && !is.na(data$chir_recent_details),
                    paste0(" (", data$chir_recent_details, ")"), "")
  
  rouge <- antibio == "Oui" || urinaire == "Oui" || chirurgie == "Oui"
  couleur_txt <- if (rouge) "#F44336" else "#4CAF50"
  icone <- if (rouge) "🔴" else "🟢"
  
  items <- list(
    list(label = "Prise d'antibiotiques (3 mois)", value = antibio, ok = antibio == "Non"),
    list(label = "Infection urinaire (3 mois)", value = urinaire, ok = urinaire == "Non"),
    list(label = "Chirurgie récente", value = paste0(chirurgie, details), ok = chirurgie == "Non")
  )
  
  tagList(
    div(style = "margin-bottom:10px;",
        actionLink("toggle_dr_infection", label = paste0(icone, " Drapeau rouge : risques infectieux"),
                   style = paste0("color:", couleur_txt, "; font-weight: bold; font-size: 18px; text-decoration: none;"))),
    hidden(div(id = "details_dr_infection",
               tags$ul(lapply(items, function(item) {
                 col <- if (item$ok) "#4CAF50" else "#F44336"
                 tags$li(span(style = paste0("color:", col, "; font-weight:bold;"),
                              paste(item$label, ": ", item$value)))
               }))
    ))
  )
}

format_drapeau_psychologique <- function(data) {
  trauma <- ifelse(is.null(data$trauma_experience) || is.na(data$trauma_experience), "Non", data$trauma_experience)
  couleur_txt <- "#4CAF50"
  texte <- "Aucun traumatisme signalé"
  
  if (trauma == "Oui") {
    symptomes <- unlist(data[c("ptsd_sympt_1", "ptsd_sympt_2", "ptsd_sympt_3", "ptsd_sympt_4", "ptsd_sympt_5")])
    n_oui <- sum(symptomes == "Oui", na.rm = TRUE)
    
    if (n_oui >= 3) {
      couleur_txt <- "#F44336"
      texte <- paste0("Symptômes PTSD : ", n_oui, "/5 signalés")
    } else {
      couleur_txt <- "#FF9800"
      texte <- paste0("Symptômes PTSD : ", n_oui, "/5 signalés")
    }
  }
  
  div(style = paste0("margin-bottom:10px; font-weight:bold; color:", couleur_txt),
      paste0(" Drapeau rouge : santé mentale - ", texte))
}

format_antecedent_chirurgie_rachis <- function(data) {
  # Récupération des infos
  op_rachis <- ifelse(is.null(data$chirurgie_rachis) || is.na(data$chirurgie_rachis), "Non", data$chirurgie_rachis)
  couleur <- if (op_rachis == "Oui") "#FF9800" else "#4CAF50"
  icone <- if (op_rachis == "Oui") "🟠" else "🟢"
  
  # Détails si opéré
  details_html <- NULL
  if (op_rachis == "Oui") {
    type <- ifelse(is.null(data$type_chirurgie), "Type inconnu", data$type_chirurgie)
    # Date et calcul de durée
    if (!is.null(data$date_chirurgie) && !is.na(data$date_chirurgie)) {
      # On essaye de parser
      try_date <- try(as.Date(data$date_chirurgie, format="%d/%m/%Y"), silent = TRUE)
      if (!inherits(try_date, "try-error")) {
        diff_years <- as.numeric(difftime(Sys.Date(), try_date, units = "days")) / 365.25
        age_chir <- ifelse(diff_years < 1, "< 1 an", paste0(round(diff_years, 1), " ans"))
      } else {
        age_chir <- "Date invalide"
      }
    } else {
      age_chir <- "Date manquante"
    }
    
    # HTML déroulant
    details_html <- tags$ul(
      tags$li(paste("Type de chirurgie :", type)),
      tags$li(paste("Durée depuis chirurgie :", age_chir))
    )
  }
  
  # Construction du tag final
  tagList(
    div(style = "margin-bottom:10px;",
        actionLink("toggle_chir_rachis", 
                   label = paste0(icone, " Chirurgie du rachis : ", op_rachis),
                   style = paste0("color:", couleur, "; font-weight: bold; font-size: 18px; text-decoration: none;"))
    ),
    hidden(div(id = "details_chir_rachis", details_html))
  )
}

format_cancer_history <- function(data) {
  diagnosed <- ifelse(is.null(data$cancer_diagnosed) || is.na(data$cancer_diagnosed), "Non", data$cancer_diagnosed)
  
  if (diagnosed == "Non") {
    couleur <- "#4CAF50"
    icone <- "🟢"
    texte <- "Aucun antécédent de cancer"
    
    return(tagList(
      div(style = "margin-bottom:10px;",
          span(style = paste0("color:", couleur, "; font-weight:bold; font-size: 18px;"),
               paste0(icone, " ", texte)))
    ))
  }
  
  # Sinon, on affiche le menu déroulant
  age_diag <- ifelse(is.null(data$cancer_diagnostic_age), "Inconnu", data$cancer_diagnostic_age)
  couleur <- switch(age_diag,
                    "Moins d'un an" = "#F44336",
                    "Entre 1 et 2 ans" = "#F44336",
                    "Entre 2 et 5 ans" = "#FF9800",
                    "Plus de 5 ans" = "#FF9800",
                    "#999999")
  icone <- ifelse(couleur == "#F44336", "🔴", ifelse(couleur == "#FF9800", "🟠", "❔"))
  
  types <- c()
  if (!is.null(data$cancer_poumon) && data$cancer_poumon == "Oui") types <- c(types, "Poumon")
  if (!is.null(data$cancer_sein) && data$cancer_sein == "Oui") types <- c(types, "Sein")
  if (!is.null(data$cancer_rein) && data$cancer_rein == "Oui") types <- c(types, "Rein")
  if (!is.null(data$cancer_prostate) && data$cancer_prostate == "Oui") types <- c(types, "Prostate")
  if (!is.null(data$cancer_thyroide) && data$cancer_thyroide == "Oui") types <- c(types, "Thyroïde")
  if (!is.null(data$cancer_autre) && data$cancer_autre == "Oui") types <- c(types, "Autre")
  types_txt <- if (length(types) == 0) "Type inconnu" else paste(types, collapse = ", ")
  
  tagList(
    div(style = "margin-bottom:10px;",
        actionLink("toggle_cancer_history",
                   label = paste0(icone, " Antécédent de cancer : ", age_diag),
                   style = paste0("color:", couleur, "; font-weight: bold; font-size: 18px; text-decoration: none;"))
    ),
    hidden(div(id = "details_cancer_history",
               tags$ul(
                 tags$li(paste("Âge du diagnostic :", age_diag)),
                 tags$li(paste("Type(s) de cancer :", types_txt))
               )
    ))
  )
}

format_antecedent_tabac <- function(data) {
  valeur <- ifelse(is.null(data$tabac) || is.na(data$tabac), "Non", data$tabac)
  couleur <- if (valeur == "Oui") "#F44336" else "#4CAF50"
  
  div(style = "margin-bottom:10px;",
      span(style = paste0("color:", couleur, "; font-weight:bold; font-size: 18px;"),
           paste0(" Tabac : ", valeur)))
}

format_antecedents_rhumato <- function(data) {
  items <- list(
    "Spondylarthrite ankylosante" = data$rhumato_spa,
    "Polyarthrite rhumatoïde" = data$rhumato_pr,
    "Crohn" = data$rhumato_crohn,
    "RCH" = data$rhumato_rch,
    "Uvée/uvéite" = data$rhumato_uveite,
    "Psoriasis" = data$rhumato_psoriasis,
    "Goutte" = data$rhumato_goutte
  )
  
  # Filtrage des pathos cochées
  pathos_oui <- names(items)[unlist(items) == "Oui"]
  couleur <- if (length(pathos_oui) > 0) "#F44336" else "#4CAF50"
  icone <- if (length(pathos_oui) > 0) "🔴" else "🟢"
  
  tagList(
    div(style = "margin-bottom:10px;",
        actionLink("toggle_rhumato", 
                   label = paste0(icone, " Antécédents rhumatologiques"),
                   style = paste0("color:", couleur, "; font-weight: bold; font-size: 18px; text-decoration: none;"))
    ),
    hidden(div(id = "details_rhumato",
               if (length(pathos_oui) > 0) {
                 tags$ul(lapply(pathos_oui, function(x) tags$li(x)))
               } else {
                 tags$p("Aucun antécédent détecté")
               }
    ))
  )
}

format_antecedents_cardio <- function(data) {
  items <- list(
    "Hypercholestérolémie" = data$cardio_chol,
    "Hypertension artérielle" = data$cardio_hta,
    "Diabète" = data$cardio_diabete,
    "Problèmes cardiaques" = data$cardio_cardiaque,
    "Artérite / AOMI" = data$cardio_arterite
  )
  
  pathos_oui <- names(items)[unlist(items) == "Oui"]
  couleur <- if (length(pathos_oui) > 0) "#F44336" else "#4CAF50"
  icone <- if (length(pathos_oui) > 0) "🔴" else "🟢"
  
  tagList(
    div(style = "margin-bottom:10px;",
        actionLink("toggle_cardio", 
                   label = paste0(icone, " Antécédents cardiovasculaires"),
                   style = paste0("color:", couleur, "; font-weight: bold; font-size: 18px; text-decoration: none;"))
    ),
    hidden(div(id = "details_cardio",
               if (length(pathos_oui) > 0) {
                 tags$ul(lapply(pathos_oui, function(x) tags$li(x)))
               } else {
                 tags$p("Aucun antécédent détecté")
               }
    ))
  )
}

format_traitement_kine <- function(data) {
  valeur <- ifelse(is.null(data$trait_kine), "Non", data$trait_kine)
  pgic <- data$pgic_kine
  
  if (valeur == "Non") {
    couleur <- "#000000"
    icone <- "⚫️"
    texte <- "Non réalisé"
  } else if (pgic %in% c(
    "Mieux avec sans aucun doute une amélioration réelle qui fait la différence",
    "Nettement mieux, une amélioration considérable qui fait toute la différence")) {
    couleur <- "#4CAF50"  # Vert
    icone <- "🟢"
    texte <- pgic
  } else if (pgic %in% c(
    "Presque pareil, pratiquement pas d’amélioration",
    "Un peu mieux mais pas de changement notable",
    "Plutôt mieux mais le changement ne fait pas de réelle différence",
    "Mieux, le changement est modéré mais notable")) {
    couleur <- "#FF9800"  # Orange
    icone <- "🟠"
    texte <- pgic
  } else if (pgic == "Pas de changement ou c’est devenu pire") {
    couleur <- "#F44336"  # Rouge
    icone <- "🔴"
    texte <- pgic
  } else {
    couleur <- "#000000"
    icone <- "⚫️"
    texte <- pgic
  }
  
  tagList(
    div(style = "margin-bottom:10px;",
        span(style = paste0("color:", couleur, "; font-weight:bold; font-size: 18px;"),
             paste0(icone, " Kinésithérapie : ", texte)))
  )
}

format_traitement_osteo <- function(data) {
  valeur <- ifelse(is.null(data$trait_osteo), "Non", data$trait_osteo)
  pgic <- data$pgic_osteo
  
  if (valeur == "Non") {
    couleur <- "#000000"
    icone <- "⚫️"
    texte <- "Non réalisé"
  } else if (pgic %in% c(
    "Mieux avec sans aucun doute une amélioration réelle qui fait la différence",
    "Nettement mieux, une amélioration considérable qui fait toute la différence")) {
    couleur <- "#4CAF50"
    icone <- "🟢"
    texte <- pgic
  } else if (pgic %in% c(
    "Presque pareil, pratiquement pas d’amélioration",
    "Un peu mieux mais pas de changement notable",
    "Plutôt mieux mais le changement ne fait pas de réelle différence",
    "Mieux, le changement est modéré mais notable")) {
    couleur <- "#FF9800"
    icone <- "🟠"
    texte <- pgic
  } else if (pgic == "Pas de changement ou c’est devenu pire") {
    couleur <- "#F44336"
    icone <- "🔴"
    texte <- pgic
  } else {
    couleur <- "#000000"
    icone <- "⚫️"
    texte <- pgic
  }
  
  tagList(
    div(style = "margin-bottom:10px;",
        span(style = paste0("color:", couleur, "; font-weight:bold; font-size: 18px;"),
             paste0(icone, " Ostéopathie : ", texte)))
  )
}

format_traitement_medoc <- function(data) {
  valeur <- ifelse(is.null(data$trait_medoc), "Non", data$trait_medoc)
  pgic <- data$pgic_medoc
  
  if (valeur == "Non") {
    couleur <- "#000000"
    icone <- "⚫️"
    texte <- "Non réalisé"
  } else if (pgic %in% c(
    "Mieux avec sans aucun doute une amélioration réelle qui fait la différence",
    "Nettement mieux, une amélioration considérable qui fait toute la différence")) {
    couleur <- "#4CAF50"
    icone <- "🟢"
    texte <- pgic
  } else if (pgic %in% c(
    "Presque pareil, pratiquement pas d’amélioration",
    "Un peu mieux mais pas de changement notable",
    "Plutôt mieux mais le changement ne fait pas de réelle différence",
    "Mieux, le changement est modéré mais notable")) {
    couleur <- "#FF9800"
    icone <- "🟠"
    texte <- pgic
  } else if (pgic == "Pas de changement ou c’est devenu pire") {
    couleur <- "#F44336"
    icone <- "🔴"
    texte <- pgic
  } else {
    couleur <- "#000000"
    icone <- "⚫️"
    texte <- pgic
  }
  
  tagList(
    div(style = "margin-bottom:10px;",
        span(style = paste0("color:", couleur, "; font-weight:bold; font-size: 18px;"),
             paste0(icone, " Médicamentation : ", texte)))
  )
}

format_traitement_infiltration <- function(data) {
  valeur <- ifelse(is.null(data$trait_infiltration), "Non", data$trait_infiltration)
  pgic <- data$pgic_infiltration
  
  if (valeur == "Non") {
    couleur <- "#000000"
    icone <- "⚫️"
    texte <- "Non réalisé"
  } else if (pgic %in% c(
    "Mieux avec sans aucun doute une amélioration réelle qui fait la différence",
    "Nettement mieux, une amélioration considérable qui fait toute la différence")) {
    couleur <- "#4CAF50"
    icone <- "🟢"
    texte <- pgic
  } else if (pgic %in% c(
    "Presque pareil, pratiquement pas d’amélioration",
    "Un peu mieux mais pas de changement notable",
    "Plutôt mieux mais le changement ne fait pas de réelle différence",
    "Mieux, le changement est modéré mais notable")) {
    couleur <- "#FF9800"
    icone <- "🟠"
    texte <- pgic
  } else if (pgic == "Pas de changement ou c’est devenu pire") {
    couleur <- "#F44336"
    icone <- "🔴"
    texte <- pgic
  } else {
    couleur <- "#000000"
    icone <- "⚫️"
    texte <- pgic
  }
  
  tagList(
    div(style = "margin-bottom:10px;",
        span(style = paste0("color:", couleur, "; font-weight:bold; font-size: 18px;"),
             paste0(icone, " Infiltration : ", texte)))
  )
}

# ---------- FORMAT ENCARTS MULTIPARAMTRIQUE ----------

# 🧠 Textes de référence pour chaque question ODI
ODI_TEXTES <- list(
  odi_douleur = c("Je n'ai pas mal actuellement.", "La douleur est très légère actuellement.", "La douleur est modérée actuellement.", "La douleur est plutôt intense actuellement.", "La douleur est très intense actuellement.", "La douleur est la pire que l’on puisse imaginer actuellement."),
  odi_soins = c("Je peux prendre soin de moi normalement, sans augmenter la douleur.", "Je peux prendre soin de moi normalement, mais c’est très douloureux.", "Cela me fait mal de prendre soin de moi, et je le fait lentement et en faisant attention.", "J’ai besoin d’aide, mais dans l'ensemble je parviens à me débrouiller seul.", "J’ai besoin d’aide tous les jours pour la plupart de ces gestes quotidiens.", "Je ne m'habille pas, me lave avec difficulté et reste au lit."),
  odi_manutentions = c("Je peux soulever des charges lourdes sans augmenter mon mal de dos", "Je peux soulever des charges lourdes mais cela augmente ma douleur", "La douleur m'empêche de soulever des charges lourdes à partir du sol mais j'y parviens si la charge est bien placée", "La douleur m'empêche de soulever des charges lourdes mais je peux déplacer des charges légères ou de poids moyen", "Je peux seulement soulever des objets très légers", "Je ne peux soulever ni transporter quoi que ce soit"),
  odi_marche = c("La douleur ne limite absolument pas mes déplacements", "La douleur m'empêche de marcher plus de 2 km", "La douleur m'empêche de marcher plus de 1 km", "La douleur m'empêche de marcher plus de 500 m", "Je me déplace seulement avec une canne ou des béquilles", "Je reste au lit la plupart du temps et je me traîne seulement jusqu'au WC"),
  odi_assis = c("Je peux rester assis sur un siège aussi longtemps que je veux.", "Je peux rester assis aussi longtemps que je veux mais seulement sur mon siège favori.", "La douleur m'empêche de rester assis plus d'une heure.", "La douleur m'empêche de rester assis plus d'1/2 heure.", "La douleur m'empêche de rester assis plus de 10 minutes.", "La douleur m'empêche de rester assis."),
  odi_debout = c("Je peux rester debout aussi longtemps que je veux sans augmenter la douleur.", "Je peux rester debout aussi longtemps que je veux mais cela augmente la douleur.", "La douleur m'empêche de rester debout plus d'une heure.", "La douleur m'empêche de rester debout plus d'1/2 heure.", "La douleur m'empêche de rester debout plus de 10 minutes.", "La douleur m'empêche de rester debout."),
  odi_sommeil = c("Mon sommeil n’est jamais perturbé par la douleur.", "Mon sommeil est parfois perturbé par la douleur", "A cause de la douleur, je dors moins de 6 heures", "A cause de la douleur, je dors moins de 4 heures", "A cause de la douleur, je dors moins de 2 heures", "La douleur m'empêche complètement de dormir"),
  odi_sexe = c("Ma vie sexuelle n'est pas modifiée et n'augmente pas mon mal de dos", "Ma vie sexuelle n'est pas modifiée, mais elle augmente la douleur", "Ma vie sexuelle est pratiquement normale, mais elle est très douloureuse", "Ma vie sexuelle est fortement limitée par la douleur", "Ma vie sexuelle est presque inexistante à cause de la douleur", "La douleur m'interdit toute vie sexuelle"),
  odi_sociale = c("Ma vie sociale est normale et n’a pas d’effet sur la douleur", "Ma vie sociale est normale, mais elle augmente la douleur", "La douleur n'a pas d'effet sur ma vie sociale, sauf pour des activités demandant plus d'énergie", "La douleur a réduit ma vie sociale et je ne sors plus autant qu'auparavant", "La douleur a limité ma vie sociale à ce qui se passe chez moi, à la maison", "Je n'ai plus de vie sociale à cause du mal de dos"),
  odi_deplacements = c("Je peux me déplacer n'importe où sans effet sur mon mal de dos", "Je peux me déplacer n'importe où, mais cela augmente la douleur", "La douleur est pénible mais je supporte des trajets de plus de 2 heures", "La douleur me limite à des trajets de moins d'une heure", "La douleur me limite aux courts trajets indispensables, de moins de 30 minutes", "La douleur m'empêche de me déplacer, sauf pour aller voir le docteur ou me rendre à l'hôpital")
)

# 🧠 Textes de référence pour NDI
NDI_TEXTES <- list(
  ndi_douleur = c("Aucune douleur", "Douleur légère", "Douleur modérée", "Douleur forte", "Douleur très forte", "Douleur insupportable"),
  ndi_soins = c("Aucune difficulté", "Légère difficulté", "Difficulté modérée", "Grande difficulté", "Dépendance partielle", "Dépendance totale"),
  ndi_manutentions = c("Sans difficulté", "Légère difficulté", "Difficulté modérée", "Grande difficulté", "Extrême difficulté", "Impossible"),
  ndi_lire = c("Aucune difficulté", "Légère difficulté", "Difficulté modérée", "Grande difficulté", "Extrême difficulté", "Impossible"),
  ndi_mauxTete = c("Jamais", "Rarement", "Parfois", "Souvent", "Très souvent", "Toujours"),
  ndi_concentration = c("Aucune difficulté", "Légère difficulté", "Difficulté modérée", "Grande difficulté", "Extrême difficulté", "Impossible"),
  ndi_travail = c("Sans difficulté", "Légère difficulté", "Difficulté modérée", "Grande difficulté", "Extrême difficulté", "Impossible"),
  ndi_actiQuoti = c("Sans difficulté", "Légère difficulté", "Difficulté modérée", "Grande difficulté", "Extrême difficulté", "Impossible"),
  ndi_sommeil = c("Pas de problème", "Léger problème", "Problème modéré", "Gros problème", "Très gros problème", "Impossible de dormir"),
  ndi_sociale = c("Aucun impact", "Léger impact", "Impact modéré", "Fort impact", "Très fort impact", "Vie sociale impossible")
)

# Fonction pour convertir les réponses texte en index numérique pour ODI/NDI
get_score_index <- function(response, question_id) {
  if (startsWith(question_id, "odi_")) {
    niveaux <- ODI_TEXTES[[question_id]]
  } else if (startsWith(question_id, "ndi_")) {
    niveaux <- NDI_TEXTES[[question_id]]
  } else {
    return(NA)
  }
  match(response, niveaux) - 1
}

# Fonction pour générer le score ODI ou NDI (renvoie % et liste de scores)
compute_disability_score <- function(data, variables) {
  scores <- sapply(variables, function(v) get_score_index(data[[v]], v))
  score_total <- sum(scores, na.rm = TRUE)
  score_percent <- round((score_total / (length(variables) * 5)) * 100)
  return(list(percent = score_percent, scores = scores))
}

# Fonction pour obtenir couleur + label en fonction du %
get_score_style <- function(score) {
  if (score <= 20) return(list(color="#4CAF50", bouton ="🟢", label = "Handicap minimal"))
  if (score <= 40) return(list(color="#FFEB3B", bouton="🟡", label = "Handicap modéré"))
  if (score <= 60) return(list(color="#FF9800", bouton="🟠", label = "Handicap sévère"))
  if (score <= 80) return(list(color="#F44336", bouton="🔴", label = "Handicap important"))
  return(list(color="#B71C1C", bouton="🚨", label = "Handicap extrême"))
}

# ... (Tout le code précédent inchangé)

EQ5D_TEXTES <- list(
  eq_mobilite = c(
    "Je n’ai aucun problème pour marcher",
    "J’ai de légers problèmes pour marcher",
    "J’ai des problèmes modérés pour marcher",
    "J’ai de graves problèmes pour marcher",
    "Je suis incapable de marcher"
  ),
  eq_autonomie = c(
    "Je n’ai aucun problème pour prendre soin de moi",
    "J’ai de légers problèmes pour prendre soin de moi",
    "J’ai des problèmes modérés pour prendre soin de moi",
    "J’ai de graves problèmes pour prendre soin de moi",
    "Je suis incapable de prendre soin de moi"
  ),
  eq_activites = c(
    "Je n’ai aucun problème dans mes activités habituelles",
    "J’ai de légers problèmes dans mes activités habituelles",
    "J’ai des problèmes modérés dans mes activités habituelles",
    "J’ai de graves problèmes dans mes activités habituelles",
    "Je ne peux pas réaliser mes activités habituelles"
  ),
  eq_douleur = c(
    "Je n’ai pas de douleur ou de malaise",
    "J’ai une légère douleur ou un léger malaise",
    "J’ai une douleur modérée ou un malaise modéré",
    "J’ai une douleur intense ou un malaise intense",
    "J’ai une douleur extrême ou un malaise extrême"
  ),
  eq_anxiete = c(
    "Je ne suis pas anxieux(se) ou déprimé(e)",
    "Je suis légèrement anxieux(se) ou déprimé(e)",
    "Je suis modérément anxieux(se) ou déprimé(e)",
    "Je suis très anxieux(se) ou déprimé(e)",
    "Je suis extrêmement anxieux(se) ou déprimé(e)"
  )
)

get_eq5d_index <- function(data, var) {
  if (!is.null(data[[var]]) && var %in% names(EQ5D_TEXTES)) {
    return(match(data[[var]], EQ5D_TEXTES[[var]]))
  }
  return(NA)
}

compute_eq5d_score <- function(data) {
  if (!is.list(data)) return(list(index = NA, scores = NA))
  scores <- sapply(names(EQ5D_TEXTES), function(v) get_eq5d_index(data, v))
  if (any(is.na(scores))) return(list(index = NA, scores = scores))
  index <- eq5d::eq5d(
    scores = setNames(scores, c("MO", "SC", "UA", "PD", "AD")),
    version = "5L",
    country = "France",
    type = "VT",
    ignore.invalid = TRUE
  )$value
  return(list(index = index, scores = scores))
}

get_eq5d_style <- function(score) {
  if (score >= 0.9) return(list(color="#4CAF50", label="🟢 Excellent"))
  if (score >= 0.75) return(list(color="#8BC34A", label="🟢 Bon"))
  if (score >= 0.5) return(list(color="#FFEB3B", label="🟡 Moyen"))
  if (score >= 0.25) return(list(color="#FF9800", label="🔸 Faible"))
  return(list(color="#F44336", label="🔴 Très faible"))
}

EQ5D_TEXTES <- list(
  eq_mobilite = c(
    "Je n’ai aucun problème pour marcher",
    "J’ai de légers problèmes pour marcher",
    "J’ai des problèmes modérés pour marcher",
    "J’ai de graves problèmes pour marcher",
    "Je suis incapable de marcher"
  ),
  eq_autonomie = c(
    "Je n’ai aucun problème pour prendre soin de moi",
    "J’ai de légers problèmes pour prendre soin de moi",
    "J’ai des problèmes modérés pour prendre soin de moi",
    "J’ai de graves problèmes pour prendre soin de moi",
    "Je suis incapable de prendre soin de moi"
  ),
  eq_activites = c(
    "Je n’ai aucun problème dans mes activités habituelles",
    "J’ai de légers problèmes dans mes activités habituelles",
    "J’ai des problèmes modérés dans mes activités habituelles",
    "J’ai de graves problèmes dans mes activités habituelles",
    "Je ne peux pas réaliser mes activités habituelles"
  ),
  eq_douleur = c(
    "Je n’ai pas de douleur ou de malaise",
    "J’ai une légère douleur ou un léger malaise",
    "J’ai une douleur modérée ou un malaise modéré",
    "J’ai une douleur intense ou un malaise intense",
    "J’ai une douleur extrême ou un malaise extrême"
  ),
  eq_anxiete = c(
    "Je ne suis pas anxieux(se) ou déprimé(e)",
    "Je suis légèrement anxieux(se) ou déprimé(e)",
    "Je suis modérément anxieux(se) ou déprimé(e)",
    "Je suis très anxieux(se) ou déprimé(e)",
    "Je suis extrêmement anxieux(se) ou déprimé(e)"
  )
)

get_eq5d_index <- function(data, var) {
  if (!is.null(data[[var]]) && var %in% names(EQ5D_TEXTES)) {
    return(match(data[[var]], EQ5D_TEXTES[[var]]))
  }
  return(NA)
}

compute_eq5d_score <- function(data) {
  scores <- sapply(names(EQ5D_TEXTES), function(v) get_eq5d_index(data, v))
  if (any(is.na(scores))) return(list(index = NA, scores = scores))
  index <- eq5d::eq5d(
    scores = setNames(scores, c("MO", "SC", "UA", "PD", "AD")),
    version = "5L",
    country = "France",
    type = "VT",
    ignore.invalid = TRUE
  )
  return(list(index = index, scores = scores))
}

get_eq5d_style <- function(score) {
  if (score >= 0.9) return(list(color="#4CAF50", bouton = "🟢", comment ="Excellent"))
  if (score >= 0.75) return(list(color="#8BC34A", bouton = "🟢", comment ="Bon"))
  if (score >= 0.5) return(list(color="#FFEB3B", bouton ="🟡",  comment = "Moyen"))
  if (score >= 0.25) return(list(color="#FF9800", bouton ="🟠", comment = "Faible"))
  return(list(color="#F44336", bouton="🔴",  comment ="Très faible"))
}

get_hada_hadd_score <- function(data) {
  vars_hada <- c(
    "hada_tendu", "hada_peur", "hada_souci", "hada_tranquilAssis",
    "hada_estomacNoue", "hada_bougeotte", "hada_paniques"
  )
  vars_hadd <- c(
    "hadd_plaisir", "hadd_rire", "hadd_humeur", "hadd_ralenti",
    "hadd_apparence", "hadd_rejouis", "hadd_livreEmission"
  )
  
  score_hada <- sum(as.numeric(data[vars_hada]), na.rm = TRUE)
  score_hadd <- sum(as.numeric(data[vars_hadd]), na.rm = TRUE)
  
  return(list(hada = score_hada, hadd = score_hadd))
}

get_had_style <- function(score) {
  if (score <= 7) return(list(color = "#4CAF50", label = "Absence de symptomatologie"))
  if (score <= 10) return(list(color = "#FF9800", label = "Symptomatologie douteuse"))
  return(list(color = "#F44336", label = "Symptomatologie certaine"))
}

compute_bipq_score <- function(data) {
  vars <- c("bipq_affect", "bipq_control", "bipq_kine", "bipq_frequence", 
            "bipq_preocuper", "bipq_comprendre", "bipq_emotion", "bipq_durer")
  scores <- as.numeric(data[vars])
  return(list(total = sum(scores, na.rm = TRUE), scores = scores, vars = vars))
}

get_bipq_style <- function() {
  return(list(color = "#000000", bouton = "⚫", label = "Score BIPQ"))
}

# 🟢 Fonction de style pour EQ VAS
get_eq_vas_style <- function(score) {
  if (is.na(score)) return(list(color = "#BDBDBD", label = "Non renseigné"))
  if (score >= 75) return(list(color = "#4CAF50"))
  if (score >= 50) return(list(color = "#FF9800"))
  return(list(color = "#F44336"))
}

# 🧾 Formatage du score EQ-VAS (en dessous du EQ-5D)
format_eq_vas_block <- function(data) {
  score <- as.numeric(data$eq_vas)
  style <- get_eq_vas_style(score)
  
  div(style = "margin-bottom: 10px; font-weight: bold;",
      span(style = paste0("color:", style$color, "; font-size:18px;"),
           paste0("EQ-VAS : ", score)
      )
  )
}

format_sympt_accept_block <- function(data) {
  val <- tolower(trimws(data$sympt_acceptables))
  if (is.na(val) || val == "") return(NULL)
  
  if (val %in% c("oui", "yes")) {
    color <- "#4CAF50"
    label <- "Acceptables"
  } else {
    color <- "#F44336"
    label <- "Inacceptables"
  }
  
  div(style = "margin-bottom: 10px; font-weight: bold;",
      span(style = paste0("color:", color, "; font-size:18px;"),
           paste0("Acceptabilité des symptômes : ", label)
      )
  )
}

format_eval_multiparametrique <- function(data) {
  zone <- data$zone_consulte
  if (is.null(zone)) return(NULL)

  contenu <- list()

  if (zone %in% c("Les lombaires", "Les 2")) {
    vars_odi <- names(ODI_TEXTES)
    result <- compute_disability_score(data, vars_odi)
    style <- get_score_style(result$percent)

    contenu <- append(contenu, list(
      div(style = "margin-bottom: 10px; font-weight: bold;",
          tags$a(
            id = "toggle_radar_odi",
            href = "#",
            onclick = "Shiny.onInputChange('toggle_radar_odi', new Date().getTime())",
            style = paste0("color:", style$color, "; font-size:18px; text-decoration:none;"),
            paste0(style$bouton, " Score ODI : ", result$percent, "% (", style$label, ")")
          )
      ),
      hidden(div(id = "details_radar_odi", plotOutput("radar_odi")))
    ))
  }

  if (zone %in% c("Les cervicales", "Les 2")) {
    vars_ndi <- names(NDI_TEXTES)
    result <- compute_disability_score(data, vars_ndi)
    style <- get_score_style(result$percent)

    contenu <- append(contenu, list(
      div(style = "margin-bottom: 10px; font-weight: bold;",
          tags$a(
            id = "toggle_radar_ndi",
            href = "#",
            onclick = "Shiny.onInputChange('toggle_radar_ndi', new Date().getTime())",
            style = paste0("color:", style$color, "; font-size:18px; text-decoration:none;"),
            paste0(style$bouton, " Score NDI : ", result$percent, "% (", style$label, ")")
          )
      ),
      hidden(div(id = "details_radar_ndi", plotOutput("radar_ndi")))
    ))
  }

  if (!any(is.na(data[names(EQ5D_TEXTES)]))) {
    result <- compute_eq5d_score(data)
    style <- get_eq5d_style(result$index)

    contenu <- append(contenu, list(
      div(style = "margin-bottom: 10px; font-weight: bold;",
          tags$a(
            id = "toggle_radar_eq5d",
            href = "#",
            onclick = "Shiny.onInputChange('toggle_radar_eq5d', new Date().getTime())",
            style = paste0("color:", style$color, "; font-size:18px; text-decoration:none;"),
            paste0(style$bouton, " EQ-5D : ", round(result$index, 3), " (", style$comment, ")")
          )
      ),
      hidden(div(id = "details_radar_eq5d", plotOutput("radar_eq5d")))
    ))
  }
  
  contenu <- append(contenu, list(
    format_eq_vas_block(data)
  ))

  # HADA
  contenu <- append(contenu, list(
    div(style = "margin-bottom: 10px; font-weight: bold;",
        span(style = paste0("color:", get_had_style(get_hada_hadd_score(data)$hada)$color, "; font-size:18px;"),
             paste0("HADA : ", get_hada_hadd_score(data)$hada, " (", get_had_style(get_hada_hadd_score(data)$hada)$label, ")"))
    )
  ))

  # HADD
  contenu <- append(contenu, list(
    div(style = "margin-bottom: 10px; font-weight: bold;",
        span(style = paste0("color:", get_had_style(get_hada_hadd_score(data)$hadd)$color, "; font-size:18px;"),
             paste0("HADD : ", get_hada_hadd_score(data)$hadd, " (", get_had_style(get_hada_hadd_score(data)$hadd)$label, ")"))
    )
  ))
  
  format_bipq_block <- function(data) {
    result <- compute_bipq_score(data)
    style <- get_bipq_style()
    
    return(list(
      div(style = "margin-bottom: 10px; font-weight: bold;",
          tags$a(
            id = "toggle_radar_bipq",
            href = "#",
            onclick = "Shiny.onInputChange('toggle_radar_bipq', new Date().getTime())",
            style = paste0("color:", style$color, "; font-size:18px; text-decoration:none;"),
            paste0(style$bouton, " ", style$label, " : ", result$total)
          )
      ),
      hidden(div(id = "details_radar_bipq", plotOutput("radar_bipq")))
    ))
  }

  contenu <- append(contenu, format_bipq_block(data))
  
  contenu <- append(contenu, list(format_sympt_accept_block(data)))
  
  return(tagList(contenu))
  
  
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
  observeEvent(input$toggle_dr_infection, {
    toggle(id = "details_dr_infection", anim = TRUE)
  })
  
  observeEvent(input$toggle_dr_neuro, {
    toggle(id = "details_dr_neuro", anim = TRUE)
  })
  
  observeEvent(input$toggle_dr_general, {
    toggle(id = "details_dr_general", anim = TRUE)
  })
  
  observeEvent(input$toggle_chir_rachis, {
    toggle(id = "details_chir_rachis", anim = TRUE)
  })
  
  observeEvent(input$toggle_cancer_history, {
    toggle(id = "details_cancer_history", anim = TRUE)
  })
  
  observeEvent(input$toggle_rhumato, {
    toggle(id = "details_rhumato", anim = TRUE)
  })
  
  observeEvent(input$toggle_cardio, {
    toggle(id = "details_cardio", anim = TRUE)
  })
  
  output$radar_odi <- renderPlot({
    req(exists("patient_data"))
    req(patient_data$zone_consulte %in% c("Les lombaires", "Les 2"))
    
    vars <- names(ODI_TEXTES)
    scores <- sapply(vars, function(v) get_score_index(patient_data[[v]], v))
    
    # Récupération du pourcentage et de la couleur associée
    result <- compute_disability_score(patient_data, vars)
    style <- get_score_style(result$percent)
    couleur <- style$color
    couleur_fill <- scales::alpha(couleur, 0.4)
    
    # Repositionner les étiquettes pour éviter le chevauchement
    nom_axes <- gsub("odi_", "", vars)
    # On place "debout" à la fin plutôt que "manutentions"
    nom_axes[nom_axes == "debout"] <- "TEMP_DEBOUT"
    nom_axes[nom_axes == "manutentions"] <- "debout"
    nom_axes[nom_axes == "TEMP_DEBOUT"] <- "manutentions"
    
    # Réorganiser les scores dans le même ordre
    scores <- scores[match(paste0("odi_", nom_axes), vars)]
    
    # Création du dataframe avec valeurs max et min
    df <- as.data.frame(t(scores))
    colnames(df) <- nom_axes
    df <- rbind(rep(5, length(nom_axes)), rep(0, length(nom_axes)), df)
    
    # Radar chart avec thème stylé
    radarchart(df,
               axistype = 1,
               pcol = couleur,
               pfcol = couleur_fill,
               plwd = 2,
               cglcol = "grey90",
               cglty = 1,
               cglwd = 1,
               caxislabels = 0:5,
               axislabcol = "black",
               axislabcex = 1.2,
               vlcex = 1.4,
               cex.main = 2,
               title = "Dimensions ODI",
               seg = 5
    )
  })
  
  output$radar_ndi <- renderPlot({
    req(exists("patient_data"))
    req(patient_data$zone_consulte %in% c("Les cervicales", "Les 2"))
    
    vars <- names(NDI_TEXTES)
    scores <- sapply(vars, function(v) get_score_index(patient_data[[v]], v))
    
    # Récupération du pourcentage et de la couleur associée
    result <- compute_disability_score(patient_data, vars)
    style <- get_score_style(result$percent)
    couleur <- style$color
    couleur_fill <- scales::alpha(couleur, 0.4)
    
    # Repositionner les étiquettes si besoin (ex: mettre "travail" à la place d’un autre si ça chevauche trop)
    nom_axes <- gsub("ndi_", "", vars)
    # Par exemple, switch entre "sociale" et "mauxTete"
    nom_axes[nom_axes == "mauxTete"] <- "TEMP_MAUX"
    nom_axes[nom_axes == "sociale"] <- "mauxTete"
    nom_axes[nom_axes == "TEMP_MAUX"] <- "sociale"
    
    # Réorganiser les scores dans le même ordre
    scores <- scores[match(paste0("ndi_", nom_axes), vars)]
    
    # Création du dataframe avec valeurs max et min
    df <- as.data.frame(t(scores))
    colnames(df) <- nom_axes
    df <- rbind(rep(5, length(nom_axes)), rep(0, length(nom_axes)), df)
    
    # Radar chart avec le thème badass
    radarchart(df,
               axistype = 1,
               pcol = couleur,
               pfcol = couleur_fill,
               plwd = 2,
               cglcol = "grey90",
               cglty = 1,
               cglwd = 1,
               caxislabels = 0:5,
               axislabcol = "black",
               axislabcex = 1.2,
               vlcex = 1.4,
               cex.main = 2,
               title = "Dimensions NDI",
               seg = 5
    )
  })
  
  observeEvent(input$toggle_radar_odi, {
    toggle("details_radar_odi", anim = TRUE)
  })
  
  observeEvent(input$toggle_radar_ndi, {
    toggle("details_radar_ndi", anim = TRUE)
  })
  
  observeEvent(input$toggle_radar_eq5d, {
    toggle("details_radar_eq5d", anim = TRUE)
  })
  
  output$radar_eq5d <- renderPlot({
    req(exists("patient_data"))
    req(!is.null(patient_data$eq_mobilite))  # Tu peux affiner selon ce que tu veux rendre obligatoire
    
    scores <- sapply(names(EQ5D_TEXTES), function(v) get_eq5d_index(patient_data, v))
    style <- get_eq5d_style(compute_eq5d_score(patient_data)$index)
    couleur <- style$color
    couleur_fill <- scales::alpha(couleur, 0.4)
    
    df <- as.data.frame(t(scores))
    colnames(df) <- c("Mobilité", "Autonomie", "Activités", "Douleur", "Anxiété")
    df <- rbind(rep(5, 5), rep(1, 5), df)
    
    radarchart(df,
               axistype = 1,
               pcol = couleur,
               pfcol = couleur_fill,
               plwd = 2,
               cglcol = "grey90",
               cglty = 1,
               cglwd = 1,
               caxislabels = 1:5,
               axislabcol = "black",
               axislabcex = 1.2,
               vlcex = 1.4,
               cex.main = 2,
               title = "Dimensions EQ-5D",
               seg = 5)
  })
  
  output$radar_bipq <- renderPlot({
    req(exists("patient_data"))
    result <- compute_bipq_score(patient_data)
    library(fmsb)
    df <- as.data.frame(t(result$scores))
    colnames(df) <- gsub("bipq_", "", result$vars)
    df <- rbind(rep(10, length(result$vars)), rep(0, length(result$vars)), df)
    radarchart(df,
               axistype = 1,
               pcol = "#000000",
               pfcol = scales::alpha("#000000", 0.3),
               plwd = 2,
               cglcol = "grey90",
               cglty = 1,
               cglwd = 1,
               caxislabels = 0:10,
               axislabcol = "black",
               axislabcex = 1.2,
               vlcex = 1.4,
               cex.main = 2,
               title = "Dimensions BIPQ",
               seg = 10
    )
  })
  
  observeEvent(input$toggle_radar_bipq, {
    toggle("details_radar_bipq", anim = TRUE)
  })
  
  
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
          patient_data <<- df[df$hash_id == hash, ]  # ✅ RENDU GLOBAL
          
          output$search_status <- renderUI({
            HTML('<span style="color:green; font-weight:bold;">✅ Patient retrouvé !</span>')
          })
          output$patient_dashboard <- renderUI({
            tagList(
              tags$head(tags$style(HTML(".encarts-row {
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
              
              # Première ligne : Identité + Évaluation multiparamétrique
              div(class = "encarts-row",
                  div(class = "encart-box",
                      HTML(format_identite(patient_data))
                  ),
                  div(class = "encart-box",
                      tags$h4("Évaluation multiparamétrique"),
                      format_eval_multiparametrique(patient_data)
                  )
              ),
              
              # Deuxième ligne : Drapeaux rouges + Antécédent médicaux + Traitement antérieur
              div(class = "encarts-row",
                  div(class = "encart-box",
                      tags$h4("Drapeaux rouges"),
                      format_drapeau_neuro(patient_data),
                      format_drapeau_general(patient_data),
                      format_drapeau_infection(patient_data),
                      format_drapeau_psychologique(patient_data)
                  ),
                  div(class = "encart-box",
                      tags$h4("Antécédents médicaux"),
                      format_antecedent_chirurgie_rachis(patient_data),
                      format_cancer_history(patient_data),
                      format_antecedents_rhumato(patient_data),
                      format_antecedents_cardio(patient_data),
                      format_antecedent_tabac(patient_data)  # ⬅️ ajoute cette ligne !
                  )
                  ,
                  div(class = "encart-box",
                      tags$h4("Traitement antérieur"),
                      format_traitement_kine(patient_data),
                      format_traitement_osteo(patient_data),
                      format_traitement_medoc(patient_data),
                      format_traitement_infiltration(patient_data)
                    
                  )
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




