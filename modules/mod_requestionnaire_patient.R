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
        ".title-logo { display: none; }",
        ".eq5d-grid, .odi-grid, .ndi-grid { display: grid; grid-template-columns: repeat(3, 1fr); gap: 20px; }",
        ".eq5d-item, .eq5d-slider, .odi-item, .ndi-item { padding: 10px; background: white; border-radius: 10px; box-shadow: 0 2px 5px rgba(0,0,0,0.1); }",
        ".eq5d-item label, .odi-item label, .ndi-item label { display: block; margin-bottom: 5px; }"
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
                       selectInput(
                         inputId = ns("situation_pro"),
                         label = "Situation professionnelle actuelle",
                         choices = c("",
                                     "Travail normal",
                                     "Arrêt de travail",
                                     "Mi-temps thérapeutique",
                                     "Invalidité / Maladie professionnelle"
                         ),
                         selected = ""
                         
                       )
                     ),
                     
                       selectInput(ns("duree_effet_douleur"), "A qeulle point vos douleurs affectent votre pratique?", 
                                   choices = c("",
                                               "Je pratique normalement", 
                                               "J'ai diminué ou adapté le sport",
                                               "J'ai arrété la pratique",
                                               "Je ne pratique habituellement pas de sport"
                                   ), selected = "")),
                     
                     tags$h4("Localisation principale de la douleur"),
                     radioButtons(ns("zone_consulte"), "Consultez-vous pour :", 
                                  choices = c("Les lombaires", "Les cervicales", "Les 2"), 
                                  selected = "", 
                                  inline = TRUE),
                     
                     conditionalPanel(
                       condition = sprintf("input['%s'] == 'Les lombaires' || input['%s'] == 'Les 2'", ns("zone_consulte"), ns("zone_consulte")),
                       tags$h4("ODI - Questionnaire lombaire"),
                       div(class = "odi-grid",
                           lapply(1:10, function(i) {
                             questions <- list(
                               "Intensité de la douleur" = c("Je n'ai pas mal actuellement.", "La douleur est très légère actuellement.", "La douleur est modérée actuellement.", "La douleur est plutôt intense actuellement.", "La douleur est très intense actuellement.", "La douleur est la pire que l’on puisse imaginer actuellement."),
                               "Soins personnels" = c("Je peux prendre soin de moi normalement, sans augmenter la douleur.", "Je peux prendre soin de moi normalement, mais c’est très douloureux.", "Cela me fait mal de prendre soin de moi, et je le fait lentement et en faisant attention.", "J’ai besoin d’aide, mais dans l'ensemble je parviens à me débrouiller seul.", "J’ai besoin d’aide tous les jours pour la plupart de ces gestes quotidiens.", "Je ne m'habille pas, me lave avec difficulté et reste au lit."),
                               "Manutention de charges" = c("Je peux soulever des charges lourdes sans augmenter mon mal de dos", "Je peux soulever des charges lourdes mais cela augmente ma douleur", "La douleur m'empêche de soulever des charges lourdes à partir du sol mais j'y parviens si la charge est bien placée", "La douleur m'empêche de soulever des charges lourdes mais je peux déplacer des charges légères ou de poids moyen", "Je peux seulement soulever des objets très légers", "Je ne peux soulever ni transporter quoi que ce soit"),
                               "Marche à pied" = c("La douleur ne limite absolument pas mes déplacements", "La douleur m'empêche de marcher plus de 2 km", "La douleur m'empêche de marcher plus de 1 km", "La douleur m'empêche de marcher plus de 500 m", "Je me déplace seulement avec une canne ou des béquilles", "Je reste au lit la plupart du temps et je me traîne seulement jusqu'au WC"),
                               "Position assise" = c("Je peux rester assis sur un siège aussi longtemps que je veux.", "Je peux rester assis aussi longtemps que je veux mais seulement sur mon siège favori.", "La douleur m'empêche de rester assis plus d'une heure.", "La douleur m'empêche de rester assis plus d'1/2 heure.", "La douleur m'empêche de rester assis plus de 10 minutes.", "La douleur m'empêche de rester assis."),
                               "Position debout" = c("Je peux rester debout aussi longtemps que je veux sans augmenter la douleur.", "Je peux rester debout aussi longtemps que je veux mais cela augmente la douleur.", "La douleur m'empêche de rester debout plus d'une heure.", "La douleur m'empêche de rester debout plus d'1/2 heure.", "La douleur m'empêche de rester debout plus de 10 minutes.", "La douleur m'empêche de rester debout."),
                               "Sommeil" = c("Mon sommeil n’est jamais perturbé par la douleur.", "Mon sommeil est parfois perturbé par la douleur", "A cause de la douleur, je dors moins de 6 heures", "A cause de la douleur, je dors moins de 4 heures", "A cause de la douleur, je dors moins de 2 heures", "La douleur m'empêche complètement de dormir"),
                               "Vie sexuelle" = c("Ma vie sexuelle n'est pas modifiée et n'augmente pas mon mal de dos", "Ma vie sexuelle n'est pas modifiée, mais elle augmente la douleur", "Ma vie sexuelle est pratiquement normale, mais elle est très douloureuse", "Ma vie sexuelle est fortement limitée par la douleur", "Ma vie sexuelle est presque inexistante à cause de la douleur", "La douleur m'interdit toute vie sexuelle"),
                               "Vie sociale" = c("Ma vie sociale est normale et n’a pas d’effet sur la douleur", "Ma vie sociale est normale, mais elle augmente la douleur", "La douleur n'a pas d'effet sur ma vie sociale, sauf pour des activités demandant plus d'énergie", "La douleur a réduit ma vie sociale et je ne sors plus autant qu'auparavant", "La douleur a limité ma vie sociale à ce qui se passe chez moi, à la maison", "Je n'ai plus de vie sociale à cause du mal de dos"),
                               "Déplacements" = c("Je peux me déplacer n'importe où sans effet sur mon mal de dos", "Je peux me déplacer n'importe où, mais cela augmente la douleur", "La douleur est pénible mais je supporte des trajets de plus de 2 heures", "La douleur me limite à des trajets de moins d'une heure", "La douleur me limite aux courts trajets indispensables, de moins de 30 minutes", "La douleur m'empêche de me déplacer, sauf pour aller voir le docteur ou me rendre à l'hôpital")
                             )
                             noms <- names(questions)
                             div(class = "odi-item",
                                 tags$h5(noms[i]),
                                 radioButtons(ns(paste0("odi_", i)), label = NULL, choices = questions[[i]], selected = "")
                             )
                           })
                       )),
                     
                     conditionalPanel(
                       condition = sprintf("input['%s'] == 'Les cervicales' || input['%s'] == 'Les 2'", ns("zone_consulte"), ns("zone_consulte")),
                       tags$h4("NDI - Questionnaire cervicale"),
                       div(class = "ndi-grid",
                           lapply(1:10, function(i) {
                             questions <- list(
                               "Intensité de la douleur" = c("Aucune douleur", "Douleur légère", "Douleur modérée", "Douleur forte", "Douleur très forte", "Douleur insupportable"),
                               "Soins personnels" = c("Aucune difficulté", "Légère difficulté", "Difficulté modérée", "Grande difficulté", "Dépendance partielle", "Dépendance totale"),
                               "Soulever des objets" = c("Sans difficulté", "Légère difficulté", "Difficulté modérée", "Grande difficulté", "Extrême difficulté", "Impossible"),
                               "Lire" = c("Aucune difficulté", "Légère difficulté", "Difficulté modérée", "Grande difficulté", "Extrême difficulté", "Impossible"),
                               "Maux de tête" = c("Jamais", "Rarement", "Parfois", "Souvent", "Très souvent", "Toujours"),
                               "Concentration" = c("Aucune difficulté", "Légère difficulté", "Difficulté modérée", "Grande difficulté", "Extrême difficulté", "Impossible"),
                               "Travail" = c("Sans difficulté", "Légère difficulté", "Difficulté modérée", "Grande difficulté", "Extrême difficulté", "Impossible"),
                               "Activité quotidienne" = c("Sans difficulté", "Légère difficulté", "Difficulté modérée", "Grande difficulté", "Extrême difficulté", "Impossible"),
                               "Sommeil" = c("Pas de problème", "Léger problème", "Problème modéré", "Gros problème", "Très gros problème", "Impossible de dormir"),
                               "Vie sociale" = c("Aucun impact", "Léger impact", "Impact modéré", "Fort impact", "Très fort impact", "Vie sociale impossible")
                             )
                             noms <- names(questions)
                             div(class = "ndi-item",
                                 tags$h5(noms[i]),
                                 radioButtons(ns(paste0("ndi_", i)), label = NULL, choices = questions[[i]], selected = "")
                             )
                           }))),
                     
                     tags$h4("EQ-5D-5L : Votre état de santé aujourd'hui"),
                     div(class = "eq5d-grid",
                         list(
                           list(id = "eq_mobilite", label = "Mobilité", choices = c(
                             "Je n’ai aucun problème pour marcher",
                             "J’ai de légers problèmes pour marcher",
                             "J’ai des problèmes modérés pour marcher",
                             "J’ai de graves problèmes pour marcher",
                             "Je suis incapable de marcher"
                           )),
                           list(id = "eq_autonomie", label = "Autonomie", choices = c(
                             "Je n’ai aucun problème pour prendre soin de moi",
                             "J’ai de légers problèmes pour prendre soin de moi",
                             "J’ai des problèmes modérés pour prendre soin de moi",
                             "J’ai de graves problèmes pour prendre soin de moi",
                             "Je suis incapable de prendre soin de moi"
                           )),
                           list(id = "eq_activites", label = "Activités habituelles", choices = c(
                             "Je n’ai aucun problème dans mes activités habituelles",
                             "J’ai de légers problèmes dans mes activités habituelles",
                             "J’ai des problèmes modérés dans mes activités habituelles",
                             "J’ai de graves problèmes dans mes activités habituelles",
                             "Je ne peux pas réaliser mes activités habituelles"
                           )),
                           list(id = "eq_douleur", label = "Douleur/Malaise", choices = c(
                             "Je n’ai pas de douleur ou de malaise",
                             "J’ai une légère douleur ou un léger malaise",
                             "J’ai une douleur modérée ou un malaise modéré",
                             "J’ai une douleur intense ou un malaise intense",
                             "J’ai une douleur extrême ou un malaise extrême"
                           )),
                           list(id = "eq_anxiete", label = "Anxiété/Dépression", choices = c(
                             "Je ne suis pas anxieux(se) ou déprimé(e)",
                             "Je suis légèrement anxieux(se) ou déprimé(e)",
                             "Je suis modérément anxieux(se) ou déprimé(e)",
                             "Je suis très anxieux(se) ou déprimé(e)",
                             "Je suis extrêmement anxieux(se) ou déprimé(e)"
                           ))
                         ) %>% lapply(function(dim) {
                           div(class = "eq5d-item",
                               tags$h5(dim$label),
                               radioButtons(ns(dim$id), label = NULL, choices = dim$choices, selected = "")
                           )
                         }) %>% append(list(
                           div(class = "eq5d-slider",
                               tags$h5("Auto-évaluation de votre santé générale aujourd'hui"),
                               sliderInput(ns("eq_vas"),
                                           "Indiquez sur cette échelle de 0 (le pire état de santé imaginable) à 100 (le meilleur état de santé imaginable), votre état de santé actuel :",
                                           min = 0, max = 100, value = 50, step = 1
                               )
                           )
                         ))
                     ),
                     
                     tags$h4("Échelle HAD : Hospital Anxiety and Depression scale"),
                     div(style = "display: grid; grid-template-columns: repeat(3, 1fr); gap: 20px;",
                         lapply(1:14, function(i) {
                           questions <- list(
                             "Je me sens tendu(e) ou énervé(e)" = c("La plupart du temps" = 3, "Souvent" = 2, "De temps en temps" = 1, "Jamais" = 0),
                             "Je prends plaisir aux mêmes choses qu’autrefois" = c("Oui, tout autant" = 0, "Pas autant" = 1, "Un peu seulement" = 2, "Presque plus" = 3),
                             "J’ai une sensation de peur comme si quelque chose d’horrible allait m’arriver" = c("Oui, très nettement" = 3, "Oui, mais pas trop grave" = 2, "Un peu, mais cela ne m’inquiète pas" = 1, "Pas du tout" = 0),
                             "Je ris facilement et vois le bon côté des choses" = c("Autant que par le passé" = 0, "Plus autant qu’avant" = 1, "Vraiment moins qu’avant" = 2, "Plus du tout" = 3),
                             "Je me fais du souci" = c("Très souvent" = 3, "Assez souvent" = 2, "Occasionnellement" = 1, "Très occasionnellement" = 0),
                             "Je suis de bonne humeur" = c("Jamais" = 3, "Rarement" = 2, "Assez souvent" = 1, "La plupart du temps" = 0),
                             "Je peux rester tranquillement assis(e) à ne rien faire et me sentir décontracté(e)" = c("Oui, quoi qu’il arrive" = 0, "Oui, en général" = 1, "Rarement" = 2, "Jamais" = 3),
                             "J’ai l’impression de fonctionner au ralenti" = c("Presque toujours" = 3, "Très souvent" = 2, "Parfois" = 1, "Jamais" = 0),
                             "J’éprouve des sensations de peur et j’ai l’estomac noué" = c("Très souvent" = 3, "Assez souvent" = 2, "Parfois" = 1, "Jamais" = 0),
                             "Je ne m’intéresse plus à mon apparence" = c("Plus du tout" = 3, "Je n’y accorde pas autant d’attention que je devrais" = 2, "Il se peut que je n’y fasse plus autant attention" = 1, "J’y prête autant d’attention que par le passé" = 0),
                             "J’ai la bougeotte et n’arrive pas à tenir en place" = c("Oui, c’est tout à fait le cas" = 3, "Un peu" = 2, "Pas tellement" = 1, "Pas du tout" = 0),
                             "Je me réjouis d’avance à l’idée de faire certaines choses" = c("Autant qu’avant" = 0, "Un peu moins qu’avant" = 1, "Bien moins qu’avant" = 2, "Presque jamais" = 3),
                             "J’éprouve des sensations soudaines de panique" = c("Vraiment très souvent" = 3, "Assez souvent" = 2, "Pas très souvent" = 1, "Jamais" = 0),
                             "Je peux prendre plaisir à un bon livre ou à une bonne émission" = c("Souvent" = 0, "Parfois" = 1, "Rarement" = 2, "Très rarement" = 3)
                           )
                           nom <- names(questions)[i]
                           div(class = "had-item",
                               tags$h5(nom),
                               radioButtons(ns(paste0("had_", i)), label = NULL, choices = questions[[i]], selected = "")
                           )
                         })
                     ),
                     tags$h4("Brief Illness Perception Questionnaire (BIPQ)"),
                     div(class = "bipq-grid",
                         lapply(1:8, function(i) {
                           questions <- c(
                             "Dans quelle mesure vos douleurs affectent-elles votre vie ?",
                             "Dans quelle mesure pensez-vous pouvoir contrôler vos douleurs ?",
                             "Dans quelle mesure pensez-vous que la kinésithérapie peut soulager vos douleurs ?",
                             "À quel point les douleurs sont-elles fréquentes ?",
                             "Dans quelle mesure êtes-vous préoccupé par vos douleurs ?",
                             "Dans quelle mesure pensez-vous comprendre vos douleurs ?",
                             "Dans quelle mesure vos douleurs vous affectent-elles sur le plan émotionnel (par exemple, vous mettent-elles en colère, vous font-elles peur, vous bouleversent-elles ou vous dépriment-elles ?)",
                             "À quel point pensez-vous que ces douleurs dureront ?"
                           )
                           labels_min <- c(
                             "Très peu", "Aucun contrôle", "Pas du tout", "Jamais", "Pas du tout concerné", "Ne comprend pas du tout", "Pas du tout affecté émotionnellement", "Très peu de temps"
                           )
                           labels_max <- c(
                             "Extrêmement", "Contrôle extrême", "Extrêmement utile", "Tout le temps", "Extrêmement préoccupé", "Comprend très bien", "Très affecté émotionnellement", "Pour toujours"
                           )
                           div(class = "bipq-item",
                               tags$h5(questions[i]),
                               sliderInput(ns(paste0("bipq_", i)), label = NULL, min = 0, max = 10, value = 5, step = 1,
                                           ticks = FALSE, post = NULL)
                           )
                         })
                     ),
                     
                     actionButton(ns("save_btn"), " 💾Sauvegarder"),
                     htmlOutput(ns("save_status"))
                     
              )
              
            )
        )
    ))
}








mod_questionnaire_patient_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    observeEvent(input$save_btn, {
      tryCatch({
        
        # 🔁 Étape 1 - Récupération et nettoyage des inputs
        values <- reactiveValuesToList(input)
        values <- lapply(values, function(x) {
          if (length(x) == 0 || (is.character(x) && x == "")) return(NA)
          return(x)
        })
        
        # 📋 Étape 2 - Champs requis de base
        required_fields <- c(
          "nom", "prenom", "naissance",
          paste0("had_", 1:14),
          paste0("bipq_", 1:8),
          "eq_mobilite", "eq_autonomie", "eq_activites", "eq_douleur", "eq_anxiete", "eq_vas", "situation_pro",  "duree_effet_douleur",
          "zone_consulte"
        )
        
        # ✅ Conditions spécifiques avec sécurité
        
        if (isTruthy(input$zone_consulte) && input$zone_consulte %in% c("Les lombaires", "Les 2")) {
          required_fields <- c(required_fields, paste0("odi_", 1:10))
        }
        if (isTruthy(input$zone_consulte) && input$zone_consulte %in% c("Les cervicales", "Les 2")) {
          required_fields <- c(required_fields, paste0("ndi_", 1:10))
        }
        
        # 🚨 Étape 3 - Vérifie les champs manquants
        missing_fields <- sapply(required_fields, function(field) {
          is.null(values[[field]]) || is.na(values[[field]]) || (is.character(values[[field]]) && values[[field]] == "")
        })
        
        if (any(missing_fields)) stop("❌ Veuillez remplir tous les champs visibles à l'écran")
        
        # 📅 Étape 4 - Vérification date
        if (!grepl("^\\d{2}/\\d{2}/\\d{4}$", input$naissance)) {
          stop("❌ Veuillez remplir tous les champs requis et les dates au format jj/mm/aaaa")
        }
        
        # 🔐 Étape 5 - Hash et date
        values$hash_id <- generate_hash(input$nom, input$prenom, input$naissance)
        values$date_sauvegarde <- format(Sys.Date(), "%d/%m/%Y")
        
        # 🚫 Étape 5b - Supprime les données identifiantes
        values$nom <- NULL
        values$prenom <- NULL
        values$naissance <- NULL
        
        # 🏷️ Étape 6 - Renommage pour analyse
        rename_map <- c(
          odi_1 = "odi_douleur", odi_2 = "odi_soins", odi_3 = "odi_manutentions", odi_4 = "odi_marche",
          odi_5 = "odi_assis", odi_6 = "odi_debout", odi_7 = "odi_sommeil", odi_8 = "odi_sexe",
          odi_9 = "odi_sociale", odi_10 = "odi_deplacements",
          ndi_1 = "ndi_douleur", ndi_2 = "ndi_soins", ndi_3 = "ndi_manutentions", ndi_4 = "ndi_lire",
          ndi_5 = "ndi_mauxTete", ndi_6 = "ndi_concentration", ndi_7 = "ndi_travail", ndi_8 = "ndi_actiQuoti",
          ndi_9 = "ndi_sommeil", ndi_10 = "ndi_sociale",
          had_1 = "hada_tendu", had_2 = "hadd_plaisir", had_3 = "hada_peur", had_4 = "hadd_rire",
          had_5 = "hada_souci", had_6 = "hadd_humeur", had_7 = "hada_tranquilAssis", had_8 = "hadd_ralenti",
          had_9 = "hada_estomacNoue", had_10 = "hadd_apparence", had_11 = "hada_bougeotte", had_12 = "hadd_rejouis",
          had_13 = "hada_paniques", had_14 = "hadd_livreEmission",
          bipq_1 = "bipq_affect", bipq_2 = "bipq_control", bipq_3 = "bipq_kine", bipq_4 = "bipq_frequence",
          bipq_5 = "bipq_preocuper", bipq_6 = "bipq_comprendre", bipq_7 = "bipq_emotion", bipq_8 = "bipq_durer"
        )
        names(values) <- ifelse(names(values) %in% names(rename_map), rename_map[names(values)], names(values))
        
        # 💾 Étape 7 - Sauvegarde CSV
        row <- as.data.frame(values, stringsAsFactors = FALSE)
        path <- "data/patients_PROM2.csv"
        if (!dir.exists("data")) dir.create("data")
        if (file.exists(path)) {
          write.table(row, path, sep = ";", row.names = FALSE, col.names = FALSE, append = TRUE)
        } else {
          write.table(row, path, sep = ";", row.names = FALSE, col.names = TRUE, append = FALSE)
        }
        
        # ✅ Étape 8 - Feedback user
        output$save_status <- renderUI({
          HTML("<span style='color:green; font-weight:bold;'>✅ Données sauvegardées avec succès !</span>")
        })
        
      }, error = function(e) {
        output$save_status <- renderUI({
          HTML(paste0("<span style='color:red; font-weight:bold;'>", e$message, "</span>"))
        })
      })
    })
    
    
    
    
  })
}