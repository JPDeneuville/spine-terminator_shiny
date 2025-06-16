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
                     fluidRow(
                       column(4, numericInput(ns("taille"), "Taille (cm)", value = NA, min = 30, max = 300)),
                       column(4, numericInput(ns("poids"), "Poids (kg)", value = NA, min = 30, max = 300)),
                       column(4, radioButtons(ns("sexe"), "Sexe", choices = c("Homme", "Femme"), selected = character(0)))
                     ),
                     
                     radioButtons(ns("statut"), "Statut professionnel", choices = c("Étudiant", "Actif", "Retraité"), selected = character(0)),
                     conditionalPanel(
                       condition = sprintf("input['%s'] == 'Actif' || input['%s'] == 'Retraité'", ns("statut"), ns("statut")),
                       textInput(ns("metier"), "Métier ou ancien métier"),
                       selectInput(ns("pcs"), "Catégorie socio-professionnelle", 
                                   choices = c("",
                                     "PCS1 - Agriculteurs exploitants / Agricultrices exploitantes",
                                     "PCS2 - Artisans / Artisanes, commerçants / commerçantes et chefs / cheffes d'entreprise",
                                     "PCS3 - Cadres et professions intellectuelles supérieures",
                                     "PCS4 - Professions intermédiaires",
                                     "PCS5 - Employés / Employées",
                                     "PCS6 - Ouvriers / Ouvrières"
                                   ),selected = "")
                     ),
                     conditionalPanel(
                       condition = sprintf("input['%s'] == 'Actif'", ns("statut"), ns("statut")),
                       
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
                     
                      tags$h4("Pratique sportive"),
                     radioButtons(ns("sport_pratique"), "Pratiquez-vous un sport ?", choices = c("Oui", "Non"), selected = character(0)),
                     
                     conditionalPanel(
                       condition = sprintf("input['%s'] == 'Oui'", ns("sport_pratique")),
                       textInput(ns("sport"), "Quel sport?"),
                       selectInput(ns("frequence_semaine"), "Combien de fois par semaine?", 
                                   choices = c("",
                                     "1", 
                                     "2",
                                     "3",
                                     "4",
                                     "5",
                                     "6",
                                     "7"
                                   ), selected = ""),
                     selectInput(ns("duree_entrainement"), "Combien dure un entrainement typique?", 
                                 choices = c("",
                                   "30 min", 
                                   "1 heure",
                                   "1 heure et 30 min",
                                   "2 heures",
                                   "2 heures et 30 min",
                                   "3 heures",
                                   "3 heures et 30 min",
                                   "4 heures"
                                 ), selected = ""),
                     selectInput(ns("duree_entrainement"), "A qeulle point vos douleurs affectent votre pratique?", 
                                 choices = c("",
                                   "Je pratique normalement", 
                                   "J'ai diminué ou adapté le sport",
                                   "J'ai arrété la pratique"
                                 ), selected = "")),
                       
                       
                       
                       
                       
                       
                       
                       # ATCD médicaux
                     tags$h4("Antécédents médicaux"),
                     
                     radioButtons(ns("chirurgie_rachis"), "Avez-vous déjà été opéré du dos, des lombaires ou des cervicales ?", choices = c("Non", "Oui"), selected = character(0)),
                     conditionalPanel(
                       condition = sprintf("input['%s'] == 'Oui'", ns("chirurgie_rachis")),
                       selectInput(ns("type_chirurgie"), "Type de chirurgie", 
                                   choices = c("", "Discectomie (hernie discale)", "Arthrodèse", "Prothèse de disque", "Autre"), selected = ""),
                       textInput(ns("date_chirurgie"), "Date de la chirurgie (JJ/MM/AAAA)")
                     ),
                     
                     radioButtons(ns("chir_recent"), "Avez-vous subi une chirurgie, quelle que soit la raison, dans les 12 derniers mois ?", choices = c("Non", "Oui"), selected = character(0)),
                     conditionalPanel(
                       condition = sprintf("input['%s'] == 'Oui'", ns("chir_recent")),
                       textInput(ns("chir_recent_details"), "Pour quelle raison ?")
                     ),
                     
                     tags$h4("Souffrez-vous de l'une des affections suivantes ?"),
                     lapply(1:6, function(i) {
                       aff_id <- paste0("atcd_rhumato_", i)
                       aff_labels <- c(
                         "Spondylarthrite ankylosante", "Polyarthrite Rhumatoïde", "Maladie de Crohn",
                         "Rectocolites hémorragiques", "Uvéite", "Psoriasis"
                       )
                       fluidRow(
                         column(6, aff_labels[i]),
                         column(3, radioButtons(ns(aff_id), NULL, choices = c("Oui", "Non"), inline = TRUE, selected = character(0)))
                       )
                     }),
                     fluidRow(
                       column(6, "Crise de goutte"),
                       column(3, radioButtons(ns("goutte"), NULL, choices = c("Oui", "Non"), inline = TRUE, selected = character(0)))
                     ),
                     
                     tags$h4("Souffrez-vous d’une des affections suivantes ?"),
                     lapply(1:5, function(i) {
                       aff_id <- paste0("atcd_cardio_", i)
                       aff_labels <- c(
                         "Cholestérol", "Hypertension", "Diabète",
                         "Problèmes cardiaques", "Artérite des membres inférieurs"
                       )
                       fluidRow(
                         column(6, aff_labels[i]),
                         column(3, radioButtons(ns(aff_id), NULL, choices = c("Oui", "Non"), inline = TRUE, selected = character(0)))
                       )
                     }),
                     
                     tags$h4("Fumez-vous ?"),
                     radioButtons(ns("tabac"), "Fumeur actuel", choices = c("Non", "Oui"), selected = character(0)),
                     
                     tags$h4("Parfois, certaines personnes vivent des expériences particulièrement terrifiantes, horribles ou traumatisantes. Par exemple :"),
                     tags$ol(
                       tags$li("Un accident ou un incendie sérieux"),
                       tags$li("Une agression ou un abus physique et/ou sexuel"),
                       tags$li("Un tremblement de terre ou une inondation importante"),
                       tags$li("Une guerre"),
                       tags$li("Voir quelqu'un être tué ou sérieusement blessé"),
                       tags$li("Un proche mort d'un suicide ou d'un homicide"),
                       tags$li("Être régulièrement confronté à des gens rapportant des situations horribles (Ex: policier prenant des dépositions de crime, travailleur social confronté à des abus sur mineur, etc...)")
                     ),
                     radioButtons(ns("trauma_experience"), "Avez-vous vécu une de ces expériences ?", choices = c("Non", "Oui"), selected = character(0)),
                     
                     conditionalPanel(
                       condition = sprintf("input['%s'] == 'Oui'", ns("trauma_experience")),
                       tags$h4("Dans les mois précédents, avez-vous :"),
                       lapply(1:5, function(i) {
                         symp_ids <- paste0("ptsd_sympt_", i)
                         symp_labels <- c(
                           "Eu des cauchemars ou des pensées/visions à propos de ces événements ?",
                           "Essayé de ne pas penser à l'événement ou s'est donné beaucoup de mal pour éviter les situations qui rappelaient l'événement ?",
                           "Été constamment sur vos gardes, sur le qui-vive ou facilement surpris(e) ?",
                           "Eu une sensation de détachement ou d'insensibilité vis à vis des personnes vous entourant ou des activités que vous faites ?",
                           "Eu un sentiment de culpabilité, sans pouvoir vous empêcher de vous en vouloir ou d'en vouloir aux autres pour ce qui vous est arrivé ?"
                         )
                         fluidRow(
                           column(6, symp_labels[i]),
                           column(3, radioButtons(ns(symp_ids), NULL, choices = c("Oui", "Non"), inline = TRUE, selected = character(0)))
                         )
                       })
                     ),
                     
                     # ATCD CANCER
                     tags$h4("Avez-vous eu un cancer ?"),
                     radioButtons(ns("cancer_diagnosed"), "Avez-vous eu un cancer ?", choices = c("Oui", "Non"), selected = character(0)),
                     
                     conditionalPanel(
                       condition = sprintf("input['%s'] == 'Oui'", ns("cancer_diagnosed")),
                       
                       tags$h4("Quel organe était touché ?"),
                       lapply(1:6, function(i) {
                         org_ids <- paste0("organe_cancer_", i)
                         org_labels <- c("Thyroïde", "Poumon", "Sein", "Rein", "Prostate", "Autre")
                         fluidRow(
                           column(6, org_labels[i]),
                           column(3, radioButtons(ns(org_ids), NULL, choices = c("Oui", "Non"), inline = TRUE, selected = character(0)))
                         )
                       }),
                       
                       conditionalPanel(
                         condition = sprintf("input['%s'] == 'Oui'", ns("organe_cancer_6")),
                         textInput(ns("cancer_autre"), "Précisez")
                       ),
                       
                       radioButtons(ns("cancer_diagnostic_age"), "Quand le diagnostic vous a-t-il été fait ?", 
                                    choices = c("Moins d'un an", "Entre 1 et 2 ans", "Entre 2 et 5 ans", "Plus de 5 ans"),
                                    selected = character(0)
                       )
                     ),
                     
                     tags$h4("Santé actuelle"),
                     lapply(1:4, function(i) {
                       sympt_ids <- paste0("sante_actuelle_", i)
                       sympt_labels <- c(
                         "Avez-vous l'impression d'avoir perdu de la force dans la/les jambe(s) ?",
                         "Avez-vous l'impression d'avoir perdu de la force dans le/les bras ?",
                         "Avez-vous l'impression d'avoir perdu de la sensibilité (zone endormie / cartonnée) dans le/les pied(s) ou jambe(s) ?",
                         "Avez-vous l'impression d'avoir perdu de la sensibilité (zone endormie / cartonnée) dans la/les main(s) ou le/les bras ?"
                       )
                       fluidRow(
                         column(6, sympt_labels[i]),
                         column(3, radioButtons(ns(sympt_ids), NULL, choices = c("Oui", "Non"), inline = TRUE, selected = character(0)))
                       )
                     }),
                     
                      tags$h4("Avez-vous constaté l'un des signes suivants ?"),
                       lapply(1:3, function(i) {
                         sympt_ids <- paste0("signes_generaux_", i)
                         sympt_labels <- c(
                           "Perte d'appétit",
                           "Amaigrissement",
                           "Fatigue importante"
                         )
                         fluidRow(
                           column(6, sympt_labels[i]),
                           column(3, radioButtons(ns(sympt_ids), NULL, choices = c("Oui", "Non"), inline = TRUE, selected = character(0)))
                         )
                       }),
                     
                     radioButtons(ns("infection_urinaire_3mois"), "Avez-vous eu une infection urinaire dans les 3 derniers mois ?", choices = c("Oui", "Non"), selected = character(0)),
                     radioButtons(ns("antibio_3mois"), "Avez-vous pris des antibiotiques, pour quelque raison que ce soit, au cours des 3 derniers mois ?", choices = c("Oui", "Non"), selected = character(0)),
                     
                     tags$h4(HTML("<u><b>Pour traiter l'épisode de douleur <i>actuelle</i></b></u>, avez-vous eu :")),
                     
                     radioButtons(ns("trait_kine"), "Kinésithérapie", choices = c("Oui", "Non"), selected = character(0)),
                     conditionalPanel(
                       condition = sprintf("input['%s'] == 'Oui'", ns("trait_kine")),
                       selectInput(ns("pgic_kine"), "Effet de la kinésithérapie (PGIC)", 
                                   choices = c("",
                                     "Pas de changement ou c’est devenu pire", 
                                     "Presque pareil, pratiquement pas d’amélioration",
                                     "Un peu mieux mais pas de changement notable",
                                     "Plutôt mieux mais le changement ne fait pas de réelle différence",
                                     "Mieux, le changement est modéré mais notable",
                                     "Mieux avec sans aucun doute une amélioration réelle qui fait la différence",
                                     "Nettement mieux, une amélioration considérable qui fait toute la différence"
                                   ), selected = character(0))
                     ),
                     
                     radioButtons(ns("trait_medoc"), "Médicaments", choices = c("Oui", "Non"), selected = character(0)),
                     conditionalPanel(
                       condition = sprintf("input['%s'] == 'Oui'", ns("trait_medoc")),
                       selectInput(ns("pgic_medoc"), "Effet des médicaments (PGIC)", 
                                   choices = c("",
                                     "Pas de changement ou c’est devenu pire", 
                                     "Presque pareil, pratiquement pas d’amélioration",
                                     "Un peu mieux mais pas de changement notable",
                                     "Plutôt mieux mais le changement ne fait pas de réelle différence",
                                     "Mieux, le changement est modéré mais notable",
                                     "Mieux avec sans aucun doute une amélioration réelle qui fait la différence",
                                     "Nettement mieux, une amélioration considérable qui fait toute la différence"
                                   ), selected = "")
                     ),
                     
                     radioButtons(ns("trait_infiltration"), "Infiltrations", choices = c("Oui", "Non"), selected = character(0)),
                     conditionalPanel(
                       condition = sprintf("input['%s'] == 'Oui'", ns("trait_infiltration")),
                       selectInput(ns("pgic_infiltration"), "Effet des infiltrations (PGIC)", 
                                   choices = c("",
                                     "Pas de changement ou c’est devenu pire", 
                                     "Presque pareil, pratiquement pas d’amélioration",
                                     "Un peu mieux mais pas de changement notable",
                                     "Plutôt mieux mais le changement ne fait pas de réelle différence",
                                     "Mieux, le changement est modéré mais notable",
                                     "Mieux avec sans aucun doute une amélioration réelle qui fait la différence",
                                     "Nettement mieux, une amélioration considérable qui fait toute la différence"
                                   ), selected = "")
                     ),
                     
                     radioButtons(ns("trait_osteo"), "Ostéopathie / Thérapie manuelle", choices = c("Oui", "Non"), selected = character(0)),
                     conditionalPanel(
                       condition = sprintf("input['%s'] == 'Oui'", ns("trait_osteo")),
                       selectInput(ns("pgic_osteo"), "Effet de l'ostéopathie (PGIC)", 
                                   choices = c(
                                     "Pas de changement ou c’est devenu pire", 
                                     "Presque pareil, pratiquement pas d’amélioration",
                                     "Un peu mieux mais pas de changement notable",
                                     "Plutôt mieux mais le changement ne fait pas de réelle différence",
                                     "Mieux, le changement est modéré mais notable",
                                     "Mieux avec sans aucun doute une amélioration réelle qui fait la différence",
                                     "Nettement mieux, une amélioration considérable qui fait toute la différence"
                                   ), selected = "")
                     ),
                     
                     tags$h4("Localisation principale de la douleur"),
                     radioButtons(ns("zone_consulte"), "Consultez-vous pour :", 
                                  choices = c("Les lombaires", "Les cervicales", "Les 2"), 
                                  selected = character(0), 
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
                                 radioButtons(ns(paste0("odi_", i)), label = NULL, choices = questions[[i]], selected = character(0))
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
                                 radioButtons(ns(paste0("ndi_", i)), label = NULL, choices = questions[[i]], selected = character(0))
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
                                         radioButtons(ns(dim$id), label = NULL, choices = dim$choices, selected = character(0))
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
                           div(class = "eq5d-item",
                               tags$h5(nom),
                               radioButtons(ns(paste0("had_", i)), label = NULL, choices = questions[[i]], selected = character(0))
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
        # ⚙️ Étape 1 - Champs de base obligatoires
        required_fields <- c("nom", "prenom", "naissance", "taille", "poids", "sexe", "statut")

        # Champs conditionnels selon le statut
        if (input$statut %in% c("Actif", "Retraité")) {
          required_fields <- c(required_fields, "metier", "pcs")
        }

        # Vérifie les champs requis
        missing_fields <- sapply(required_fields, function(field) {
          is.null(input[[field]]) || input[[field]] == "" || is.na(input[[field]])
        })

        if (any(missing_fields)) stop("❌ Veuillez remplir tous les champs requis et les dates au format jj/mm/aaaa")

        # Vérification format date de naissance
        if (!grepl("^\\d{2}/\\d{2}/\\d{4}$", input$naissance)) {
          stop("❌ Veuillez remplir tous les champs requis et les dates au format jj/mm/aaaa")
        }

        # 🧬 Génère le hash ID
        hash_id <- generate_hash(input$nom, input$prenom, input$naissance)

        # 📦 Collecte des données
        values <- reactiveValuesToList(input)
        values$hash_id <- hash_id
        values$date_sauvegarde <- format(Sys.Date(), "%d/%m/%Y")

        values_fixed <- lapply(values, function(x) {
      if (length(x) == 0) return(NA)
      if (length(x) > 1) return(paste(x, collapse = ", "))
      return(x)
    })
    
    # 🏷️ Renommage intelligent des colonnes pour l'analyse
    rename_map <- c(
      atcd_rhumato_1 = "rhumato_spa",
      atcd_rhumato_2 = "rhumato_pr",
      atcd_rhumato_3 = "rhumato_crohn",
      atcd_rhumato_4 = "rhumato_rch",
      atcd_rhumato_5 = "rhumato_uveite",
      atcd_rhumato_6 = "rhumato_psoriasis",
      goutte = "rhumato_goutte",

      atcd_cardio_1 = "cardio_chol",
      atcd_cardio_2 = "cardio_hta",
      atcd_cardio_3 = "cardio_diabete",
      atcd_cardio_4 = "cardio_cardiaque",
      atcd_cardio_5 = "cardio_arterite",

      sante_actuelle_1 = "neuro_force_mi",
      sante_actuelle_2 = "neuro_force_ms",
      sante_actuelle_3 = "neuro_sens_mi",
      sante_actuelle_4 = "neuro_sens_ms",

      signes_generaux_1 = "general_appetit",
      signes_generaux_2 = "general_perte_poids",
      signes_generaux_3 = "general_fatigue"
    )

    # Ajout dynamique des mappings pour ODI, NDI, HAD, BIPQ
    for (i in 1:10) rename_map[paste0("odi_", i)] <- paste0("odi_Q", i)
    for (i in 1:10) rename_map[paste0("ndi_", i)] <- paste0("ndi_Q", i)
    for (i in 1:14) rename_map[paste0("had_", i)] <- paste0("had_Q", i)
    for (i in 1:8) rename_map[paste0("bipq_", i)] <- paste0("bipq_Q", i)

    rename_map <- c(rename_map,
      eq_mobilite = "eq5d_mob",
      eq_autonomie = "eq5d_autonomie",
      eq_activites = "eq5d_act",
      eq_douleur = "eq5d_douleur",
      eq_anxiete = "eq5d_anxiete",
      eq_vas = "eq5d_vas"
    )

    names(values_fixed) <- ifelse(names(values_fixed) %in% names(rename_map),
                                  rename_map[names(values_fixed)],
                                  names(values_fixed))

    row <- as.data.frame(values_fixed, stringsAsFactors = FALSE)

        # 📁 Sauvegarde dans data/patients.csv
        path <- "data/patients.csv"
        if (!dir.exists("data")) dir.create("data")

        if (file.exists(path)) {
          write.table(row, path, sep = ";", row.names = FALSE, col.names = FALSE, append = TRUE)
        } else {
          write.table(row, path, sep = ";", row.names = FALSE, col.names = TRUE, append = FALSE)
        }

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
