
library(tibble)
library(knitr)
library(fontawesome)

widgets <- tibble::tribble(
  ~fonction,            ~icone,               ~description,
  "actionButton",       "hand-pointer",       "Bouton d'action",
  "checkboxGroupInput", "list-check",         "Un groupe de cases à cocher",
  "checkboxInput",      "square-check",       "Une case à cocher unique",
  "dateInput",          "calendar-days",      "Un calendrier pour choisir une date",
  "dateRangeInput",     "calendar-week",      "Une paire de calendriers pour choisir une période",
  "fileInput",          "file-arrow-up",      "Un contrôle de téléversement de fichier",
  "helpText",           "circle-question",    "Un texte d'aide à ajouter à un formulaire",
  "numericInput",       "hashtag",            "Un champ de saisie de nombres",
  "radioButtons",       "circle-dot",         "Un ensemble de boutons radio",
  "selectInput",        "square-caret-down",  "Une liste déroulante de choix",
  "sliderInput",        "sliders",            "Un curseur",
  "submitButton",       "paper-plane",        "Un bouton de validation",
  "textInput",          "i-cursor",           "Un champ de saisie de texte"
)

widgets <- widgets |>
  mutate(
    icone = vapply(
      icone,
      \(nom) as.character(fa(nom, fill = "steelblue", height = "1.4em")),
      character(1)
    ),
    fonction = sprintf("`%s`", fonction)
  )

kable(
  widgets[, c("icone", "fonction", "description")],
  col.names = c("", "Fonction", "Widget"),
  align = c("c", "l", "l"),
  escape = FALSE
)
