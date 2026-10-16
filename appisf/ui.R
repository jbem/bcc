ui <- page_sidebar(
  title = "Solidité financière des banques",
  theme = bs_theme(version = 5),
 
  sidebar = sidebar(
    width = 320,
    radioButtons("niveau", "Niveau d'analyse",
                 choices = c("Banque" = "banque", "Pays" = "pays",
                             "Catégorie" = "categorie",
                             "Zone CEMAC" = "zone"),
                 selected = "pays"),
    selectInput("pays", "Pays (vide = tous)", choices = PAYS, multiple = TRUE),
    selectInput("categorie", "Catégorie (vide = toutes)",
                choices = CATEGORIES, multiple = TRUE),
    selectizeInput("banques", "Banques (vide = toutes)",
                   choices = BANQUES, multiple = TRUE),
    dateRangeInput("periode", "Période", start = '2016-01-01', end = 2018-12-31,
                   min = '2016-01-01', max = '2018-12-31', language = "fr",
                   format = "yyyy-mm")
  ),
 
  navset_card_tab(
    # ---------------------------------------------------------------- Onglet 1
    nav_panel(
      "Indicateurs",
      layout_columns(
        col_widths = c(12, 12),
        card(
          card_header("Tableau des indicateurs"),
          uiOutput("choix_date_tab"),
          DTOutput("tableau"),
          downloadButton("telecharger", "Télécharger (CSV)")
        ),
        card(
          card_header("Graphique"),
          layout_columns(
            col_widths = c(6, 6),
            selectInput("indic", "Indicateur", choices = tabisf$nom),
            radioButtons("type_graph", "Type de graphique",
                         choices = c("Évolution" = "evol",
                                     "Comparaison à la date du tableau" = "comp"),
                         inline = TRUE)
          ),
          plotOutput("graphique", height = "420px")
        )
      )
    ),
 
    # ---------------------------------------------------------------- Onglet 2
    nav_panel(
      "Analyse multivariée",
      layout_columns(
        col_widths = c(3, 3, 6),
        radioButtons("methode", "Méthode",
                     choices = c("ACP (une date)" = "acp",
                                 "STATIS (plusieurs années)" = "statis")),
        selectInput("niveau_mv", "Individus",
                    choices = c("Banques" = "banque", "Pays" = "pays",
                                "Catégories" = "categorie"),
                    selected = "banque"),
        checkboxGroupInput("ind_mv", "Indicateurs inclus",
                           choices = tabisf$court,
                           selected = tabisf$court)
      ),
      conditionalPanel(
        "input.methode == 'acp'",
        uiOutput("choix_date_acp"),
        layout_columns(
          col_widths = c(6, 6),
          card(card_header("Individus (axes 1-2)"), plotOutput("acp_ind")),
          card(card_header("Cercle des corrélations"), plotOutput("acp_var"))
        ),
        card(card_header("Valeurs propres (éboulis)"), plotOutput("acp_eig", height = "260px"))
      ),
      conditionalPanel(
        "input.methode == 'statis'",
        layout_columns(
          col_widths = c(6, 6),
          card(card_header("Compromis : individus (axes 1-2)"),
               plotOutput("st_comp")),
          card(card_header("Poids des années dans le compromis"),
               plotOutput("st_poids"))
        ),
        card(card_header("Matrice RV entre années (similarité des structures)"),
             plotOutput("st_rv", height = "380px"))
      )
    )
  )
)