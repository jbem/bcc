server <- function(input, output) {

  # ---- Données filtrées -------------------------------------------------------
  donnees <- reactive({
    req(input$periode)
    
    d <- dataisf |>filter(
        make_date(year = annee, month = mois) >= input$periode[1], 
        make_date(year = annee, month = mois) <= input$periode[2])

    if (length(input$pays))      d <- filter(d, pays %in% input$pays)
    if (length(input$categorie)) d <- filter(d, categorie %in% input$categorie)
    if (length(input$banques))   d <- filter(d, banque %in% input$banques)
    d
  })
 
  indic_series <- reactive({
    s <- calculer(donnees(), input$niveau, "annee")
    validate(need(nrow(s) > 0, "Aucune donnée pour cette sélection."))
    s
  })
 
  # ---- Onglet 1 : tableau -----------------------------------------------------
  output$choix_date_tab <- renderUI({
    p <- sort(unique(indic_series()$periode), decreasing = TRUE)
    selectInput("date_tab", "Période", choices = as.character(p),
                selected = as.character(p[1]))
  })
 
  tableau <- reactive({
    req(input$date_tab)
    indic_series() |>
      filter(as.character(periode) == input$date_tab) |>
      select(entite, indicateur, valeur) |>
      pivot_wider(names_from = isf, values_from = valeur) |>
      rename(`Entité` = entite) |>
      arrange(`Entité`)
  })
 
  output$tableau <- renderDT({
    t <- tableau()
    cols <- setdiff(names(t), "Entité")
    datatable(t, rownames = FALSE,
              options = list(pageLength = 15, scrollX = TRUE),
              caption = "Valeurs en %") |>
      formatRound(cols, 2)
  })
 
  output$telecharger <- downloadHandler(
    filename = function() paste0("indicateurs_", input$date_tab, ".csv"),
    content  = function(file) write.csv(tableau(), file, row.names = FALSE,
                                        fileEncoding = "UTF-8")
  )
 
  # ---- Onglet 1 : graphique ---------------------------------------------------
  output$graphique <- renderPlot({
    ind   <- input$indic
    s     <- indic_series() |> filter(indicateur == ind, !is.na(valeur))
    seuil <- tabisf$seuil[tabisf$nom == ind]
    validate(need(nrow(s) > 0, "Aucune donnée pour cet indicateur."))
 
    if (input$type_graph == "evol") {
      p <- ggplot(s, aes(periode, valeur, colour = entite, group = entite)) +
        geom_line() + geom_point(size = 1) +
        labs(x = NULL, y = paste0(ind, " (%)"), colour = NULL)
      if (n_distinct(s$entite) > 15) p <- p + theme(legend.position = "none")
    } else {
      req(input$date_tab)
      sd <- s |> filter(as.character(periode) == input$date_tab)
      validate(need(nrow(sd) > 0, "Aucune donnée à cette date."))
      p <- ggplot(sd, aes(reorder(entite, valeur), valeur)) +
        geom_col(fill = "steelblue") +
        coord_flip() +
        labs(x = NULL, y = paste0(ind, " (%)"))
    }
 
    if (!is.na(seuil)) {
      p <- p + geom_hline(yintercept = seuil, linetype = "dashed", colour = "red")
    }
    p + theme_minimal(base_size = 14) + theme(legend.position = "bottom")
  })
 
  # ---- Onglet 2 : choix de la date pour l'ACP ----------------------------------
  output$choix_date_acp <- renderUI({
    p <- sort(unique(donnees()$date), decreasing = TRUE)
    selectInput("date_acp", "Date de l'ACP", choices = as.character(p),
                selected = as.character(p[1]))
  })
 
  # ---- ACP --------------------------------------------------------------------
  acp <- reactive({
    req(input$date_acp)
    validate(need(length(input$ind_mv) >= 2, "Choisissez au moins 2 indicateurs."))
 
    w <- calculer(donnees(), input$niveau_mv, "date") |>
      filter(as.character(periode) == input$date_acp,
             indicateur %in% input$ind_mv) |>
      select(entite, indicateur, valeur) |>
      pivot_wider(names_from = indicateur, values_from = valeur)
 
    sel <- intersect(input$ind_mv, names(w))
    validate(need(length(sel) >= 2, "Pas assez d'indicateurs disponibles."))
    w <- w |> filter(if_all(all_of(sel), ~ !is.na(.x)))
    validate(need(nrow(w) >= 3,
                  "Moins de 3 individus complets : élargissez la sélection."))
 
    mat <- as.data.frame(w[, sel])
    rownames(mat) <- w$entite
    validate(need(all(apply(mat, 2, sd) > 0),
                  "Un indicateur est constant : retirez-le de l'analyse."))
    prcomp(mat, scale. = TRUE)
  })
 
  output$acp_ind <- renderPlot({
    res <- acp()
    ve  <- 100 * res$sdev^2 / sum(res$sdev^2)
    sc  <- as.data.frame(res$x[, 1:2])
    names(sc) <- c("x", "y")
    sc$entite <- rownames(sc)
    ggplot(sc, aes(x, y, label = entite)) +
      geom_hline(yintercept = 0, colour = "grey60") +
      geom_vline(xintercept = 0, colour = "grey60") +
      geom_point(colour = "steelblue", size = 2) +
      geom_text(vjust = -0.8, size = 3.5, check_overlap = TRUE) +
      labs(x = sprintf("Axe 1 (%.1f %%)", ve[1]),
           y = sprintf("Axe 2 (%.1f %%)", ve[2])) +
      theme_minimal(base_size = 14)
  })
 
  output$acp_var <- renderPlot({
    res <- acp()
    cor_v <- res$rotation[, 1:2, drop = FALSE] %*% diag(res$sdev[1:2])
    v <- data.frame(x = cor_v[, 1], y = cor_v[, 2], nom = rownames(cor_v))
    t <- seq(0, 2 * pi, length.out = 200)
    cercle <- data.frame(x = cos(t), y = sin(t))
    ggplot() +
      geom_path(data = cercle, aes(x, y), colour = "grey60") +
      geom_hline(yintercept = 0, colour = "grey80") +
      geom_vline(xintercept = 0, colour = "grey80") +
      geom_segment(data = v, aes(x = 0, y = 0, xend = x, yend = y),
                   arrow = arrow(length = unit(0.2, "cm")), colour = "firebrick") +
      geom_text(data = v, aes(x, y, label = nom), vjust = -0.6, size = 3.5) +
      coord_fixed(xlim = c(-1.2, 1.2), ylim = c(-1.2, 1.2)) +
      labs(x = "Axe 1", y = "Axe 2") +
      theme_minimal(base_size = 14)
  })
 
  output$acp_eig <- renderPlot({
    res <- acp()
    ve  <- data.frame(axe = factor(seq_along(res$sdev)),
                      pct = 100 * res$sdev^2 / sum(res$sdev^2))
    ggplot(ve, aes(axe, pct)) +
      geom_col(fill = "steelblue") +
      geom_text(aes(label = sprintf("%.1f %%", pct)), vjust = -0.4) +
      labs(x = "Axe", y = "Inertie expliquée (%)") +
      theme_minimal(base_size = 14)
  })
 
  # ---- STATIS (une table par année, mêmes individus et mêmes indicateurs) -----
  statis_res <- reactive({
    validate(need(length(input$ind_mv) >= 2, "Choisissez au moins 2 indicateurs."))
 
    w <- calculer(donnees(), input$niveau_mv, "annee") |>
      filter(indicateur %in% input$ind_mv) |>
      select(entite, periode, indicateur, valeur) |>
      pivot_wider(names_from = indicateur, values_from = valeur)
 
    sel <- intersect(input$ind_mv, names(w))
    validate(need(length(sel) >= 2, "Pas assez d'indicateurs disponibles."))
    w <- w |> filter(if_all(all_of(sel), ~ !is.na(.x)))
 
    annees <- sort(unique(w$periode))
    validate(need(length(annees) >= 2, "STATIS demande au moins 2 années."))
 
    # On ne garde que les individus présents (et complets) chaque année
    complets <- w |> count(entite) |> filter(n == length(annees)) |> pull(entite)
    validate(need(length(complets) >= 3,
                  "Moins de 3 individus présents sur toutes les années."))
 
    tabs <- lapply(annees, function(a) {
      x <- w |> filter(periode == a, entite %in% complets) |> arrange(entite)
      m <- as.data.frame(x[, sel])
      rownames(m) <- x$entite
      m
    })
    validate(need(all(vapply(tabs, function(m) all(apply(m, 2, sd) > 0), logical(1))),
                  "Un indicateur est constant sur une année : retirez-le."))
 
    dudis <- lapply(tabs, dudi.pca, scale = TRUE, scannf = FALSE, nf = 2)
    kt    <- ktab.list.dudi(dudis, tabnames = as.character(annees))
    list(st = statis(kt, scannf = FALSE, nf = 2), annees = annees)
  })
 
  output$st_comp <- renderPlot({
    st <- statis_res()$st
    cl <- as.data.frame(st$C.li[, 1:2])
    names(cl) <- c("x", "y")
    cl$entite <- rownames(st$C.li)
    ggplot(cl, aes(x, y, label = entite)) +
      geom_hline(yintercept = 0, colour = "grey60") +
      geom_vline(xintercept = 0, colour = "grey60") +
      geom_point(colour = "steelblue", size = 2) +
      geom_text(vjust = -0.8, size = 3.5, check_overlap = TRUE) +
      labs(x = "Axe 1", y = "Axe 2") +
      theme_minimal(base_size = 14)
  })
 
  output$st_poids <- renderPlot({
    r <- statis_res()
    p <- data.frame(annee = factor(r$annees), poids = as.numeric(r$st$RV.tabw))
    ggplot(p, aes(annee, poids)) +
      geom_col(fill = "steelblue") +
      labs(x = NULL, y = "Poids dans le compromis") +
      theme_minimal(base_size = 14)
  })
 
  output$st_rv <- renderPlot({
    r  <- statis_res()
    rv <- as.matrix(r$st$RV)
    dimnames(rv) <- list(r$annees, r$annees)
    d  <- as.data.frame(as.table(rv))
    names(d) <- c("a", "b", "rv")
    ggplot(d, aes(a, b, fill = rv)) +
      geom_tile() +
      geom_text(aes(label = sprintf("%.2f", rv))) +
      scale_fill_gradient(low = "white", high = "steelblue", limits = c(0, 1)) +
      labs(x = NULL, y = NULL, fill = "RV") +
      theme_minimal(base_size = 14)
  })
}

