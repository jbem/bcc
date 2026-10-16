suppressPackageStartupMessages({
  library(lubridate)
  library(shiny)
  library(bslib)
  library(DBI)
  library(RSQLite)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(DT)
  library(ade4)
})

sqcon<-dbConnect(SQLite(),"../dbBCC.db")
capital<-dbReadTable(sqcon,"capital")
banque<-dbReadTable(sqcon,"banque")
loansdeposits<-dbReadTable(sqcon,"loansdeposits")

tabisf <- tibble::tribble(
 ~isf, ~nom,                             ~num,         ~den,      ~seuil,
  1,   "Solvabilité (FPR/RWA)",            "fpr",        "rwa",     8,
  2,   "Levier (FPR/Bilan)",               "fpr",        "bilan",   5,
  3,   "Créances douteuses (NPL/Crédits)", "npl",        "credits", 10,
  4,   "Couverture (Provisions/NPL)",      "provisions", "npl",     50,
  5,   "Transformation (Crédits/Dépôts)",  "credits",    "depots",  NA_real_
)

dataisf<-capital|>left_join(banque)|>mutate(
    banque=paste0(pays,"-",id_banque),
    periode_valo=paste0("A",annee,"M",mois)
)|>left_join(loansdeposits)|>mutate(categ=ifelse(categorie==1,"Banque","Etablissement financier"))

PAYS<-c(sort(unique(dataisf$pays)),"Zone CEMAC")
BANQUES<-sort(unique(dataisf$banque))
PERIODES<-sort(unique(dataisf$periode_valo))
CATEGORIES<-c("Banque","Etablissement financier")

#Calcul les ISF
#Niveau categorie, pays, Zone
#pas = periode ou annee

ratios_agreges <- function(gdata) {
  vapply(seq_len(nrow(tabisf)), function(i) {

    n  <- gdata[[tabisf$num[i]]]
    d  <- gdata[[tabisf$den[i]]]
    ok <- !is.na(n) & !is.na(d)
    100 * sum(n[ok]) / sum(d[ok])
  }, numeric(1))|>tibble(isf=tabisf$nom,valeur=_)
}

calculer <- function(df, niveau, pas = c("periode", "annee")) {
  
  pas <- match.arg(pas)

  if (nrow(df) == 0) {
    return(tibble(entite = character(), periode = numeric(),
                  indicateur = character(), valeur = numeric()))
  }

  df|>
    mutate(
      entite  = switch(niveau,
                  banque    = banque,
                  pays      = pays,
                  categorie = ifelse(categorie==1,"Banque","Etablissement financier"),
                  zone      = "Zone"),

      periode = switch(pas, periode=periode,annee=annee) 
    )|>group_by(entite, periode)|>reframe(
       ratios_agreges(pick(everything()))
    )|>mutate(valeur = ifelse(is.finite(valeur), valeur, NA_real_))
}

