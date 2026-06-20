#---------PACKAGES ---------
library(tidyverse)
library(openxlsx2)
library(rmarkdown)
library(DBI)
library(ROracle)
library(RPostgres)
library(RSQLite)
library(reticulate)
library(kableExtra)
library(scales)
library(patchwork) 
library(vars)
library(broom)
library(reactable)
library(quarto)
library(magrittr)
library(RPostgres)

#-----RETICULATE --------
use_virtualenv("~/.radian")
source_python("imports.py")

Sys.setlocale("LC_CTYPE","fr_FR.UTF-8")
Sys.setlocale("LC_ALL","fr_FR.utf8")

if(!file.exists('banque.xlsx')){
    # pg_ctl -D /var/lib/postgres/data -l /var/lib/postgres/logfile start
    pgcon<-dbConnect(Postgres(), dbname = 'cerber', host = 'localhost', user = 'dbadmin', 
             password = '12345', port = 5432, timezone = NULL, bigint='integer')

    capital<-"select cast(id_banque % 10000 as BIGINT) as banque, cast((id_banque -id_banque % 10000)/10000 as BIGINT) as id_pays, 
    cast(extract(YEAR from date_valo) AS BIGINT) as annee, 
    cast(extract(MONTH from date_valo) AS BIGINT) as mois, trim(id_poste) as poste, valeur 
    from public.analyse
    where id_poste in ('AF088','AF073','X19','X26') and extract(YEAR from date_valo) in (2016,2017,2018)"|>
    dbGetQuery(pgcon,statement=_)|>pivot_wider(names_from=poste,values_from=valeur)|>
    rename(bilan=AF088,fpc=AF073,fpr=X19,rwa=X26)
    write_xlsx(capital,"capital.xlsx")

    
    loansdeposits<-"select id_banque % 10000 as banque, (id_banque - id_banque % 10000)/10000 as id_pays, extract(YEAR from date_valo) as annee, 
    extract(MONTH from date_valo) as mois, trim(id_poste) as poste, valeur 
    from public.analyse
    where id_poste in ('AF043','AF051','AF057','AF029') and extract(YEAR from date_valo) in (2016,2017,2018)"|>
    dbGetQuery(pgcon,statement=_)|>pivot_wider(names_from=poste,values_from=valeur)|>
    rename(credits=AF043, provisions=AF051,npl=AF057,depots=AF029)
    write_xlsx(loansdeposits,"loansdeposits.xlsx")


    liquidity<-"select id_banque % 10000 as banque, (id_banque -id_banque % 10000)/10000 as id_pays, extract(YEAR from date_valo) as annee, 
    extract(MONTH from date_valo) as mois, trim(id_poste) as poste, valeur 
    from public.analyse 
    where id_poste in ('AF028','AF053','AF071','AF084') and extract(YEAR from date_valo) in (2016,2017,2018)"|>
    dbGetQuery(pgcon,statement=_)|>pivot_wider(names_from=poste,values_from=valeur)|>
    rename(pnl1=AF028, pnl2=AF053,pnl3=AF071,pnl4=AF084)
    write_xlsx(liquidity,"liquidity.xlsx")


    bks<-"select id_banque % 10000 as banque, (id_banque - id_banque % 10000)/10000 as id_pays, categorie, trim(pays) as pays
    from public.banque"|>dbGetQuery(pgcon, statement = _)
    write_xlsx(bks,"banque.xlsx")
}



