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

if(!file.exists('stats.xlsx')){
    # pg_ctl -D /var/lib/postgres/data -l /var/lib/postgres/logfile start
    pgcon<-dbConnect(Postgres(), dbname = 'cerber', host = 'localhost', user = 'dbadmin', 
             password = '12345', port = 5432, timezone = NULL, bigint='integer')

    stats<-"select * from public.bancarisation"|>dbGetQuery(pgcon, statement = _)
    write_xlsx(stats,"stats.xlsx")
}
