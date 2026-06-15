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

#-----RETICULATE --------
use_virtualenv("~/.radian")
source_python("imports.py")


