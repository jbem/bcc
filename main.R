#---------PACKAGES ---------
library(tidyverse)
library(openxlsx2)
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

#-----RETICULATE --------
use_virtualenv("~/.radian")
source_python("imports.py")


