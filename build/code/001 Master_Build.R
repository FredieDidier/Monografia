# Script para rodar os códigos que geram as bases prontas para produzir resultados
# Estrutura:
#
# 1. Funcoes que vao limpar os paineis
# 2. Gerar paineis por nivel educacional
# 3. Criar uma base adequada para regressao

###########
#  Setup  #
###########

# Limpeza do environnment

rm(list = ls())

# Pacotes utilizados

library(tidyverse)
library(here)
library(fs)
library(foreign)
library(haven)
library(data.table)

####################
# Folder Path
####################

user <- Sys.info()[["user"]]
print(paste("user name:", user))

if (user == "Fredie") {
  wd <- "/Users/Fredie/Documents/GitHub/Monografia"
} else if (user == "Francisco") {
  wd <- "C:/Users/Francisco/Dropbox/Research/Monografia-Fredie"
} else if (user == "DELL") {
  wd <- "D:/OneDrive/Documentos/GitHub/Monografia-Fredie"
} else if (user == "f.cavalcanti") {
  wd <- "C:/Users/DELL/Documents/GitHub/Monografia-Fredie"
} else {
  stop("Invalid user")
}

setwd(wd)

##################################
#                                #
#   1) Gerar paineis limpos      #
#                                #
##################################

source("./build/code/01_cleaning_paineis.R")
source("./build/code/02_aggregating_sector_codes.R")
source("./build/code/03_aggregating_occupation_codes.R")

#############################################
#                                           #
#   2) Loop que gera paineis limpos         #
#       Cria paineis por nivel educacional  #
#############################################

source("./build/code/04_panel_by_education_level.R")

##################################
#                                #
#   3) Base para regressao       #
#                                #
##################################

source("./build/code/05_main_data.R")



