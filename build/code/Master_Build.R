# Script para rodar os códigos que geram as bases prontas para produzir resultados
# Estrutura:
#
# 1. Gerar paineis limpos
# 2. Gerar painel de 2019
# 3. Criar uma base adequada para regressao
# 4. 4) Base para grafico de proporcao de ocupados por educ 2012-2022

###########
#  Setup  #
###########

# Limpeza do environnment

rm(list = ls())

# Pacotes utilizados

library(tidyverse)
library(here)

####################
# Folder Path
####################

user <- Sys.info()[["user"]]
print(paste("user name:", user))

if (user == "Fredie") {
  wd <- "C:/GitHub/Monografia"
} else if (user == "Francisco") {
  wd <- "C:/Users/Francisco/Dropbox/Research/Monografia-Fredie"
} else if (user == "DELL") {
  wd <- "C:/Users/DELL/Documents/GitHub/Monografia-Fredie"
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

source("./build/code/_cleaning_paineis.R")
source("./build/code/_aggregating_sector_codes.R")

#############################################
#                                           #
#   2) Loop que gera paineis limpos         #
#       Cria paineis por nivel educacional  #
#############################################

source("./build/code/_panel_by_education_level.R")



##########################################
#                                        #
#   3) Gerar painel de 2019              #     
#                                        #
#########################################

#list_trimestres = c("2019_1", "2019_3")

#df = map(list_trimestres,
#        
#         function(trim){
#           df <- haven::read_dta(paste0("build/output/painel_", trim, ".dta")) %>%
#             clean_painel() %>%
#             aggregate_sectors()
#           
#           
#         }
#) %>% bind_rows()
#
#df %>%
#  readr::write_rds(paste0("build/output/painel_2019.rds"))

##################################
#                                #
#   3) Base para regressao       #
#                                #
##################################

source("./build/code/_regression.R")


#####################################################################
#                                                          
#   4) Base para grafico de proporcao de ocupados por educ 2012-2022    
#                                                         
####################################################################

source("./build/_cleaning_trimesters.R")
