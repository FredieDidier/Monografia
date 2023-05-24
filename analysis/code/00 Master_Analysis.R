# Script para rodar os codigos que geram as bases prontas para produzir resultados
# Estrutura:
#
# 1. Replicar o trabalho do Journal of Public Economics "Inequality of the coronavirus shock".
# 2. Primeira analise: perda de emprego #grafico
# 2.1. Porcentagem de perda de emprego por setor/categoria de emprego e educacao. #graficos
# 3. Regressao para determinantes de perda de emprego e renda.
# 4. Regressao para determinantes de perda de emprego e renda: caracteristicas individuais.
# 5. Efeitos da industria para perder emprego (efeitos fixos) - grafico da regressao
# 6. Efeitos da ocupacao para perder emprego (efeitos fixos) - grafico da regressao



###########
#  Setup  #
###########

# Limpeza do environnment

rm(list = ls())

# Pacotes utilizados

library(tidyverse)
library(showtext)
library(modelsummary)
library(estimatr)
library(rcartocolor)
library(Hmisc)
library(data.table)
library(RColorBrewer)
library(haven)
library(here)

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
  wd <- "C:/Users/DELL/Documents/GitHub/Monografia-Fredie"
} else if (user == "f.cavalcanti") {
  wd <- "C:/Users/DELL/Documents/GitHub/Monografia-Fredie"
} else {
  stop("Invalid user")
}

setwd(wd)

###########################################
#                                         #
# 1) 
#                                         #
###########################################


# ##############################################
# #                                            #
# # 2) Perda de Emprego
# #                                            #
# ##############################################

# Educaçao

source("analysis/code/01_graph_job_loss_educ.R")

# Setores

source("analysis/code/02_graph_job_loss_sectors.R")

# Ocupacao

source("analysis/code/03_graph_job_loss_occupation.R")

# Work Arrangement

source("analysis/code/04_graph_job_loss_work_arrangement.R")

# Categoria de Emprego

source("analysis/code/05_graph_job_loss_work_category.R")




###########################################
#                                         #
# 3)  
#                                         #
###########################################


####################################################
#                                                  #
# 4) 
#                                                  #
####################################################



#############################################################################
#                                                                           #
# 5)                       
#                                                                           #
#############################################################################



#####################################################
#                                                   #                       
# 6)                                        
#                                                   #                        
#####################################################


