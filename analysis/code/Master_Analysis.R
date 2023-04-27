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

# Categoria de Emprego

source("analysis/tmp/_graph_job_loss_work_category.R")

# Educação

source("analysis/tmp/_graph_job_loss_educ.R")

# Work Arrangement

source("analysis/tmp/_graph_job_loss_work_arrangement.R")


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


