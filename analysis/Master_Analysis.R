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
library(extrafont)
library(modelsummary)
library(estimatr)
library(rcartocolor)
library(grid)
library(gridExtra)
library(Hmisc)
library(data.table)

###########################################
#                                         #
# 1) 
#                                         #
###########################################

############################
#      
############################

# ###################################
# #  
# ###################################
# 
# source("./analysis/_transition_matrix_function.R")
# 
# matriz1 = cria_matriz_transicao(painel_2019, "2019_1", "2019_2", 10, prop = FALSE)
# matriz2 = cria_matriz_transicao(painel_2019, "2019_2", "2019_3", 10, prop = FALSE)
# matriz3 = cria_matriz_transicao(painel_2019, "2019_3", "2019_4", 10, prop = FALSE)
# matriz_soma = matriz1 + matriz2 + matriz3
# 
# matriz_final = scale(matriz_soma, center = F, scale = colSums(matriz_soma))
# 
# saveRDS(matriz_final, "./input/transition_matrix_2019.rds")

# ##############################################
# #                                            #
# # 2)   
# #                                            #
# ##############################################
# 


###########################################
#                                         #
# 3)  
#                                         #
###########################################

source("./analysis/_transition_matrices_by_educ.R")


####################################################
#                                                  #
# 4) 
#                                                  #
####################################################

source("./analysis/_graphs.R")

ggsave("transicao_formal_nao_empregado.png", path = "./output",
       width = 13.66, height = 7.05)

ggsave("transicao_informal_nao_empregado.png", path = "./output",
       width = 13.66, height = 7.05)

ggsave("transicao_ocupado_nao_empregado.png", path = "./output",
       width = 13.66, height = 7.05)

ggsave("transicao_informal_formal.png", path = "./output",
       width = 13.66, height = 7.05)

ggsave("transicao_formal_informal.png", path = "./output",
       width = 13.66, height = 7.05)

ggsave("ocupados_priv_and_pub_2012-2021.png", path = "./output",
       width = 11.19, height = 6.64)

ggsave("ocupados_conta_propria_2012-2021.png", path = "./output",
       width = 11.19, height = 6.64)


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


