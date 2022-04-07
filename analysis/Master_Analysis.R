# Script para rodar os códigos que geram as bases prontas para produzir resultados
# Estrutura:
#
# 1. Gerar matriz de transicao 2019
# 2. Gerar matriz de transicao 2012 - 2021 por educacao
# 3. Gerar gráficos sobre elementos da matriz


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

########################################
#                                      #
# 1) Gerar matriz de transicao 2019    #
#                                      #
########################################

############################
#  1.1) Lendo paineis      #
############################

list_trimestres <- c("2019_1_1", "2019_1_2", "2019_1_3", "2019_1_4",
                     "2019_2_1", "2019_2_2", "2019_2_3", "2019_2_4",
                     "2019_3_1", "2019_3_2", "2019_3_3", "2019_3_4")

paineis = map(list_trimestres,
     
     function(trim){
      
       
       df = readr::read_rds(paste0("input/painel_",
                                 trim,
                                 ".rds")
         )   
     }
)

names(paineis) = list_trimestres

paineis = list(
  "2019_1" = bind_rows(paineis["2019_1_1"],
                       paineis["2019_1_2"],
                       paineis["2019_1_3"],
                       paineis["2019_1_4"]),
  
  "2019_2" = bind_rows(paineis["2019_2_1"],
                       paineis["2019_2_2"],
                       paineis["2019_2_3"],
                       paineis["2019_2_4"]),
  
  "2019_3" = bind_rows(paineis["2019_3_1"],
                       paineis["2019_3_2"],
                       paineis["2019_3_3"],
                       paineis["2019_3_4"])
  
)

##############################
#  1.2) Gerando matriz 2019  #
#############################

source("./analysis/_transition_matrix_function.R")

pesos = c(
  paineis["2019_1"] %>%
    filter(year_quarter == "2019_1") %>%
    {.$weights} %>%
    sum(),
  
  paineis["2019_2"] %>%
    filter(year_quarter == "2019_2") %>%
    {.$weights} %>%
    sum(),
  
  paineis["2019_3"] %>%
    filter(year_quarter == "2019_3") %>%
    {.$weights} %>%
    sum()
)

matriz1 = cria_matriz_transicao(paineis["2019_1"], "2019_1", "2019_2")
matriz2 = cria_matriz_transicao(paineis["2019_2"], "2019_2", "2019_3")
matriz3 = cria_matriz_transicao(paineis["2019_3"], "2019_3", "2019_4")
matriz_final = (matriz1*pesos[1] + matriz2*pesos[2]
                + matriz3 * pesos[3])/(sum(pesos))


###########################################
#                                         #
# 2) Gerar matriz de transicao 2012-2021  #
#                                         #
###########################################

source("./analysis/_transition_matrices_by_educ.R")