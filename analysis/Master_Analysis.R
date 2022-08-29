# Script para rodar os códigos que geram as bases prontas para produzir resultados
# Estrutura:
#
# 1. Gerar matriz de transicao 2019 Geral
# 2. Gerar matriz de transicao 2019 por nivel educ
# 3. Gerar matriz de transicao 2012 - 2021 por educacao
# 4. Gerar graficos sobre elementos da matriz 2012-2021
# 5. Gerar graficos de mercado de trabalho/nivel educ de 2019 a 2022        
# 6. Gerar informacoes sobre tabela descritiva


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

###########################################
#                                         #
# 1) Gerar matriz de transicao 2019 Geral # 
#                                         #
###########################################

############################
#  1.1) Lendo painel     #
############################

painel_2019 = readr::read_rds("./input/painel_2019.rds")

###################################
#  1.2) Gerando matriz 2019 Geral #
###################################

source("./analysis/_transition_matrix_function.R")

matriz1 = cria_matriz_transicao(painel_2019, "2019_1", "2019_2", 10, prop = FALSE)
matriz2 = cria_matriz_transicao(painel_2019, "2019_2", "2019_3", 10, prop = FALSE)
matriz3 = cria_matriz_transicao(painel_2019, "2019_3", "2019_4", 10, prop = FALSE)
matriz_soma = matriz1 + matriz2 + matriz3

matriz_final = scale(matriz_soma, center = F, scale = colSums(matriz_soma))

saveRDS(matriz_final, "./input/transition_matrix_2019.rds")

##############################################
#                                            #
# 2) Gerar matriz de transicao 2019 por educ #  
#                                            #
##############################################

source("./analysis/_transition_matrices_by_educ.R")


###########################################
#                                         #
# 3) Gerar matriz de transicao 2012-2021  #
#                                         #
###########################################

source("./analysis/_transition_matrices_by_educ.R")


####################################################
#                                                  #
# 4) Gerar graficos de transicao e outros 2012-2021#
#                                                  #
####################################################

source("./analysis/_graphs.R")

ggsave("transicao_formal_nao_empregado.png", path = "./output",
       width = 11.19, height = 6.64)

ggsave("transicao_informal_nao_empregado.png", path = "./output",
       width = 11.19, height = 6.64)

ggsave("transicao_ocupado_nao_empregado.png", path = "./output",
       width = 11.19, height = 6.64)

ggsave("transicao_informal_formal.png", path = "./output",
       width = 11.19, height = 6.64)

ggsave("transicao_formal_informal.png", path = "./output",
       width = 11.19, height = 6.64)

ggsave("ocupados_priv_and_pub_2012-2021.png", path = "./output",
       width = 11.19, height = 6.64)

ggsave("ocupados_conta_propria_2012-2021.png", path = "./output",
       width = 11.19, height = 6.64)


#############################################################################
#                                                                           #
# 5) Gerar graficos de mercado de trabalho/nivel educ de 2019 a 2022        #                
#                                                                           #
#############################################################################

source("./analysis/_graph_2019_2020_2021_2022_prop.R")

ggsave("prop_inativos_desempregados_2019_2022.png", path = "./output",
       width = 13.66, height = 7.05)

ggsave("prop_formal_informal_priv_2019_2022.png", path = "./output",
       width = 13.66, height = 7.05)

ggsave("prop_formal_informal_conta_propria_2019_2022.png", path = "./output",
       width = 13.66, height = 7.05)

ggsave("prop_formal_informal_empreg_2019_2022.png", path = "./output",
       width = 13.66, height = 7.05)

ggsave("prop_formal_informal_pub_2019_2022.png", path = "./output",
       width = 13.66, height = 7.05)

ggsave("wage_formal_informal_priv_2019_2022.png", path = "./output",
       width = 13.66, height = 7.05)

ggsave("wage_formal_informal_self_employed_2019_2022.png", path = "./output",
       width = 13.66, height = 7.05)

ggsave("wage_formal_informal_empreg_2019_2022.png", path = "./output",
       width = 13.66, height = 7.05)

ggsave("wage_formal_informal_pub_2019_2022.png", path = "./output",
       width = 13.66, height = 7.05)

################################################
#                                              #
# 6) Gerar informacoes sobre tabela descritiva # 
#                                              #
################################################

source("./analysis/_descriptive_table.R")



