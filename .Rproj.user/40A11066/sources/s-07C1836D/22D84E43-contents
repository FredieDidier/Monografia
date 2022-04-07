# Script para rodar os códigos que geram as bases prontas para produzir resultados
# Estrutura:
#
# 1. Gerar paineis limpos
# 2. Criar uma base adequada para regressao

###########
#  Setup  #
###########

# Limpeza do environnment

rm(list = ls())

# Pacotes utilizados

library(tidyverse)

##################################
#                                #
#   1) Gerar paineis limpos      #
#                                #
##################################

source("./build/_cleaning_paineis")

##################################
# Loop que gera paineis limpos   #                        
##################################
list_trimestres <- c("2012_1", "2012_2", "2012_3", "2012_4",
                     "2013_1", "2013_2", "2013_3", "2013_4",
                     "2014_1", "2014_2", "2014_3", "2014_4",
                     "2015_1", "2015_2", "2015_3", "2015_4",
                     "2016_1", "2016_2", "2016_3", "2016_4",
                     "2017_1", "2017_2", "2017_3", "2017_4",
                     "2018_1", "2018_2", "2018_3", "2018_4",
                     "2019_1", "2019_2", "2019_3", "2019_4",
                     "2020_1", "2020_2", "2020_3", "2020_4",
                     "2021_1", "2021_2", "2021_3")

list_trimestres <- rep(list_trimestres, 4)

list_educ <- c(
  rep(1, 39),
  rep(2, 39),
  rep(3, 39),
  rep(4, 39)
)

map2(list_trimestres, list_educ,
     
     function(trim, educ_level){
       df <- haven::read_dta(paste0("data-raw/Trimestres/painel_", trim, ".dta")) %>%
         clean_painel()
       
       
       df <- df %>%
         filter(educ == educ_level) %>%
         group_by(id_code) %>%
         mutate(new_id = n()) %>%
         filter(new_id == 2)
       
       df %>%
         readr::write_rds(paste0("input/painel_",
                                 trim,
                                 "_",
                                 educ_level,
                                 ".rds")
         )   
     }
)

##################################
#                                #
#   2) Base para regressao       #
#                                #
##################################

##################################
# 2.1) Empilhando trimestres     #                             
##################################

list_trimestres <- c("2012_1", "2012_2", "2012_3", "2012_4",
                     "2013_1", "2013_2", "2013_3", "2013_4",
                     "2014_1", "2014_2", "2014_3", "2014_4",
                     "2015_1", "2015_2", "2015_3", "2015_4",
                     "2016_1", "2016_2", "2016_3", "2016_4",
                     "2017_1", "2017_2", "2017_3", "2017_4",
                     "2018_1", "2018_2", "2018_3", "2018_4",
                     "2019_1", "2019_2", "2019_3", "2019_4",
                     "2020_1", "2020_2", "2020_3", "2020_4",
                     "2021_1", "2021_2", "2021_3")

df = map(list_trimestres,
     
     function(trim){
       df <- haven::read_dta(paste0("data-raw/Trimestres/painel_", trim, ".dta")) %>%
         clean_painel()
       
       
       df
        
     }
) %>%
  reduce(bind_rows)

##################################
# 2.2) Agregando codigo setor    #                             
##################################

source("./build/_aggregating_sector_codes")

