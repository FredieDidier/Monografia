

library(ggpubr)

############ Trabalhadores Conta Própria Contribuintes INSS 2019 #######
ggarrange(graf_1, graf_2, graf_3, graf_4,
          common.legend = TRUE,
          legend = "bottom")

###############################
#### Trabalhadores Conta Própria que não contribuem para o INSS 2019 #####
ggarrange(grafico1, grafico2, grafico3, grafico4, 
          common.legend = T, legend = "bottom")

########################################################
#### Gráfico Trabalhadores Formais em 2019 ######
ggarrange(graf.1, graf.2,
          common.legend = T,
          legend = "bottom")
########################################################################
###### Trabalhadores Informais em 2019 ######
ggarrange(inf1, inf2,
          common.legend = T,
          legend = "bottom")

############### Desempregados em 2019

ggarrange(desemp)
