

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

################## Gráfico Servidores Públicos com Carteira em 2019

ggarrange(n_grafico, n_grafico_2, n_grafico_3, n_grafico_4,
         common.legend = T,
         legend = "bottom")


############ Gráfico Servidores Públicos sem Carteira em 2019
ggarrange(nov_grafico, nov_grafico_2, nov_grafico_3, nov_grafico_4,
          common.legend = T,
          legend = "bottom")

############ Gráfico Servidores Públicos Contribuintes em 2019

ggarrange(novo_grafico, novo_grafico_2, novo_grafico_3, novo_grafico_4,
          common.legend = T,
          legend = "bottom")

############# Gráfico Servidores Públicos Não Contribuintes em 2019

ggarrange(new_grafico, new_grafico_2, new_grafico_3, new_grafico_4,
          common.legend = T,
          legend = "bottom")

################# Gráfico Empregadores Contribuintes em 2019

ggarrange(nuevo_grafico, nuevo_grafico_2, nuevo_grafico_3, nuevo_grafico_4,
          common.legend = T,
          legend = "bottom")

############### Gráfico Empregadores Não Contribuintes em 2019
ggarrange(nueva_grafico, nueva_grafico_2, nueva_grafico_3, nueva_grafico_4,
          common.legend = T,
          legend = "bottom")

######### Gráfico Inativos em 2019

ggarrange(grafico_inat, grafico_inat2, grafico_inat3, grafico_inat4,
          common.legend = T,
          legend = "bottom")

## INFORMAÇÕES IMPORTANTES:
## 1. TRABALHADORES COM CART ASS SÃO TRAB DOMÉSTICO, SETOR PRIVADO E SETOR PÚBLICO
## 2. TRABALHADORES CONTRIBUINTES DO INSS SÃO TRAB DOMÉSTICO, SETOR PRIVADO, SETOR PÚBLICO, EMPREGADOR E CONTA PRÓPRIA
## 3. MILITARES NÃO TEM CARTEIRA E NEM CONTRIBUEM PRO INSS