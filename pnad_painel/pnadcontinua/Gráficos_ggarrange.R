

library(ggpubr)

############ Trabalhadores Conta Própria Contribuintes/Não Contribuintes INSS 2019 #######
ggarrange(graf_1, grafico1,
          common.legend = TRUE,
          legend = "bottom")

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

################## Gráfico Servidores Públicos com/sem Carteira em 2019

ggarrange(n_grafico, nov_grafico,
         common.legend = T,
         legend = "bottom")

############ Gráfico Servidores Públicos Contribuintes/Não Contribuintes em 2019

ggarrange(novo_grafico, new_grafico,
          common.legend = T,
          legend = "bottom")

################# Gráfico Empregadores Contribuintes/Não Contribuintes em 2019

ggarrange(nuevo_grafico, nueva_grafico,
          common.legend = T,
          legend = "bottom")

######### Gráfico Inativos em 2019

ggarrange(grafico_inat,
          common.legend = T,
          legend = "bottom")

######## Gráfico Salário Médio Ocupações 

ggarrange(grafico_sal_ocupacoes,
          common.legend = T,
          legend = "bottom")

###### Gráfico Salário Médio Educação

ggarrange(grafico_sal,
          common.legend = T,
          legend = "bottom")

## INFORMAÇÕES IMPORTANTES:
## 1. TRABALHADORES COM CART ASS SÃO TRAB DOMÉSTICO, SETOR PRIVADO E SETOR PÚBLICO
## 2. TRABALHADORES CONTRIBUINTES DO INSS SÃO TRAB DOMÉSTICO, SETOR PRIVADO, SETOR PÚBLICO, EMPREGADOR E CONTA PRÓPRIA
## 3. MILITARES NÃO TEM CARTEIRA E NEM CONTRIBUEM PRO INSS