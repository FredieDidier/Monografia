

library(ggpubr)

############ Trabalhadores Conta Propria Contribuintes/Noo Contribuintes INSS 2019 #######
ggarrange(graf_1, grafico1,
          common.legend = TRUE,
          legend = "bottom")

########################################################
#### Grafico Setor Privado Formal e Informal em 2019 ######
ggarrange(priv_graf, priv_inf_graf,
          common.legend = T,
          legend = "bottom")

############### Desempregados em 2019

ggarrange(desemp)

################## Grafico Servidores Publicos com/sem Carteira em 2019

ggarrange(n_grafico, nov_grafico,
         common.legend = T,
         legend = "bottom")


################# Grafico Empregadores Formal/Informal em 2019

ggarrange(nuevo_grafico, nueva_grafico,
          common.legend = T,
          legend = "bottom")

######### Gr?fico Inativos em 2019

ggarrange(grafico_inat,
          common.legend = T,
          legend = "bottom")

###### Gr?fico Sal?rio M?dio Educa??o

ggarrange(grafico_sal,
          common.legend = T,
          legend = "bottom")

## INFORMA??ES IMPORTANTES:
## 1. TRABALHADORES COM CART ASS S?O TRAB DOM?STICO, SETOR PRIVADO E SETOR P?BLICO
## 2. TRABALHADORES CONTRIBUINTES DO INSS S?O TRAB DOM?STICO, SETOR PRIVADO, SETOR P?BLICO, EMPREGADOR E CONTA PR?PRIA
## 3. MILITARES N?O TEM CARTEIRA E NEM CONTRIBUEM PRO INSS