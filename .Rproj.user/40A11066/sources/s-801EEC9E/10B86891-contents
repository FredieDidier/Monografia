library(tidyverse)

trimestres <- c("2012_1", "2012_2", "2012_3", "2012_4",
                   "2013_1", "2013_2", "2013_3", "2013_4",
                   "2014_1", "2014_2", "2014_3", "2014_4",
                   "2015_1", "2015_2", "2015_3", "2015_4",
                   "2016_1", "2016_2", "2016_3", "2016_4",
                   "2017_1", "2017_2", "2017_3", "2017_4",
                   "2018_1", "2018_2", "2018_3", "2018_4",
                   "2019_1", "2019_2", "2019_3", "2019_4",
                   "2020_1", "2020_2", "2020_3", "2020_4",
                   "2021_1", "2021_2", "2021_3")

trimestres <- rep(trimestres, 4)

next_trimestres <- c("2012_2", "2012_3", "2012_4",
                        "2013_1", "2013_2", "2013_3", "2013_4",
                        "2014_1", "2014_2", "2014_3", "2014_4",
                        "2015_1", "2015_2", "2015_3", "2015_4",
                        "2016_1", "2016_2", "2016_3", "2016_4",
                        "2017_1", "2017_2", "2017_3", "2017_4",
                        "2018_1", "2018_2", "2018_3", "2018_4",
                        "2019_1", "2019_2", "2019_3", "2019_4",
                        "2020_1", "2020_2", "2020_3", "2020_4",
                        "2021_1", "2021_2", "2021_3", "2021_4")

next_trimestres <- rep(next_trimestres, 4)

educ <- c(
  rep(1, 39),
  rep(2, 39),
  rep(3, 39),
  rep(4, 39)
)

source("./Transitions Matrices/Funções para Matriz.R")

matrizes <- pmap_dfr(
  
    list(trimestres, next_trimestres, educ),
     
     function(trim, next_trim, educ){
       
       df <- readr::read_rds(
         paste0("Cleaning Data/Paineis/painel_",
                trim,
                "_",
                educ,
                ".rds")
       )
       
       mat <- cria_matriz_transicao(df, trim, next_trim)
       colnames(mat) <- 1:10
       
       mat <- as_tibble(mat)
       mat$posicao_final <- 1:10
       
       mat <- mat %>%
         pivot_longer(-posicao_final,
                      names_to = "posicao_inicial",
                      values_to = "transition")
       
       mat$educ <- educ
       mat$trim <- trim
       
       mat <- mat %>%
         relocate(trim, educ, posicao_inicial, posicao_final)
     }
       
)

write_csv(matrizes, "./Transitions Matrices/transicoes_por_educ.csv")


## Formal - Desempregado

matriz1 = matrizes %>%
  filter(posicao_inicial %in% c(3,5,7,9) &
           posicao_final == 2) %>%
  group_by(trim, educ) %>%
  summarise(transition = sum(transition)) %>%
  mutate(educ = as.character(educ))

graf_f_d = ggplot(matriz1, aes(x = trim, y = transition, 
                               color = educ, group = educ)) +
  scale_color_manual(labels = c("Uneducated and Incompleted Primary School",
                                "Completed Primary School and Incompleted High School",
                                "Completed High School and Incompleted College Degree",
                                "Completed College Degree"), values = carto_pal(name = "Vivid")) +
  geom_line(size = 2) +
  geom_point(size = 2)


## Informal - Desempregado

matriz2 = matrizes %>%
  filter(posicao_inicial %in% c(4,6,8,10) &
           posicao_final == 2) %>%
  group_by(trim, educ) %>%
  summarise(transition = sum(transition)) %>%
  mutate(educ = as.character(educ))

graf_i_d = ggplot(matriz2, aes(x = trim, y = transition, 
                               color = educ, group = educ)) +
  scale_color_manual(labels = c("Uneducated and Incompleted Primary School",
                                "Completed Primary School and Incompleted High School",
                                "Completed High School and Incompleted College Degree",
                                "Completed College Degree"), values = carto_pal(name = "Vivid")) +
  geom_line(size = 2) +
  geom_point(size = 2)


## Formal - Informal
  
matriz3 = matrizes %>%
  filter(posicao_inicial %in% c(3,5,7,9) &
           posicao_final %in% c(4,6,8,10)) %>%
  group_by(trim, educ) %>%
  summarise(transition = sum(transition)) %>%
  mutate(educ = as.character(educ))

graf_f_i = ggplot(matriz3, aes(x = trim, y = transition, 
                               color = educ, group = educ)) +
  scale_color_manual(labels = c("Uneducated and Incompleted Primary School",
                                "Completed Primary School and Incompleted High School",
                                "Completed High School and Incompleted College Degree",
                                "Completed College Degree"), values = carto_pal(name = "Vivid")) +
  geom_line(size = 2) +
  geom_point(size = 2)


## Formal - Inativo

matriz4 = matrizes %>%
  filter(posicao_inicial %in% c(3,5,7,9) &
           posicao_final %in% c(1)) %>%
  group_by(trim, educ) %>%
  summarise(transition = sum(transition)) %>%
  mutate(educ = as.character(educ))

graf_f_inat = ggplot(matriz4, aes(x = trim, y = transition, 
                               color = educ, group = educ)) +
  scale_color_manual(labels = c("Uneducated and Incompleted Primary School",
                                "Completed Primary School and Incompleted High School",
                                "Completed High School and Incompleted College Degree",
                                "Completed College Degree"), values = carto_pal(name = "Vivid")) +
  geom_line(size = 2) +
  geom_point(size = 2)


## Informal - Inativo

matriz5 = matrizes %>%
  filter(posicao_inicial %in% c(4,6,8,10) &
           posicao_final %in% c(1)) %>%
  group_by(trim, educ) %>%
  summarise(transition = sum(transition)) %>%
  mutate(educ = as.character(educ))

graf_i_inat = ggplot(matriz5, aes(x = trim, y = transition, 
                                  color = educ, group = educ)) +
  scale_color_manual(labels = c("Uneducated and Incompleted Primary School",
                                "Completed Primary School and Incompleted High School",
                                "Completed High School and Incompleted College Degree",
                                "Completed College Degree"), values = carto_pal(name = "Vivid")) +
  geom_line(size = 2) +
  geom_point(size = 2)
