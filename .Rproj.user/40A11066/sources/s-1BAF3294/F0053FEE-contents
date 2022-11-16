

### filtrar pra pessoas de determinada educação
### Multiplicar pela soma total do peso amostral
### Identificar esses mesmos indivíduos no trimestre seguinte
### E utilizar o peso amostral desse indivíduo no trimestre anterior

trimestres <- c("2012_1", "2012_2", "2012_3", "2012_4",
                   "2013_1", "2013_2", "2013_3", "2013_4",
                   "2014_1", "2014_2", "2014_3", "2014_4",
                   "2015_1", "2015_2", "2015_3", "2015_4",
                   "2016_1", "2016_2", "2016_3", "2016_4",
                   "2017_1", "2017_2", "2017_3", "2017_4",
                   "2018_1", "2018_2", "2018_3", "2018_4",
                   "2019_1", "2019_2", "2019_3", "2019_4",
                   "2020_1", "2020_2", "2020_3", "2020_4",
                   "2021_1", "2021_2", "2021_3", "2021_4")

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
                        "2021_1", "2021_2", "2021_3", "2021_4", "2022_1")

next_trimestres <- rep(next_trimestres, 4)

educ <- c(
  rep(1, 40),
  rep(2, 40),
  rep(3, 40),
  rep(4, 40)
)

source("./analysis/_transition_matrix_function.R")

matrizes <- pmap_dfr(
  
    list(trimestres, next_trimestres, educ),
     
     purrr::insistently(function(trim, next_trim, educ){
       message(paste0("Transition ", trim, " to ", next_trim, "\n educ ", educ))
       
       df <- readr::read_rds(
         paste0("input/painel_",
                trim,
                "_",
                educ,
                ".rds")
       )
       
       df = df %>%
         mutate(aux = case_when(position %in% c(3,5,7,9) ~ 1, #formal
                                position %in% c(4,6,8,10) ~ 2, #informal
                                position %in% c(1,2) ~ 3 #inactive + unemployed
                               )) %>%
       mutate(position = aux)
       
       mat <- cria_matriz_transicao(df, trim, next_trim, 3)
       colnames(mat) <- 1:3
       
       mat <- as_tibble(mat)
       mat$posicao_inicial <- 1:3
       
       mat <- mat %>%
         pivot_longer(-posicao_inicial,
                      names_to = "posicao_final",
                      values_to = "transition")
       
       mat$educ <- educ
       mat$trim <- trim
       
       mat <- mat %>%
         relocate(trim, educ, posicao_inicial, posicao_final)
     }, quiet = FALSE)
       
)

write_csv(matrizes, "./input/transicoes_por_educ_3_x_3.csv")



matrizes <- pmap_dfr(
  
  list(trimestres, next_trimestres, educ),
  
  purrr::insistently(function(trim, next_trim, educ){
    message(paste0("Transition ", trim, " to ", next_trim, "\n educ ", educ))
    
    df <- readr::read_rds(
      paste0("input/painel_",
             trim,
             "_",
             educ,
             ".rds")
    )
    
    df = df %>%
      mutate(aux = case_when(occupation_condition == 1 ~ 1,
                             position %in% c(1,2) ~ 2)) %>%
      mutate(position = aux)
    
    mat <- cria_matriz_transicao(df, trim, next_trim, 2)
    colnames(mat) <- 1:2
    
    mat <- as_tibble(mat)
    mat$posicao_inicial <- 1:2
    
    mat <- mat %>%
      pivot_longer(-posicao_inicial,
                   names_to = "posicao_final",
                   values_to = "transition")
    
    mat$educ <- educ
    mat$trim <- trim
    
    mat <- mat %>%
      relocate(trim, educ, posicao_inicial, posicao_final)
  }, quiet = FALSE)
  
)

write_csv(matrizes, "./input/transicoes_por_educ_2_x_2.csv")



matrizes <- pmap_dfr(
  
  list(trimestres, next_trimestres, educ),
  
  purrr::insistently(function(trim, next_trim, educ){
    
    message(paste0("Transition ", trim, " to ", next_trim, "\n educ ", educ))
    
    df <- readr::read_rds(
      paste0("input/painel_",
             trim,
             "_",
             educ,
             ".rds")
    )
    
    mat <- cria_matriz_transicao(df, trim, next_trim, 10)
    colnames(mat) <- 1:10
    
    mat <- as_tibble(mat)
    mat$posicao_inicial <- 1:10
    
    mat <- mat %>%
      pivot_longer(-posicao_inicial,
                   names_to = "posicao_final",
                   values_to = "transition")
    
    mat$educ <- educ
    mat$trim <- trim
    
    mat <- mat %>%
      relocate(trim, educ, posicao_inicial, posicao_final)
  }, quiet = FALSE)
  
)

write_csv(matrizes, "./input/transicoes_por_educ_10_x_10.csv")




matrizes <- pmap_dfr(
  
  list(trimestres, next_trimestres, educ),
  
  purrr::insistently(function(trim, next_trim, educ){
    
    message(paste0("Transition ", trim, " to ", next_trim, "\n educ ", educ))
    
    df <- readr::read_rds(
      paste0("input/painel_",
             trim,
             "_",
             educ,
             ".rds")
    )
    
    mat <- cria_matriz_transicao(df, trim, next_trim, 10, prop = FALSE)
    colnames(mat) <- 1:10
    
    mat <- as_tibble(mat)
    mat$posicao_inicial <- 1:10
    
    mat <- mat %>%
      pivot_longer(-posicao_inicial,
                   names_to = "posicao_final",
                   values_to = "transition")
    
    mat$educ <- educ
    mat$trim <- trim
    
    mat <- mat %>%
      relocate(trim, educ, posicao_inicial, posicao_final)
  }, quiet = FALSE)
  
)

 write_csv(matrizes, "./input/transicoes_por_educ_10_x_10_prop_F.csv")

 
 ### Para fazer a matriz de transição de 2019 por educ
 
 matrizes = read_csv("./input/transicoes_por_educ_10_x_10_prop_F.csv")
 
 matrizes = matrizes %>%
   filter(trim %in% c("2019_1", "2019_2", "2019_3", "2019_4")) %>%
   group_by(educ, posicao_inicial, posicao_final) %>%
   summarise(transition = sum(transition)) %>%
   mutate(transition = transition/sum(transition)) %>%
   mutate(transition = transition * 100)

