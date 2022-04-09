

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

source("./analysis/_transition_matrix_function.R")

matrizes <- pmap_dfr(
  
    list(trimestres, next_trimestres, educ),
     
     function(trim, next_trim, educ){
       
       df <- readr::read_rds(
         paste0("input/painel_",
                trim,
                "_",
                educ,
                ".rds")
       )
       
       df = df %>%
         mutate(aux = case_when(position %in% c(3,5,7,9) ~ 1,
                                position %in% c(4,6,8,10) ~ 2,
                                position == 1 ~ 3,
                                position == 2 ~ 4)) %>%
         mutate(position = aux)
       
       mat <- cria_matriz_transicao(df, trim, next_trim, 4)
       colnames(mat) <- 1:4
       
       mat <- as_tibble(mat)
       mat$posicao_final <- 1:4
       
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

write_csv(matrizes, "./input/transicoes_por_educ.csv")
