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

educ <- c(
  rep(1, 39),
  rep(2, 39),
  rep(3, 39),
  rep(4, 39)
)

priv_public_reg_workers = map2_dfr(trimestres, educ,
                                   
                                   purrr::insistently(function(trim, educ){
                                     
                                     df <- readr::read_rds(
                                       paste0("input/trimestre_",
                                              trim,
                                              "_",
                                              educ,
                                              ".rds")
                                     )
                                     
                                     df = df %>%
                                       mutate(aux = case_when(worker == 1 & job_function %in% c(3,4) & work_category %in% c(1,5,7) ~ 1,
                                                              TRUE ~ 0))
                                     
                                     df %>% group_by(year_quarter, educ) %>%
                                       mutate(aux = aux * weights) %>%
                                       summarise(proporcao = sum(aux)/sum(weights))
                                     
                                   }, quiet = FALSE)
                                   
)


write_csv(priv_public_reg_workers, "./input/prop_cart_ass_ocupados_por_educ.csv")



priv_public_unreg_workers = map2_dfr(trimestres, educ,
                                     
                                     purrr::insistently(function(trim, educ){
                                       
                                       df <- readr::read_rds(
                                         paste0("input/trimestre_",
                                                trim,
                                                "_",
                                                educ,
                                                ".rds")
                                       )
                                       
                                       df = df %>%
                                         mutate(aux = case_when(worker == 1 & job_function %in% c(3,4) & work_category %in% c(2,6) ~ 1,
                                                                TRUE ~ 0))
                                       
                                       df %>% group_by(year_quarter, educ) %>%
                                         mutate(aux = aux * weights) %>%
                                         summarise(proporcao = sum(aux)/sum(weights))
                                       
                                     }, quiet = FALSE)
                                     
)


write_csv(priv_public_unreg_workers, "./input/prop_s_cart_ass_ocupados_por_educ.csv")



self_employed_inss_taxpayer = map2_dfr(trimestres, educ,
                                       
                                       purrr::insistently(function(trim, educ){
                                         
                                         df <- readr::read_rds(
                                           paste0("input/trimestre_",
                                                  trim,
                                                  "_",
                                                  educ,
                                                  ".rds")
                                         )
                                         
                                         df = df %>%
                                           mutate(aux = case_when(worker == 1 & job_function %in% c(6) & social_security_taxpayer == 1 ~ 1,
                                                                  TRUE ~ 0))
                                         
                                         df %>% group_by(year_quarter, educ) %>%
                                           mutate(aux = aux * weights) %>%
                                           summarise(proporcao = sum(aux)/sum(weights))
                                         
                                       }, quiet = FALSE)
                                       
)

write.csv(self_employed_inss_taxpayer, "./input/prop_self_employed_inss_ocupados_por_educ.csv")



self_employed__s_inss_taxpayer = map2_dfr(trimestres, educ,
                                       
                                       purrr::insistently(function(trim, educ){
                                         
                                         df <- readr::read_rds(
                                           paste0("input/trimestre_",
                                                  trim,
                                                  "_",
                                                  educ,
                                                  ".rds")
                                         )
                                         
                                         df = df %>%
                                           mutate(aux = case_when(worker == 1 & job_function %in% c(6) & social_security_taxpayer == 2 ~ 1,
                                                                  TRUE ~ 0))
                                         
                                         df %>% group_by(year_quarter, educ) %>%
                                           mutate(aux = aux * weights) %>%
                                           summarise(proporcao = sum(aux)/sum(weights))
                                         
                                       }, quiet = FALSE)
                                       
)

write.csv(self_employed__s_inss_taxpayer, "./input/prop_self_employed_s_inss_ocupados_por_educ.csv")
