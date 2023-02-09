

trimestres <- c("2012_1", "2012_2", "2012_3", "2012_4",
                "2013_1", "2013_2", "2013_3", "2013_4",
                "2014_1", "2014_2", "2014_3", "2014_4",
                "2015_1", "2015_2", "2015_3", "2015_4",
                "2016_1", "2016_2", "2016_3", "2016_4",
                "2017_1", "2017_2", "2017_3", "2017_4",
                "2018_1", "2018_2", "2018_3", "2018_4",
                "2019_1", "2019_2", "2019_3", "2019_4",
                "2020_1", "2020_2", "2020_3", "2020_4",
                "2021_1", "2021_2", "2021_3", "2021_4", "2022_1")

trimestres <- rep(trimestres, 4)

educ <- c(
  rep(1, 41),
  rep(2, 41),
  rep(3, 41),
  rep(4, 41)
)

base_reg = map2_dfr(trimestres, educ,
  
  function(trim, educ){
  
    message(paste0("Downloading", trim, "\n educ ", educ))
    
    df <- readr::read_rds(
      paste0("input/trimestre_",
             trim,
             "_",
             educ,
             ".rds")
      
  
      
    )
    df
    }
)

base_reg = base_reg %>%
  mutate(position_names = case_when(position %in% c(3,5,7,9) ~ "Formal",
                                    position %in% c(4,6,8,10) ~ "Informal",
                                    position %in% c(1,2) ~ "Non-Employed")) %>%
  mutate(homem = case_when(gender == 1 ~ 1,
                            gender == 2 ~ 0)) %>%
  mutate(negro = case_when(race == 2 | race == 4 | race == 5 ~ 1,
                           race == 1 | race == 3 ~ 0)) %>%
  mutate(urbana = case_when(household_location == 1 ~ 1,
                            household_location == 2 ~ 0))

base_reg = base_reg %>%
  mutate(year_quarter = str_remove(year_quarter, "_")) %>%
  mutate(year_quarter = as.numeric(year_quarter)) %>%
  group_by(id_code) %>%
  arrange(year_quarter) %>%
  mutate(lead_position = dplyr::lead(position_names)) %>%
  mutate(position_transition = paste(position_names, sep = " to ", lead_position)) %>%
  filter(!str_detect(position_transition, "NA"))


library(foreign)
write.dta(base_reg, "./input/regression_df.dta")

