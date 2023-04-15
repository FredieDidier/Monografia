source("./build/_cleaning_paineis.R")
source("./build/_aggregating_sector_codes.R")
source("./build/_aggregating_occupation_codes.R")

trimestres <- c("2012_1", "2012_2", "2012_3", "2012_4",
                "2013_1", "2013_2", "2013_3", "2013_4",
                "2014_1", "2014_2", "2014_3", "2014_4",
                "2015_1", "2015_2", "2015_3", "2015_4",
                "2016_1", "2016_2", "2016_3", "2016_4",
                "2017_1", "2017_2", "2017_3", "2017_4",
                "2018_1", "2018_2", "2018_3", "2018_4",
                "2019_1", "2019_2", "2019_3", "2019_4",
                "2020_1", "2020_2", "2020_3", "2020_4",
                "2021_1", "2021_2", "2021_3", "2021_4",
                "2022_1", "2022_2", "2022_3")

trimestres <- rep(trimestres, 4)

educ <- c(
  rep(1, 43),
  rep(2, 43),
  rep(3, 43),
  rep(4, 43)
)

map(trimestres,
  function(trim){
    
    base::message(paste0("Trimester ", trim))
            
     df <- haven::read_dta(
       paste0("data-raw/Trimestres/painel_",
              trim,
              ".dta")
       ) %>%
       clean_painel() %>%
       aggregate_sectors() %>%
       aggregate_occupations()
     
     map(1:4,
         function(educ_level){
           base::message(paste0("    ", "Education level ", educ_level))
           
           df = df %>%
           filter(educ == educ_level, year_quarter == trim) %>%
           save(df, paste0("output/trimestre_", trim, "_", educ_level, ".RData"))
         }
         )
   }
                                   
)
