# This code is written in the R programming language and performs a loop to generate clean datasets.
# The first lines define two vectors, list_trimestres and list_educ, which will be used to specify the datasets that the loop will generate. list_trimestres contains strings with the names of files that the loop will read, and list_educ contains the education levels that will be used to filter the data.
# The loop starts with the map2 function from the purrr package. This function iterates over two vectors, list_trimestres and list_educ, applying a function to each pair of values. The function is defined as an anonymous function with two arguments, trim and educ_level, corresponding to the current values of the two vectors.
# Inside the loop, the function reads a file using the haven::read_dta function and applies two functions, clean_painel() and aggregate_sectors(), to clean and aggregate the data. The cleaned dataset is then filtered by educ_level and year_quarter, and the id_code column is selected and unlisted to obtain a vector of unique IDs.
# The cleaned dataset is filtered again to keep only the rows that have an id_code present in the vector of unique IDs, then grouped by id_code and sorted by id_code and year_quarter. A new column, new_id, is created that indicates the row's position in the group, and only rows with a new_id equal to 2 are kept.
# Finally, the resulting dataset is saved to a file in RDS format using the readr::write_rds function, with a filename based on the current values of trim and educ_level.
# In summary, this code generates clean datasets from a set of files, filters them by education level and quarter, and keeps only the second row of each group of IDs. The resulting datasets are saved to separate files.

##################################
# Loop que gera paineis limpos   #                        
##################################
list_trimestres <- c("2012_1", "2012_2", "2012_3", "2012_4",
                     "2013_1", "2013_2", "2013_3", "2013_4",
                     "2014_1", "2014_2", "2014_3", "2014_4",
                     "2015_1", "2015_2", "2015_3", "2015_4",
                     "2016_1", "2016_2", "2016_3", "2016_4",
                     "2017_1", "2017_2", "2017_3", "2017_4",
                     "2018_1", "2018_2", "2018_3", "2018_4",
                     "2019_1", "2019_2", "2019_3", "2019_4",
                     "2020_1", "2020_2", "2020_3", "2020_4",
                     "2021_1", "2021_2", "2021_3", "2021_4")

list_trimestres <- rep(list_trimestres, 4)

list_educ <- c(
  rep(1, 40),
  rep(2, 40),
  rep(3, 40),
  rep(4, 40)
)

map2(list_trimestres, list_educ,
     
     function(trim, educ_level){
       df <- haven::read_dta(paste0("build/output/painel_", trim, ".dta")) %>%
         clean_painel() %>%
         aggregate_sectors()
       
       df_aux <- df %>%
         filter(educ == educ_level, year_quarter == trim) %>%
         select(id_code) %>%
         unlist()
       
       df <- df %>%
         filter(id_code %in% df_aux) %>%
         group_by(id_code) %>%
         arrange(id_code, year_quarter) %>%
         mutate(new_id = n()) %>%
         filter(new_id == 2)
       
       df %>%
         load(., file = paste0("build/output/painel_",

                                 trim,
                                 "_",
                                 educ_level,
                                 ".RData")
         )
     }
)