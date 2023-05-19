# The code reads a list of files from a specific directory using the dir_ls function from the fs package. It then loads each file from the list using the get and load functions, and combines them into a single data frame using the map_dfr function from the purrr package.
# 
# The code then converts the year_quarter column from string to numeric using as.numeric and str_remove functions from the stringr package. It creates a new column called lead_position by shifting the position_names column by one observation for each id_code, using the shift function from the data.table package. It then creates a new column called position_transition by pasting position_names and lead_position together, separated by " to ", using the paste function.
# 
# The code removes any observations where position_transition contains the string "NA" and the ones started with "Non-Employed", using the !grepl function. Finally, it writes the resulting data frame to a Stata file using the write.dta function from the foreign package.

file_list <- dir_ls("build/output/panel_by_education_level")
file_list = str_extract(file_list, "painel_[0-9]{4}_[0-9]{1}_[0-9]{1}")

reg_prep = function(panel){
  
  panel = str_extract(panel, "painel_[0-9]{4}_[0-9]{1}_[0-9]{1}")

  df = data.table(get(load(paste0("build/output/panel_by_education_level/", panel, ".RData"))))

df[, `:=` (
  year_quarter = as.numeric(str_remove(year_quarter, "_")))]

df[, `:=` (
  position_names = case_when(position %in% c(3,5,7,9) ~ "Formal",
                             position %in% c(4,6,8,10) ~ "Informal",
                             position %in% c(1,2) ~ "Non-Employed"))]

  df[, `:=`( homem = case_when(gender == 1 ~ 1,
                    gender == 2 ~ 0),
                    negro = case_when(race == 2 | race == 4 | race == 5 ~ 1,
                                      race == 1 | race == 3 ~ 0))]
  
  df[, `:=`(urbana = case_when(household_location == 1 ~ 1,
                     household_location == 2 ~ 0))]

df[, `:=` (
    lead_position = shift(position_names, type = "lead")), by = id_code]

df[, position_transition := paste(position_names, sep = " to ", lead_position), by = id_code]

df = df %>%
  select(-c(new_id))

df = df %>%
  filter(position_transition %in% c("Formal to Formal",
                                    "Formal to Informal",
                                    "Formal to Non-Employed",
                                    "Informal to Formal",
                                    "Informal to Informal",
                                    "Informal to Non-Employed"))

df = df %>%
  select(-c(not_salaried_worker))

df = df %>%
  mutate(labor_status = case_when(temporary_worker == 1 ~ "Temporary",
                                  temporary_worker == 2 ~ "Permanent",
                                  monthly_work_income > 0 ~ "Salaried",
                                  occupation_condition == 1 & monthly_work_income == 0 ~ "Not Salaried"))

# Set the path to the folder
folder_path <- paste0("build/output/regression") 

# Check if the folder exists
if (!file.exists(folder_path)) {
  # Create the folder if it does not exist
  dir.create(folder_path)
}  

save(df, file = paste0("build/output/regression/",
                       panel,
                       ".RData"))
}

for(i in 1:length(file_list)){
  cat(i, sep="\n")
  
  reg_prep(file_list[i])
  }

ffs = dir_ls("build/output/regression")
ffs = str_extract(ffs, "painel_[0-9]{4}_[0-9]{1}_[0-9]{1}") 
## A linha 78 filtra o diretório "regression" para apenas os arquivos que contém "painel_"

df <- ffs %>% 
  map_dfr(~ get(load(.))) 

df = df %>%
  group_by(id_code) %>%
  mutate(id = n())

write.dta(df, "build/output/regression/main_data.dta")

#df = df %>%
  #mutate(position_names = case_when(position %in% c(3,5,7,9) ~ "Formal",
                  #                  position %in% c(4,6,8,10) ~ "Informal",
                   #                 position %in% c(1,2) ~ "Non-Employed")) %>%
  #mutate(homem = case_when(gender == 1 ~ 1,
                           #gender == 2 ~ 0)) %>%
  #mutate(negro = case_when(race == 2 | race == 4 | race == 5 ~ 1,
   #                        race == 1 | race == 3 ~ 0)) %>%
  #mutate(urbana = case_when(household_location == 1 ~ 1,
                            #household_location == 2 ~ 0))


#df = df %>%
 # mutate(year_quarter = str_remove(year_quarter, "_")) %>%
  #mutate(year_quarter = as.numeric(year_quarter)) %>%
  #group_by(id_code) %>%
  #arrange(year_quarter) %>%
  #mutate(lead_position = dplyr::lead(position_names)) %>%
  #mutate(position_transition = paste(position_names, sep = " to ", lead_position)) %>%
  #filter(!str_detect(position_transition, "NA"))

