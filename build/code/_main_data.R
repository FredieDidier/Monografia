# The code reads a list of files from a specific directory using the dir_ls function from the fs package. It then loads each file from the list using the get and load functions, and combines them into a single data frame using the map_dfr function from the purrr package.
# 
# The code then converts the year_quarter column from string to numeric using as.numeric and str_remove functions from the stringr package. It creates a new column called lead_position by shifting the position_names column by one observation for each id_code, using the shift function from the data.table package. It then creates a new column called position_transition by pasting position_names and lead_position together, separated by " to ", using the paste function.
# 
# The code removes any observations where position_transition contains the string "NA", using the !grepl function. Finally, it writes the resulting data frame to a Stata file using the write.dta function from the foreign package.

file_list <- dir_ls("build/output/panel_by_education_level")
base_reg <- file_list %>% 
  map_dfr(~ get(load(.))) 

base_reg = data.table(base_reg)

base_reg[, `:=` (
  year_quarter = as.numeric(str_remove(year_quarter, "_")))]

base_reg[, `:=` (
    lead_position = shift(position_names, type = "lead")), by = id_code]

base_reg[, position_transition := paste(position_names, sep = " to ", lead_position), by = id_code]

base_reg <- base_reg[!grepl("NA", position_transition)]

write.dta(base_reg, "./output/main_data.dta")



#base_reg = base_reg %>%
  #mutate(position_names = case_when(position %in% c(3,5,7,9) ~ "Formal",
                  #                  position %in% c(4,6,8,10) ~ "Informal",
                   #                 position %in% c(1,2) ~ "Non-Employed")) %>%
  #mutate(homem = case_when(gender == 1 ~ 1,
                           #gender == 2 ~ 0)) %>%
  #mutate(negro = case_when(race == 2 | race == 4 | race == 5 ~ 1,
   #                        race == 1 | race == 3 ~ 0)) %>%
  #mutate(urbana = case_when(household_location == 1 ~ 1,
                            #household_location == 2 ~ 0))


#base_reg = base_reg %>%
 # mutate(year_quarter = str_remove(year_quarter, "_")) %>%
  #mutate(year_quarter = as.numeric(year_quarter)) %>%
  #group_by(id_code) %>%
  #arrange(year_quarter) %>%
  #mutate(lead_position = dplyr::lead(position_names)) %>%
  #mutate(position_transition = paste(position_names, sep = " to ", lead_position)) %>%
  #filter(!str_detect(position_transition, "NA"))


