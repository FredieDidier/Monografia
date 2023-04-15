# The code is loading multiple R data files stored in a directory named "build/output/panel_by_education_level" 
# and combining them into a single data frame called base_reg using map_dfr(). 
# It then converts the year_quarter variable to numeric format and creates a new variable called 
# lead_position which contains the next position in the sequence for each individual in the dataset.
# The code then creates a variable called position_transition which is a string combining the position_names and lead_position variables using "to" as a separator. 
# Finally, the code removes any observations where position_transition contains the string "NA", and writes the resulting data frame as a Stata file to "./output/_main_data.dta".

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

write.dta(base_reg, "./output/_main_data.dta")



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


