# The code defines a function called clean_painel which takes a dataframe as input and processes it to create a cleaned version of the original data. The cleaned data includes selected variables and renamed columns for easier identification. Additionally, a new variable called position is created based on the values of other variables. Finally, a new variable educ is created based on the value of the higher_educ_level variable.
# The function uses the tidyverse package, which is a collection of R packages for data manipulation and visualization.
# The mutate() function is used to create new variables, and the select() function is used to select a subset of columns from the original data.
# The unite() function is used to combine the year, quarter columns into a new year_quarter column.
# The case_when() function is used to create a new variable position based on the value of several other variables. The variable takes on one of ten possible values based on a series of logical conditions that test the values of worker, job_function, work_category, occupation_condition, social_security_taxpayer, and cnpj.
# The educ variable is created using case_when() to categorize the value of higher_educ_level into one of four categories.
# Note that the function does not return the cleaned dataframe; it only modifies the input dataframe in place. To get the cleaned dataframe, you would need to call the function with a dataframe as input and then save the modified dataframe.

library(tidyverse)

clean_painel = function(df){
  
  df <- df %>%
    select(idind, Ano, Trimestre,
           UF, UPA, V1022, V1028, V2007, V2009, V1022,
           V1023, V2010, V3003A, V3009A,
           V4009, V4012, V4014, V4019,
           V4001, V4028, V4029,
           V4032, V403312, V4034, V4040, V4013,
           V4071, V4076, VD2003, 
           VD3004, VD3005, VD4001, VD4002,
           VD4005, VD4007, VD4009,
           V4013, V4039,
           VD4012, VD4017, V4010, VD4037,
           V40121, V4025) %>%
    rename(id_code = idind, year = Ano, quarter = Trimestre,
           state = UF,
           primary_sampling_unit = UPA,
           area_type = V1023,
           weights = V1028,
           hours_worked = V4039,
           gender = V2007, age = V2009,
           race = V2010,
           educ_level = V3003A, 
           higher_educ_course_attended = V3009A,
           sector_code = V4013,
           number_of_jobs = V4009,
           cnpj = V4019,
           job_function = V4012,
           job_area = V4014,
           public_server = V4028,
           signed_work_card = V4029,
           social_security_taxpayer = V4032,
           reference_month_income = V4034,
           job_start= V4040,
           looked_for_a_job = V4071,
           time_without_job = V4076,
           number_household_members = VD2003,
           higher_educ_level = VD3004,
           years_of_study = VD3005,
           workforce_condition = VD4001,
           occupation_condition = VD4002,
           despondent_people = VD4005,
           work_position = VD4007,
           work_category = VD4009,
           social_security_taxpayer_ref_week = VD4012,
           monthly_work_income = VD4017,
           household_location = V1022,
           occupation_code = V4010,
           not_salaried_worker = V40121,
           temporary_worker = V4025) %>%
    unite(col = "year_quarter", year:quarter, sep = "_")
  
  df = df %>%
    mutate(position = case_when(workforce_condition == 2 | is.na(workforce_condition) ~ 1, #inativo
                                occupation_condition == 2  & workforce_condition == 1 ~ 2, #desempregado
                                occupation_condition == 1 & work_category %in% c(1,3) & job_function %in% c(1,3) ~ 3, #Formal Private
                                occupation_condition == 1 & work_category %in% c(2,4,10) & job_function %in% c(1,3,7) ~ 4, #Informal Private
                                occupation_condition == 1 & job_function == 6 & social_security_taxpayer == 1 ~ 5, #Formal Self-Employed
                                occupation_condition == 1 & job_function == 6 & social_security_taxpayer == 2 ~ 6, #Informal Self-Employed
                                occupation_condition == 1 & job_function == 5 & cnpj == 1 ~ 7, #Formal Employer
                                occupation_condition == 1 & job_function == 5 & cnpj == 2 ~ 8, #Informal Employer
                                occupation_condition == 1 & job_function %in% c(2,4) & work_category %in% c(5,7) ~ 9, #Formal Public Sector
                                occupation_condition == 1 & job_function %in% c(2,4) & work_category == 6 ~ 10)) %>% #Informal Public Sector
    select(id_code, year_quarter, state, workforce_condition, signed_work_card, cnpj,
           job_function, hours_worked, not_salaried_worker, temporary_worker,
           occupation_condition, position, social_security_taxpayer,
           higher_educ_level, work_category, gender, race, age,
           years_of_study, monthly_work_income, weights, job_start, sector_code,
           household_location, occupation_code) %>%
    mutate(educ = case_when(
      higher_educ_level %in% c(1,2) ~ 1,
      higher_educ_level %in% c(3,4) ~ 2,
      higher_educ_level %in% c(5,6) ~ 3,
      higher_educ_level %in% c(7) ~ 4
    ))
  
}

