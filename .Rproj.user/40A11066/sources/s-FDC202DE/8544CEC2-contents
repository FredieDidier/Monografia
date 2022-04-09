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
           V4013, V4039C,
           VD4012, VD4017) %>%
    rename(id_code = idind, year = Ano, quarter = Trimestre,
           primary_sampling_unit = UPA,
           area_type = V1023,
           weights = V1028,
           hours_worked = V4039C,
           gender = V2007, age = V2009,
           race = V2010,
           educ_level = V3003A, 
           higher_educ_course_attended = V3009A,
           sector_code = V4013,
           number_of_jobs = V4009,
           cnpj = V4019,
           job_function = V4012,
           job_area = V4014,
           worker = V4001,
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
           household_location = V1022) %>%
    unite(col = "year_quarter", year:quarter, sep = "_")
  
  df = df %>%
    mutate(position = case_when(workforce_condition == 2 ~ 1,
                                worker == 2 & occupation_condition == 2 & workforce_condition == 1 ~ 2,
                                worker == 1 & work_category == 1 & job_function == 3 ~ 3,
                                worker == 1 & work_category == 2 & job_function == 3 ~ 4,
                                worker == 1 & job_function == 6 & social_security_taxpayer == 1 ~ 5,
                                worker == 1 & job_function == 6 & social_security_taxpayer == 2 ~ 6,
                                worker == 1 & job_function == 5 & cnpj == 1 ~ 7,
                                worker == 1 & job_function == 5 & cnpj == 2 ~ 8,
                                worker == 1 & job_function == 4 & work_category %in% c(5,7) ~ 9,
                                worker == 1 & job_function == 4 & work_category == 6 ~ 10)) %>%
    filter(!is.na(position)) %>%
    group_by(id_code) %>%
    mutate(new_id = n()) %>%
    filter(new_id == 2) %>%
    select(id_code, year_quarter, workforce_condition, worker, signed_work_card, cnpj,
           job_function, occupation_condition, position, social_security_taxpayer,
           higher_educ_level, work_category, weights) %>%
    mutate(educ = case_when(
      higher_educ_level %in% c(1,2) ~ 1,
      higher_educ_level %in% c(3,4) ~ 2,
      higher_educ_level %in% c(5,6) ~ 3,
      higher_educ_level %in% c(7) ~ 4
    )) %>%
    select(-higher_educ_level)
  
}

