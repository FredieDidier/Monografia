library(tidyverse)
library(estimatr)
library(modelsummary)
library(extrafont)

painel = haven::read_dta("Painel_completo.dta")

clean_painel = function(df){
  
  df %>%
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
           household_location = V1022)
}

data = clean_painel(painel)



## Criando coluna de trimestre

data = unite(data, col = "year_quarter", year:quarter, sep = "_") 

## Criando dataframe s? filtrado para 2019 para a Tabela Descritiva

data_2019 = data %>%
  filter(year_quarter %in% c("2019_1", "2019_2", "2019_3", "2019_4"))
###########################################################

## CriandoVariavel Dependente - Trabalhador Conta Propria que contribui pro INSS

## Isso aqui foi uma regressao teste que eu fiz (apenas pra brincar)!

worker = data %>%
  pivot_wider(id_cols = c(id_code),
              names_from = year_quarter,
              values_from = worker)

worker = worker %>%
  filter(`2020_1` == 1)

variables = data %>%
  select(id_code, race, gender, higher_educ_level, weights,
         job_start, worker, year_quarter, household_location, job_function,
         social_security_taxpayer, sector_code)

merge = left_join(worker, variables)

merge = merge %>%
  mutate(unemployed = case_when(worker == 1 & social_security_taxpayer == 1 &
                                  job_function == 6 ~ 0,
                                worker == 2 & social_security_taxpayer == 2 &
                                  job_function  == 6 ~ 1)) %>%
  filter(year_quarter %in% c("2020_1", "2020_2", "2020_3", "2020_4")) %>%
  mutate(negro = case_when(race == 2 | race == 4 | race == 5 ~ 1,
                           race == 1 | race == 3 ~ 0))

reg = lm_robust(unemployed ~ negro + as.factor(gender) +
                  as.factor(higher_educ_level) +
                  as.factor(job_start) + as.factor(year_quarter) +
                  as.factor(household_location) + as.factor(sector_code),
                data = merge, weights = weights)


