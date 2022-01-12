library(tidyverse)
library(estimatr)
library(modelsummary)
library(extrafont)

setwd("C:\\GitHub\\TCC\\pnad_painel\\pnadcontinua")

painel = haven::read_dta("Painel_completo.dta")

clean_painel = function(df){
  
  df %>%
    select(idind, Ano, Trimestre,
           UF, UPA, V1022, V1028, V2007, V2009, V1022,
           V1023, V2010, V3003A, V3009A,
           V4001, V4009, V4012, V40121, V4014,
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
           worked_on_the_week = V4001,
           number_of_jobs = V4009,
           job_function = V4012,
           unpaid_worker = V40121,
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
           monthly_work_income = V403312,
           household_location = V1022)
}

data = clean_painel(painel)

## Criando dataframe só filtrado para 2019 para a Tabela Descritiva

data_2019 = data %>%
  filter(year_quarter %in% c("2019_1", "2019_2", "2019_3", "2019_4"))


## Criando 1ª Variável Dependente - Trabalhador empregado


data = unite(data, col = "year_quarter", year:quarter, sep = "_") %>%
  mutate(Male = case_when(gender == 1 ~ 1,
                            gender == 2 ~ 0)) %>%
  mutate(Female = case_when(gender == 1 ~ 0,
                          gender == 2 ~ 1)) %>%
  mutate(signed_card_employee = case_when(signed_work_card == 1 ~ 1,
                                          signed_work_card == 2 ~ 0)) %>%
  mutate(no_signed_card_employee = case_when(signed_work_card == 1 ~ 0,
                                          signed_work_card == 2 ~ 1)) %>%
  mutate(last_week_worker = case_when(worker == 1 ~ 1,
                                      worker == 2 ~ 0)) %>%
  mutate(no_last_week_worker = case_when(worker == 1 ~ 0,
                                         worker == 2 ~ 1)) %>%
  mutate(social_security_contributor = case_when(social_security_taxpayer  == 1 ~ 1,
                                                 social_security_taxpayer == 2 ~ 0)) %>%
  mutate(no_social_security_contributor = case_when(social_security_taxpayer == 1 ~ 0,
                                                 social_security_taxpayer == 2 ~ 1))

worker = data %>%
  pivot_wider(id_cols = c(id_code),
              names_from = year_quarter,
              values_from = worker)

worker = worker %>%
  filter(`2020_1` == 1)

variables = data %>%
  select(id_code, race, gender, higher_educ_level, weights,
         job_start, worker, year_quarter, household_location, job_function)

merge = left_join(worker, variables)

merge = merge %>%
  mutate(unemployed = case_when(worker == 1 ~ 0,
                                worker == 2 ~ 1)) %>%
  filter(year_quarter %in% c("2020_1", "2020_2", "2020_3", "2020_4")) %>%
  mutate(negro = case_when(race == 2 | race == 4 | race == 5 ~ 1,
                           race == 1 | race == 3 ~ 0))

reg = lm_robust(unemployed ~ negro + as.factor(gender) +
                  as.factor(higher_educ_level) +
                  as.factor(job_start) + as.factor(year_quarter) +
                  as.factor(household_location) + as.factor(job_function),
                data = merge, weights = weights)


## Criando 2ª variável dependente - Trabalhador que contribui para o INSS

inss = data %>%
  pivot_wider(id_cols = c(id_code),
              names_from = year_quarter,
              values_from = social_security_taxpayer)

inss_payer = inss %>%
  filter(`2020_1` == 1)

variables_1 = data %>%
  select(id_code, race, gender, higher_educ_level, weights,
         job_start, social_security_taxpayer, year_quarter, worker,
         household_location, job_function) %>%
  filter(!job_function == 7 & !job_function == 2)

merge_1 = left_join(inss_payer, variables_1)

merge_1 = merge_1%>%
  mutate(unemployed = case_when(social_security_taxpayer == 1 ~ 0,
                                social_security_taxpayer == 2 | worker == 2 ~ 1)) %>%
  filter(year_quarter %in% c("2020_1", "2020_2", "2020_3", "2020_4")) %>%
  mutate(negro = case_when(race == 2 | race == 4 | race == 5 ~ 1,
                           race == 1 | race == 3 ~ 0))

reg_1 = lm_robust(unemployed ~ negro + as.factor(gender) +
                  as.factor(higher_educ_level) +
                  as.factor(job_start) + as.factor(year_quarter) +
                    as.factor(household_location) + as.factor(job_function),
                data = merge_1, weights = weights)


## Criando 3ª Variável Dependente - Trabalhador com carteira assinada

card = data %>%
  pivot_wider(id_cols = c(id_code),
              names_from = year_quarter,
              values_from = signed_work_card)

card_holder = card %>%
  filter(`2020_1` == 1)

variables_2 = data %>%
  select(id_code, race, gender, higher_educ_level, weights,
         job_start, signed_work_card, year_quarter, worker,
         household_location, job_function)

merge_2 = left_join(card_holder, variables_2)

merge_2 = merge_2%>%
  mutate(unemployed = case_when(signed_work_card == 1 ~ 0,
                                signed_work_card == 2 | worker == 2 ~ 1)) %>%
  filter(year_quarter %in% c("2020_1", "2020_2", "2020_3", "2020_4")) %>%
  mutate(negro = case_when(race == 2 | race == 4 | race == 5 ~ 1,
                           race == 1 | race == 3 ~ 0))

reg_2 = lm_robust(unemployed ~ negro + as.factor(gender) +
                    as.factor(higher_educ_level) +
                    as.factor(job_start) + as.factor(year_quarter) +
                    as.factor(household_location) + as.factor(job_function),
                  data = merge_2, weights = weights)
