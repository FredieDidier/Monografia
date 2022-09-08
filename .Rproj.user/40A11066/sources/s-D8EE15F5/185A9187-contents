library(tidyverse)
library(estimatr)
library(modelsummary)
library(extrafont)

painel = readr::read_rds("./input/painel_2019-2021.rds")

data = painel

###########################################################

## Criando Variavel Dependente - Trabalhador Conta Propria que contribui pro INSS

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
         social_security_taxpayer, sector_code, higher_educ_course_attended,
         work_category)

merge = left_join(worker, variables)

merge = merge %>%
  mutate(unemployed = case_when(worker == 1 & work_category == 1 &
                                  job_function == 3 ~ 0,
                                worker == 2 & work_category == 2 &
                                  job_function  == 3 ~ 1)) %>%
  filter(year_quarter %in% c("2020_1", "2020_2", "2020_3", "2020_4")) %>%
  mutate(negro = case_when(race == 2 | race == 4 | race == 5 ~ 1,
                           race == 1 | race == 3 ~ 0))

reg = lm_robust(unemployed ~ negro + as.factor(gender) +
                  as.factor(higher_educ_level) +
                  as.factor(job_start) + as.factor(year_quarter) +
                  as.factor(household_location) + as.factor(sector_code),
                data = merge, weights = weights)


