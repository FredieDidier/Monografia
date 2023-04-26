library(tidyverse)

df1 = readr::read_rds("./input/trimestre_2019_1.rds")
df2 = readr::read_rds("./input/trimestre_2019_2.rds")
df3 = readr::read_rds("./input/trimestre_2019_3.rds")
df4 = readr::read_rds("./input/trimestre_2019_4.rds")

df = bind_rows(df1, df2, df3, df4)

# Numero de Inativos e da força de trabalho

dat_num_inat = df %>%
  mutate(laborforce = case_when(workforce_condition == 1 ~ 1,
                                TRUE ~ 0)) %>%
  mutate(inactive = case_when(workforce_condition == 2 ~ 1,
                              TRUE ~ 0)) %>%
  group_by(year_quarter) %>%
  summarise(laborforce = sum(laborforce * weights), inactive = sum(inactive * weights), weights = sum(weights)) %>%
  summarise(across(c(laborforce, inactive), ~ sum(. * weights)/sum(weights)))

## Checando numero de mulheres e homens

dat_num_m_f = df %>%
  mutate(male = case_when(gender == 1 ~ 1,
                          TRUE ~ 0)) %>%
  mutate(female = case_when(gender == 2 ~ 1,
                            TRUE ~ 0)) %>%
  group_by(year_quarter) %>%
  summarise(male = sum(male * weights), female = sum(female * weights), weights = sum(weights)) %>%
  summarise(across(c(male, female), ~ sum(. * weights)/sum(weights)))
#########################################

## Checando numero de negros e brancos

dat_num_b_n = df %>%
  mutate(negro = case_when(race == 2 | race == 4 | race == 5 ~ 1,
                           TRUE ~ 0)) %>%
  mutate(branco = case_when(race == 2 | race == 4 | race == 5 ~ 0,
                            TRUE ~ 1)) %>%
  group_by(year_quarter) %>%
  summarise(negro = sum(negro * weights), branco = sum(branco * weights), weights = sum(weights)) %>%
  summarise(across(c(negro, branco), ~ sum(. * weights)/sum(weights)))
#########################

## Descobrindo o numero de trabalhadores formais pela ocupacao

dat_num_formal_priv = df %>%
  filter(job_function %in% c(3) & worker == 1 & work_category == 1) %>%
  select(job_function, worker, signed_work_card, year_quarter, weights,
         work_category) %>%
  group_by(year_quarter) %>%
  summarise(peso = sum(worker * weights), weights = sum(weights)) %>%
  summarise(across(c(peso), ~ sum(. * weights)/sum(weights)))

dat_num_formal_public = df %>%
  filter(job_function %in% c(4) & worker == 1 & work_category %in% c(5,7)) %>%
  select(job_function, worker, signed_work_card, year_quarter, weights,
         work_category)  %>%
  group_by(year_quarter) %>%
  summarise(peso = sum(worker * weights), weights = sum(weights)) %>%
  summarise(across(c(peso), ~ sum(. * weights)/sum(weights)))

dat_num_formal_employer = df %>%
  filter(job_function %in% c(5) & worker == 1 & cnpj == 1) %>%
  select(job_function, worker, social_security_taxpayer, year_quarter, weights) %>%
  group_by(year_quarter) %>%
  summarise(peso = sum(worker * weights), weights = sum(weights)) %>%
  summarise(across(c(peso), ~ sum(. * weights)/sum(weights)))

dat_num_formal_self_employed = df %>%
  filter(job_function %in% c(6) & worker == 1 & social_security_taxpayer == 1) %>%
  select(job_function, worker, social_security_taxpayer, year_quarter, weights) %>%
  group_by(year_quarter) %>%
  summarise(peso = sum(worker * weights), weights = sum(weights)) %>%
  summarise(across(c(peso), ~ sum(. * weights)/sum(weights)))

###################################

## Descobrindo o n?mero de trabalhadores informais pela ocupa??o

dat_num_informal_priv = df %>%
  filter(job_function %in% c(3) & worker == 1 & work_category == 2) %>%
  select(job_function, worker, signed_work_card, year_quarter, weights,
         work_category) %>%
  group_by(year_quarter) %>%
  summarise(peso = sum(worker * weights), weights = sum(weights)) %>%
  summarise(across(c(peso), ~ sum(. * weights)/sum(weights)))

dat_num_informal_public = df %>%
  filter(job_function %in% c(4) & worker == 1 & work_category == 6) %>%
  select(job_function, worker, signed_work_card, year_quarter, weights,
         work_category) %>%
  group_by(year_quarter) %>%
  summarise(peso = sum(worker * weights), weights = sum(weights)) %>%
  summarise(across(c(peso), ~ sum(. * weights)/sum(weights)))

dat_num_informal_employer = df %>%
  filter(job_function %in% c(5) & worker == 1 & cnpj == 2) %>%
  select(job_function, worker, social_security_taxpayer, year_quarter, weights) %>%
  group_by(year_quarter) %>%
  summarise(peso = sum(worker * weights), weights = sum(weights)) %>%
  summarise(across(c(peso), ~ sum(. * weights)/sum(weights)))

dat_num_informal_self_employed = df %>%
  filter(job_function %in% c(6) & worker == 1 & social_security_taxpayer == 2) %>%
  select(job_function, worker, social_security_taxpayer, year_quarter, weights) %>%
  group_by(year_quarter) %>%
  summarise(peso = sum(worker * weights), weights = sum(weights)) %>%
  summarise(across(c(peso), ~ sum(. * weights)/sum(weights)))

#################################

## Descobrindo anos de estudo medio dos inativos e das ocupacoes e salario medio

# Inativos

dat_estudo_inat = df %>%
  filter(workforce_condition == 2) %>%
  select(workforce_condition, years_of_study, weights) %>%
  mutate(peso_estudo = wtd.mean(years_of_study, weights = weights)) %>%
  mutate(var = wtd.var(years_of_study, weights = weights)) %>%
  mutate(sd = sqrt(var)) %>%
  summarise(peso_estudo, sd)

# Setor Privado Formal e Informal

dat_estudo_priv_formal = df %>%
  filter(job_function %in% c(3) & worker == 1 & work_category == 1) %>%
  select(job_function, worker, work_category, years_of_study, weights) %>%
  mutate(peso_estudo = wtd.mean(years_of_study, weights = weights)) %>%
  mutate(var = wtd.var(years_of_study, weights = weights)) %>%
  mutate(sd = sqrt(var)) %>%
  summarise(peso_estudo, sd)

dat_salario_formal_priv = df %>%
  filter(job_function %in% c(3) & worker == 1 & work_category == 1 &
           !is.na(monthly_work_income)) %>%
  select(job_function, worker, work_category, monthly_work_income, weights) %>%
  mutate(peso_salario = wtd.mean(monthly_work_income, weights = weights)) %>%
  mutate(var = wtd.var(monthly_work_income, weights = weights)) %>%
  mutate(sd = sqrt(var)) %>%
  summarise(peso_salario, sd)

dat_estudo_priv_informal = df %>%
  filter(job_function %in% c(3) & worker == 1 & work_category == 2) %>%
  select(job_function, worker, work_category, years_of_study, weights) %>%
  mutate(peso_estudo = wtd.mean(years_of_study, weights = weights)) %>%
  mutate(var = wtd.var(years_of_study, weights = weights)) %>%
  mutate(sd = sqrt(var)) %>%
  summarise(peso_estudo, sd)

dat_salario_informal_priv = df %>%
  filter(job_function %in% c(3) & worker == 1 & work_category == 2 &
           !is.na(monthly_work_income)) %>%
  select(job_function, worker, work_category, monthly_work_income, weights) %>%
  mutate(peso_salario = wtd.mean(monthly_work_income, weights = weights)) %>%
  mutate(var = wtd.var(monthly_work_income, weights = weights)) %>%
  mutate(sd = sqrt(var)) %>%
  summarise(peso_salario, sd)

# Setor Publico Formal e Informal

dat_estudo_public_formal = df %>%
  filter(job_function %in% c(4) & worker == 1 & work_category %in% c(5,7)) %>%
  select(job_function, worker, work_category, years_of_study, weights) %>%
  mutate(peso_estudo = wtd.mean(years_of_study, weights = weights)) %>%
  mutate(var = wtd.var(years_of_study, weights = weights)) %>%
  mutate(sd = sqrt(var)) %>%
  summarise(peso_estudo, sd)

dat_salario_formal_public = df %>%
  filter(job_function %in% c(4) & worker == 1 & work_category %in% c(5,7) &
           !is.na(monthly_work_income)) %>%
  select(job_function, worker, work_category, monthly_work_income, weights) %>%
  mutate(peso_salario = wtd.mean(monthly_work_income, weights = weights)) %>%
  mutate(var = wtd.var(monthly_work_income, weights = weights)) %>%
  mutate(sd = sqrt(var)) %>%
  summarise(peso_salario, sd)

dat_estudo_public_informal = df %>%
  filter(job_function %in% c(4) & worker == 1 & work_category == 6) %>%
  select(job_function, worker, work_category, years_of_study, weights) %>%
  mutate(peso_estudo = wtd.mean(years_of_study, weights = weights)) %>%
  mutate(var = wtd.var(years_of_study, weights = weights)) %>%
  mutate(sd = sqrt(var)) %>%
  summarise(peso_estudo, sd)

dat_salario_informal_public = df %>%
  filter(job_function %in% c(4) & worker == 1 & work_category == 6 &
           !is.na(monthly_work_income)) %>%
  select(job_function, worker, work_category, monthly_work_income, weights) %>%
  mutate(peso_salario = wtd.mean(monthly_work_income, weights = weights)) %>%
  mutate(var = wtd.var(monthly_work_income, weights = weights)) %>%
  mutate(sd = sqrt(var)) %>%
  summarise(peso_salario, sd)

# Empregadores Formais e Informais

dat_estudo_empreg_formal = df %>%
  filter(job_function %in% c(5) & worker == 1 & cnpj == 1) %>%
  select(job_function, worker, cnpj, years_of_study, weights) %>%
  mutate(peso_estudo = wtd.mean(years_of_study, weights = weights)) %>%
  mutate(var = wtd.var(years_of_study, weights = weights)) %>%
  mutate(sd = sqrt(var)) %>%
  summarise(peso_estudo, sd)

dat_salario_formal_empreg = df %>%
  filter(job_function %in% c(5) & worker == 1 & cnpj == 1 &
           !is.na(monthly_work_income)) %>%
  select(job_function, worker, cnpj, monthly_work_income, weights) %>%
  mutate(peso_salario = wtd.mean(monthly_work_income, weights = weights)) %>%
  mutate(var = wtd.var(monthly_work_income, weights = weights)) %>%
  mutate(sd = sqrt(var)) %>%
  summarise(peso_salario, sd)

dat_estudo_empreg_informal = df %>%
  filter(job_function %in% c(5) & worker == 1 & cnpj == 2) %>%
  select(job_function, worker, cnpj, years_of_study, weights) %>%
  mutate(peso_estudo = wtd.mean(years_of_study, weights = weights)) %>%
  mutate(var = wtd.var(years_of_study, weights = weights)) %>%
  mutate(sd = sqrt(var)) %>%
  summarise(peso_estudo, sd)


dat_salario_informal_empreg = df %>%
  filter(job_function %in% c(5) & worker == 1 & cnpj == 2 &
           !is.na(monthly_work_income)) %>%
  select(job_function, worker, cnpj, monthly_work_income, weights) %>%
  mutate(peso_salario = wtd.mean(monthly_work_income, weights = weights)) %>%
  mutate(var = wtd.var(monthly_work_income, weights = weights)) %>%
  mutate(sd = sqrt(var)) %>%
  summarise(peso_salario, sd)

# Conta Propria Formal e Informal

dat_estudo_self_employed_formal = df %>%
  filter(job_function %in% c(6) & worker == 1 & social_security_taxpayer == 1) %>%
  select(job_function, worker, social_security_taxpayer, years_of_study,weights) %>%
  mutate(peso_estudo = wtd.mean(years_of_study, weights = weights)) %>%
  mutate(var = wtd.var(years_of_study, weights = weights)) %>%
  mutate(sd = sqrt(var)) %>%
  summarise(peso_estudo, sd)

dat_salario_formal_self_employed = df %>%
  filter(job_function %in% c(6) & worker == 1 & social_security_taxpayer == 1 &
           !is.na(monthly_work_income)) %>%
  select(job_function, worker, social_security_taxpayer, monthly_work_income, weights) %>%
  mutate(peso_salario = wtd.mean(monthly_work_income, weights = weights)) %>%
  mutate(var = wtd.var(monthly_work_income, weights = weights)) %>%
  mutate(sd = sqrt(var)) %>%
  summarise(peso_salario, sd)


dat_estudo_self_employed_informal = df %>%
  filter(job_function %in% c(6) & worker == 1 & social_security_taxpayer == 2) %>%
  select(job_function, worker, social_security_taxpayer, years_of_study,weights) %>%
  mutate(peso_estudo = wtd.mean(years_of_study, weights = weights)) %>%
  mutate(var = wtd.var(years_of_study, weights = weights)) %>%
  mutate(sd = sqrt(var)) %>%
  summarise(peso_estudo, sd)


dat_salario_informal_self_employed = df %>%
  filter(job_function %in% c(6) & worker == 1 & social_security_taxpayer == 2 &
           !is.na(monthly_work_income)) %>%
  select(job_function, worker, social_security_taxpayer, monthly_work_income, weights = weights) %>%
  mutate(peso_salario = wtd.mean(monthly_work_income, weights = weights)) %>%
  mutate(var = wtd.var(monthly_work_income, weights = weights)) %>%
  mutate(sd = sqrt(var)) %>%
  summarise(peso_salario, sd)

######################