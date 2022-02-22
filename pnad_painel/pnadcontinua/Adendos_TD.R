
## Checando número de mulheres

dat = data_2019 %>%
  filter(gender == 2) %>%
  select(id_code, gender, weights) 
#########################################

## Checando número de negros

dat = data_2019 %>%
  mutate(negro = case_when(race == 2 | race == 4 | race == 5 ~ 1,
                           race == 1 | race == 3 ~ 0)) %>%
  filter(negro == 1) %>%
  select(negro)
#########################

## Descobrindo o número de trabalhadores formais pela ocupação

dat = data_2019 %>%
  filter(job_function %in% c(3) & worker == 1 & signed_work_card == 1) %>%
  select(job_function, worker, signed_work_card)

dat = data_2019 %>%
  filter(job_function %in% c(4) & worker == 1 & signed_work_card == 1) %>%
  select(job_function, worker, signed_work_card)

dat = data_2019 %>%
  filter(job_function %in% c(5) & worker == 1 & cnpj == 1) %>%
  select(job_function, worker, social_security_taxpayer)

dat = data_2019 %>%
  filter(job_function %in% c(6) & worker == 1 & social_security_taxpayer == 1) %>%
  select(job_function, worker, social_security_taxpayer)

###################################

## Descobrindo o número de trabalhadores informais pela ocupação

dat = data_2019 %>%
  filter(job_function %in% c(3) & worker == 1 & signed_work_card == 2) %>%
  select(job_function, worker, signed_work_card)

dat = data_2019 %>%
  filter(job_function %in% c(4) & worker == 1 & signed_work_card == 2) %>%
  select(job_function, worker, signed_work_card)

dat = data_2019 %>%
  filter(job_function %in% c(5) & worker == 1 & cnpj == 2) %>%
  select(job_function, worker, social_security_taxpayer)

dat = data_2019 %>%
  filter(job_function %in% c(6) & worker == 1 & social_security_taxpayer == 2) %>%
  select(job_function, worker, social_security_taxpayer)

#################################

## Descobrindo o número de inativos

dat = data_2019 %>%
  filter(workforce_condition == 2) %>%
  select(workforce_condition)
##############################

## Descobrindo idade média de todos os grupos formais + inativos

# Inativos
dat = data_2019 %>%
  filter(workforce_condition == 2) %>%
  select(workforce_condition, age)

a = mean(dat$age)
aa = sd(dat$age)

# Setor Privado
dat = data_2019 %>%
  filter(job_function %in% c(3) & worker == 1 & signed_work_card == 1) %>%
  select(job_function, worker, signed_work_card, age)

b = mean(dat$age)
bb = sd(dat$age)

# Setor Público
dat = data_2019 %>%
  filter(job_function %in% c(4) & worker == 1 & signed_work_card == 1) %>%
  select(job_function, worker, signed_work_card, age)

c = mean(dat$age)
cc = sd(dat$age)

# Empregadores 

dat = data_2019 %>%
  filter(job_function %in% c(5) & worker == 1 & cnpj == 1) %>%
  select(job_function, worker, cnpj, age)

d = mean(dat$age)
dd = sd(dat$age)

# Conta Própria
dat = data_2019 %>%
  filter(job_function %in% c(6) & worker == 1 & social_security_taxpayer == 1) %>%
  select(job_function, worker, social_security_taxpayer, age)

e = mean(dat$age)
ee = sd(dat$age)
########################################

# Descobrindo idade média de todos os grupos informais

dat = data_2019 %>%
  filter(job_function %in% c(3) & worker == 1 & signed_work_card == 2) %>%
  select(job_function, worker, signed_work_card, age)

f = mean(dat$age)
ff = sd(dat$age)

dat = data_2019 %>%
  filter(job_function %in% c(4) & worker == 1 & signed_work_card == 2) %>%
  select(job_function, worker, signed_work_card, age)

g = mean(dat$age)
gg = sd(dat$age)

dat = data_2019 %>%
  filter(job_function %in% c(5) & worker == 1 & cnpj == 2) %>%
  select(job_function, worker, cnpj, age)

h = mean(dat$age)
hh = sd(dat$age)

dat = data_2019 %>%
  filter(job_function %in% c(6) & worker == 1 & social_security_taxpayer == 2) %>%
  select(job_function, worker, social_security_taxpayer, age)

i = mean(dat$age)
ii = sd(dat$age)
##########################

## Descobrindo anos de estudo médio dos inativos e das ocupações e salário médio

# Inativos
dat = data_2019 %>%
  filter(workforce_condition == 2) %>%
  select(workforce_condition, years_of_study)

j = mean(dat$years_of_study)
jj = sd(dat$years_of_study)

# Setor Privado Formal e Informal

dat = data_2019 %>%
  filter(job_function %in% c(3) & worker == 1 & signed_work_card == 1) %>%
  select(job_function, worker, signed_work_card, years_of_study)

k = mean(dat$years_of_study)
kk = sd(dat$years_of_study)

dat = data_2019 %>%
  filter(job_function %in% c(3) & worker == 1 & signed_work_card == 1 &
           !is.na(monthly_work_income)) %>%
  select(job_function, worker, signed_work_card, monthly_work_income)

kkk = mean(dat$monthly_work_income)
kkkk = sd(dat$monthly_work_income)

dat = data_2019 %>%
  filter(job_function %in% c(3) & worker == 1 & signed_work_card == 2) %>%
  select(job_function, worker, signed_work_card, years_of_study)

l = mean(dat$years_of_study)
ll = sd(dat$years_of_study)

dat = data_2019 %>%
  filter(job_function %in% c(3) & worker == 1 & signed_work_card == 2 &
           !is.na(monthly_work_income)) %>%
  select(job_function, worker, signed_work_card, monthly_work_income)

lll = mean(dat$monthly_work_income)
llll = sd(dat$monthly_work_income)

# Setor Público Formal e Informal

dat = data_2019 %>%
  filter(job_function %in% c(4) & worker == 1 & signed_work_card == 1) %>%
  select(job_function, worker, signed_work_card, years_of_study)

m = mean(dat$years_of_study)
mm = sd(dat$years_of_study)

dat = data_2019 %>%
  filter(job_function %in% c(4) & worker == 1 & signed_work_card == 1 &
           !is.na(monthly_work_income)) %>%
  select(job_function, worker, signed_work_card, monthly_work_income)

mmm = mean(dat$monthly_work_income)
mmmm = sd(dat$monthly_work_income)

dat = data_2019 %>%
  filter(job_function %in% c(4) & worker == 1 & signed_work_card == 2) %>%
  select(job_function, worker, signed_work_card, years_of_study)

n = mean(dat$years_of_study)
nn = sd(dat$years_of_study)

dat = data_2019 %>%
  filter(job_function %in% c(4) & worker == 1 & signed_work_card == 2 &
           !is.na(monthly_work_income)) %>%
  select(job_function, worker, signed_work_card, monthly_work_income)

nnn = mean(dat$monthly_work_income)
nnnn = sd(dat$monthly_work_income)

# Empregadores Formais e Informais

dat = data_2019 %>%
  filter(job_function %in% c(5) & worker == 1 & cnpj == 1) %>%
  select(job_function, worker, cnpj, years_of_study)

o = mean(dat$years_of_study)
oo = sd(dat$years_of_study)

dat = data_2019 %>%
  filter(job_function %in% c(5) & worker == 1 & cnpj == 1 &
           !is.na(monthly_work_income)) %>%
  select(job_function, worker, cnpj, monthly_work_income)

ooo = mean(dat$monthly_work_income)
oooo = sd(dat$monthly_work_income)

dat = data_2019 %>%
  filter(job_function %in% c(5) & worker == 1 & cnpj == 2) %>%
  select(job_function, worker, cnpj, years_of_study)

p = mean(dat$years_of_study)
pp = sd(dat$years_of_study)

dat = data_2019 %>%
  filter(job_function %in% c(5) & worker == 1 & cnpj == 2 &
           !is.na(monthly_work_income)) %>%
  select(job_function, worker, cnpj, monthly_work_income)

ppp = mean(dat$monthly_work_income)
pppp = sd(dat$monthly_work_income)

# Conta Própria Formal e Informal

dat = data_2019 %>%
  filter(job_function %in% c(6) & worker == 1 & social_security_taxpayer == 1) %>%
  select(job_function, worker, social_security_taxpayer, years_of_study)

r = mean(dat$years_of_study)
rr = sd(dat$years_of_study)

dat = data_2019 %>%
  filter(job_function %in% c(6) & worker == 1 & social_security_taxpayer == 1 &
           !is.na(monthly_work_income)) %>%
  select(job_function, worker, social_security_taxpayer, monthly_work_income)

rrr = mean(dat$monthly_work_income)
rrrr = sd(dat$monthly_work_income)

dat = data_2019 %>%
  filter(job_function %in% c(6) & worker == 1 & social_security_taxpayer == 2) %>%
  select(job_function, worker, social_security_taxpayer, years_of_study)

s = mean(dat$years_of_study)
ss = sd(dat$years_of_study)

dat = data_2019 %>%
  filter(job_function %in% c(6) & worker == 1 & social_security_taxpayer == 2 &
           !is.na(monthly_work_income)) %>%
  select(job_function, worker, social_security_taxpayer, monthly_work_income)

sss = mean(dat$monthly_work_income)
ssss = sd(dat$monthly_work_income)
######################