
## Checando número de mulheres

df = data_2019 %>%
  filter(gender == 2) %>%
  select(gender) 
#########################################

## Checando número de negros

df = data_2019 %>%
  mutate(negro = case_when(race == 2 | race == 4 | race == 5 ~ 1,
                           race == 1 | race == 3 ~ 0)) %>%
  filter(negro == 1) %>%
  select(negro)
#########################

## Descobrindo o número de trabalhadores formais pela ocupação

df = data_2019 %>%
  filter(job_function %in% c(3) & worker == 1 & signed_work_card == 1) %>%
  select(job_function, worker, signed_work_card)

df = data_2019 %>%
  filter(job_function %in% c(4) & worker == 1 & signed_work_card == 1) %>%
  select(job_function, worker, signed_work_card)

df = data_2019 %>%
  filter(job_function %in% c(5) & worker == 1 & cnpj == 1) %>%
  select(job_function, worker, social_security_taxpayer)

df = data_2019 %>%
  filter(job_function %in% c(6) & worker == 1 & social_security_taxpayer == 1) %>%
  select(job_function, worker, social_security_taxpayer)

###################################

## Descobrindo o número de trabalhadores informais pela ocupação

df = data_2019 %>%
  filter(job_function %in% c(3) & worker == 1 & signed_work_card == 2) %>%
  select(job_function, worker, signed_work_card)

df = data_2019 %>%
  filter(job_function %in% c(4) & worker == 1 & signed_work_card == 2) %>%
  select(job_function, worker, signed_work_card)

df = data_2019 %>%
  filter(job_function %in% c(5) & worker == 1 & cnpj == 2) %>%
  select(job_function, worker, social_security_taxpayer)

df = data_2019 %>%
  filter(job_function %in% c(6) & worker == 1 & social_security_taxpayer == 2) %>%
  select(job_function, worker, social_security_taxpayer)

#################################

## Descobrindo o número de inativos

df = data_2019 %>%
  filter(workforce_condition == 2) %>%
  select(workforce_condition)
##############################

## Descobrindo idade média de todos os grupos formais + inativos

# Inativos
df = data_2019 %>%
  filter(workforce_condition == 2) %>%
  select(workforce_condition, age)

a = mean(df$age)
aa = sd(df$age)

# Setor Privado
df = data_2019 %>%
  filter(job_function %in% c(3) & worker == 1 & signed_work_card == 1) %>%
  select(job_function, worker, signed_work_card, age)

b = mean(df$age)
bb = sd(df$age)

# Setor Público
df = data_2019 %>%
  filter(job_function %in% c(4) & worker == 1 & signed_work_card == 1) %>%
  select(job_function, worker, signed_work_card, age)

c = mean(df$age)
cc = sd(df$age)

# Empregadores 

df = data_2019 %>%
  filter(job_function %in% c(5) & worker == 1 & cnpj == 1) %>%
  select(job_function, worker, cnpj, age)

d = mean(df$age)
dd = sd(df$age)

# Conta Própria
df = data_2019 %>%
  filter(job_function %in% c(6) & worker == 1 & social_security_taxpayer == 1) %>%
  select(job_function, worker, social_security_taxpayer, age)

e = mean(df$age)
ee = sd(df$age)
########################################

# Descobrindo idade média de todos os grupos informais

df = data_2019 %>%
  filter(job_function %in% c(3) & worker == 1 & signed_work_card == 2) %>%
  select(job_function, worker, signed_work_card, age)

f = mean(df$age)
ff = sd(df$age)

df = data_2019 %>%
  filter(job_function %in% c(4) & worker == 1 & signed_work_card == 2) %>%
  select(job_function, worker, signed_work_card, age)

g = mean(df$age)
gg = sd(df$age)

df = data_2019 %>%
  filter(job_function %in% c(5) & worker == 1 & cnpj == 2) %>%
  select(job_function, worker, cnpj, age)

h = mean(df$age)
hh = sd(df$age)

df = data_2019 %>%
  filter(job_function %in% c(6) & worker == 1 & social_security_taxpayer == 2) %>%
  select(job_function, worker, social_security_taxpayer, age)

i = mean(df$age)
ii = sd(df$age)
##########################

## Descobrindo anos de estudo médio dos inativos e das ocupações e salário médio

# Inativos
df = data_2019 %>%
  filter(workforce_condition == 2) %>%
  select(workforce_condition, years_of_study)

j = mean(df$years_of_study)
jj = sd(df$years_of_study)

# Setor Privado Formal e Informal

df = data_2019 %>%
  filter(job_function %in% c(3) & worker == 1 & signed_work_card == 1) %>%
  select(job_function, worker, signed_work_card, years_of_study)

k = mean(df$years_of_study)
kk = sd(df$years_of_study)

df = data_2019 %>%
  filter(job_function %in% c(3) & worker == 1 & signed_work_card == 1 &
           !is.na(monthly_work_income)) %>%
  select(job_function, worker, signed_work_card, monthly_work_income)

kkk = mean(df$monthly_work_income)
kkkk = sd(df$monthly_work_income)

df = data_2019 %>%
  filter(job_function %in% c(3) & worker == 1 & signed_work_card == 2) %>%
  select(job_function, worker, signed_work_card, years_of_study)

l = mean(df$years_of_study)
ll = sd(df$years_of_study)

df = data_2019 %>%
  filter(job_function %in% c(3) & worker == 1 & signed_work_card == 2 &
           !is.na(monthly_work_income)) %>%
  select(job_function, worker, signed_work_card, monthly_work_income)

lll = mean(df$monthly_work_income)
llll = sd(df$monthly_work_income)

# Setor Público Formal e Informal

df = data_2019 %>%
  filter(job_function %in% c(4) & worker == 1 & signed_work_card == 1) %>%
  select(job_function, worker, signed_work_card, years_of_study)

m = mean(df$years_of_study)
mm = sd(df$years_of_study)

df = data_2019 %>%
  filter(job_function %in% c(4) & worker == 1 & signed_work_card == 1 &
           !is.na(monthly_work_income)) %>%
  select(job_function, worker, signed_work_card, monthly_work_income)

mmm = mean(df$monthly_work_income)
mmmm = sd(df$monthly_work_income)

df = data_2019 %>%
  filter(job_function %in% c(4) & worker == 1 & signed_work_card == 2) %>%
  select(job_function, worker, signed_work_card, years_of_study)

n = mean(df$years_of_study)
nn = sd(df$years_of_study)

df = data_2019 %>%
  filter(job_function %in% c(4) & worker == 1 & signed_work_card == 2 &
           !is.na(monthly_work_income)) %>%
  select(job_function, worker, signed_work_card, monthly_work_income)

nnn = mean(df$monthly_work_income)
nnnn = sd(df$monthly_work_income)

# Empregadores Formais e Informais

df = data_2019 %>%
  filter(job_function %in% c(5) & worker == 1 & cnpj == 1) %>%
  select(job_function, worker, cnpj, years_of_study)

o = mean(df$years_of_study)
oo = sd(df$years_of_study)

df = data_2019 %>%
  filter(job_function %in% c(5) & worker == 1 & cnpj == 1 &
           !is.na(monthly_work_income)) %>%
  select(job_function, worker, cnpj, monthly_work_income)

ooo = mean(df$monthly_work_income)
oooo = sd(df$monthly_work_income)

df = data_2019 %>%
  filter(job_function %in% c(5) & worker == 1 & cnpj == 2) %>%
  select(job_function, worker, cnpj, years_of_study)

p = mean(df$years_of_study)
pp = sd(df$years_of_study)

df = data_2019 %>%
  filter(job_function %in% c(5) & worker == 1 & cnpj == 2 &
           !is.na(monthly_work_income)) %>%
  select(job_function, worker, cnpj, monthly_work_income)

ppp = mean(df$monthly_work_income)
pppp = sd(df$monthly_work_income)

# Conta Própria Formal e Informal

df = data_2019 %>%
  filter(job_function %in% c(6) & worker == 1 & social_security_taxpayer == 1) %>%
  select(job_function, worker, social_security_taxpayer, years_of_study)

r = mean(df$years_of_study)
rr = sd(df$years_of_study)

df = data_2019 %>%
  filter(job_function %in% c(6) & worker == 1 & social_security_taxpayer == 1 &
           !is.na(monthly_work_income)) %>%
  select(job_function, worker, social_security_taxpayer, monthly_work_income)

rrr = mean(df$monthly_work_income)
rrrr = sd(df$monthly_work_income)

df = data_2019 %>%
  filter(job_function %in% c(6) & worker == 1 & social_security_taxpayer == 2) %>%
  select(job_function, worker, social_security_taxpayer, years_of_study)

s = mean(df$years_of_study)
ss = sd(df$years_of_study)

df = data_2019 %>%
  filter(job_function %in% c(6) & worker == 1 & social_security_taxpayer == 2 &
           !is.na(monthly_work_income)) %>%
  select(job_function, worker, social_security_taxpayer, monthly_work_income)

sss = mean(df$monthly_work_income)
ssss = sd(df$monthly_work_income)
######################