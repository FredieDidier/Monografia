
df = data_2019 %>%
  select(monthly_work_income, age, hours_worked, worker, social_security_taxpayer,
         signed_work_card, job_function)%>%
  filter(!is.na(monthly_work_income)) %>%
  filter(!is.na(worker)) %>%
  filter(!is.na(social_security_taxpayer)) %>%
  filter(!is.na(signed_work_card)) %>%
  filter(!is.na(age)) %>%
  filter(!is.na(hours_worked)) %>%
  filter(!job_function == 6)
## Media de salário de trabalhadores sem carteira
i = mean(df$monthly_work_income)
ii = sd(df$monthly_work_income)
iii = mean(df$age)
iiii = sd(df$age)
iiiii = mean(df$hours_worked)
iiiiii = sd(df$hours_worked)

df2 = data_2019 %>%
  select(monthly_work_income, age, hours_worked, worker,
         signed_work_card, job_function)%>%
  filter(!is.na(monthly_work_income)) %>%
  filter(!is.na(worker)) %>%
  filter(!is.na(signed_work_card)) %>%
  filter(!is.na(age)) %>%
  filter(!is.na(hours_worked)) %>%
  filter(worker == 1 & signed_work_card == 1)
## Média de salário de trabalhadores com carteira
w = mean(df2$monthly_work_income)
ww = sd(df2$monthly_work_income)
www = mean(df2$age)
wwww = sd(df2$age)
wwwww = mean(df2$hours_worked)
wwwwww = sd(df2$hours_worked)

df3 = data_2019 %>%
  select(monthly_work_income, age, hours_worked, worker, social_security_taxpayer,
   job_function)%>%
  filter(!is.na(monthly_work_income)) %>%
  filter(!is.na(worker)) %>%
  filter(!is.na(social_security_taxpayer)) %>%
  filter(!job_function == 6) %>%
  filter(!is.na(age)) %>%
  filter(!is.na(hours_worked)) %>%
  filter(social_security_taxpayer == 1 & worker == 1)
## Média de saláro de trabalhadores que contribuem pro INSS
e = mean(df3$monthly_work_income)
ee = sd(df3$monthly_work_income)
eee = mean(df3$age)
eeee = sd(df3$age)
eeeee = mean(df3$hours_worked)
eeeeee = sd(df3$hours_worked)

df4 = data_2019 %>%
  select(monthly_work_income, age, hours_worked, worker, social_security_taxpayer,
         job_function)%>%
  filter(!is.na(monthly_work_income)) %>%
  filter(!is.na(worker)) %>%
  filter(!is.na(social_security_taxpayer)) %>%
  filter(!job_function == 6) %>%
  filter(!is.na(hours_worked)) %>%
  filter(social_security_taxpayer == 2 & worker == 1)
## Média de salário de trabalhadores que não contribuem pro INSS
f = mean(df4$monthly_work_income)
ff = sd(df4$monthly_work_income)
fff = mean(df4$age)
ffff = sd(df4$age)
fffff = mean(df4$hours_worked)
ffffff = sd(df4$hours_worked)
  
df5 = data_2019 %>%
  select(monthly_work_income, age, hours_worked, worker, social_security_taxpayer,
         job_function)%>%
  filter(!is.na(monthly_work_income)) %>%
  filter(!is.na(worker)) %>%
  filter(!is.na(social_security_taxpayer)) %>%
  filter(job_function == 6) %>%
  filter(!is.na(hours_worked)) %>%
  filter(social_security_taxpayer == 1 & worker == 1)
## Média de Salário de autonõmos que contribuem pro INSS
b = mean(df5$monthly_work_income)
bb = sd(df5$monthly_work_income)
bbb = mean(df5$age)
bbbb = sd(df5$age)
bbbbb = mean(df5$hours_worked)
bbbbbb = sd(df5$hours_worked)

df6 = data_2019 %>%
  select(monthly_work_income, age, hours_worked, worker, social_security_taxpayer,
         job_function)%>%
  filter(!is.na(monthly_work_income)) %>%
  filter(!is.na(worker)) %>%
  filter(!is.na(social_security_taxpayer)) %>%
  filter(job_function == 6) %>%
  filter(!is.na(hours_worked)) %>%
  filter(social_security_taxpayer == 2 & worker == 1)

t = mean(df6$monthly_work_income)
tt = sd(df6$monthly_work_income)
ttt = mean(df6$age)
tttt = sd(df$age)
ttttt = mean(df6$hours_worked)
tttttt = sd(df6$hours_worked)

ed_years = data_2019 %>%
  select(years_of_study, worker, signed_work_card, job_function) %>%
  filter(!is.na(years_of_study)) %>%
  filter(!is.na(worker)) %>%
  filter(!is.na(signed_work_card)) %>%
  filter(worker == 1 & signed_work_card == 1)
## Média de anos de estudo cart ass
o = mean(ed_years$years_of_study)
oo = sd(ed_years$years_of_study)

ed_years_2 = data_2019 %>%
  select(years_of_study, worker, signed_work_card, job_function) %>%
  filter(!is.na(years_of_study)) %>%
  filter(!is.na(worker)) %>%
  filter(!is.na(signed_work_card)) %>%
  filter(worker == 1 & signed_work_card == 2)
# Média de anos de estudo sem cart ass
u = mean(ed_years_2$years_of_study)
uu = sd(ed_years_2$years_of_study)

ed_years_3 = data_2019 %>%
  select(years_of_study, worker, social_security_taxpayer, job_function) %>%
  filter(!is.na(years_of_study)) %>%
  filter(!is.na(worker)) %>%
  filter(!is.na(social_security_taxpayer)) %>%
  filter(!job_function == 6) %>%
  filter(worker == 1 & social_security_taxpayer == 1)
## Média de anos de estudo INSS
y = mean(ed_years_3$years_of_study)
yy = sd(ed_years_3$years_of_study)

ed_years_4 = data_2019 %>%
  select(years_of_study, worker, social_security_taxpayer, job_function) %>%
  filter(!is.na(years_of_study)) %>%
  filter(!is.na(worker)) %>%
  filter(!is.na(social_security_taxpayer)) %>%
  filter(!job_function == 6) %>%
  filter(worker == 1 & social_security_taxpayer == 2)
## Média de anos de estudo sem INSS
po = mean(ed_years_4$years_of_study)
poo = sd(ed_years_4$years_of_study)

ed_years_5 = data_2019 %>%
  select(years_of_study, worker, social_security_taxpayer,
         job_function)%>%
  filter(!is.na(worker)) %>%
  filter(!is.na(social_security_taxpayer)) %>%
  filter(!is.na(years_of_study)) %>%
  filter(job_function == 6) %>%
  filter(social_security_taxpayer == 1 & worker == 1)
## Media anos de estudo autonomos INSS
hg = mean(ed_years_5$years_of_study)
hgg = sd(ed_years_5$years_of_study)

ed_years_6 = data_2019 %>%
  select(years_of_study, worker, social_security_taxpayer,
         job_function)%>%
  filter(!is.na(worker)) %>%
  filter(!is.na(social_security_taxpayer)) %>%
  filter(!is.na(years_of_study)) %>%
  filter(job_function == 6) %>%
  filter(social_security_taxpayer == 2 & worker == 1)

rt = mean(ed_years_6$years_of_study)
rtt = sd(ed_years_6$years_of_study)

#############################

df = data_2019 %>%
select(monthly_work_income, years_of_study, age, hours_worked, job_function, worker, signed_work_card) %>%
  filter(worker == 1 & job_function == 4, signed_work_card == 1) %>%
  filter(!is.na(signed_work_card)) %>%
  filter(!is.na(years_of_study)) %>%
  filter(!is.na(hours_worked)) %>%
 mutate(i = case_when(job_function == 4 & worker == 1 & signed_work_card == 1 ~ 1,
                      job_function == 4 & worker == 1 & signed_work_card == 2 ~ 0))

ui = mean(df$i, na.rm = T)
uui = sd(df$i, na.rm = T)
uuui = mean(df$monthly_work_income)
uuuui = sd(df$monthly_work_income)
fg = mean(df$years_of_study)
fgg = sd(df$years_of_study)
ty = mean(df$age)
tyy = sd(df$age)
er = mean(df$hours_worked)
err = sd(df$hours_worked)


df = data_2019 %>%
  select(monthly_work_income, years_of_study, age, hours_worked, job_function, worker, signed_work_card) %>%
  filter(worker == 1 & job_function == 4, signed_work_card == 2) %>%
  filter(!is.na(signed_work_card)) %>%
  filter(!is.na(years_of_study)) %>%
  filter(!is.na(hours_worked)) %>%
  mutate(i = case_when(job_function == 4 & worker == 1 & signed_work_card == 2 ~ 1,
                       job_function == 4 & worker == 1 & signed_work_card == 1 ~ 0))

ui = mean(df$i, na.rm = T)
uui = sd(df$i, na.rm = T)
uuui = mean(df$monthly_work_income)
uuuui = sd(df$monthly_work_income)
fg = mean(df$years_of_study)
fgg = sd(df$years_of_study)
ty = mean(df$age)
tyy = sd(df$age)
er = mean(df$hours_worked)
err = sd(df$hours_worked)


df = data_2019 %>%
  select(monthly_work_income, years_of_study, age, hours_worked, job_function, worker, signed_work_card,
         social_security_taxpayer) %>%
  filter(worker == 1 & job_function == 4, signed_work_card == 2 & social_security_taxpayer == 1) %>%
  filter(!is.na(signed_work_card)) %>%
  filter(!is.na(years_of_study)) %>%
  filter(!is.na(hours_worked)) %>%
  mutate(i = case_when(job_function == 4 & worker == 1 & signed_work_card == 2 ~ 1,
                       job_function == 4 & worker == 1 & signed_work_card == 1 ~ 0))

ui = mean(df$i, na.rm = T)
uui = sd(df$i, na.rm = T)
uuui = mean(df$monthly_work_income)
uuuui = sd(df$monthly_work_income)
fg = mean(df$years_of_study)
fgg = sd(df$years_of_study)
ty = mean(df$age)
tyy = sd(df$age)
er = mean(df$hours_worked)
err = sd(df$hours_worked)


df = data_2019 %>%
  select(monthly_work_income, years_of_study, age, hours_worked, job_function, worker, signed_work_card,
         social_security_taxpayer) %>%
  filter(worker == 1 & job_function == 4, signed_work_card == 2 & social_security_taxpayer == 2) %>%
  filter(!is.na(signed_work_card)) %>%
  filter(!is.na(years_of_study)) %>%
  filter(!is.na(hours_worked)) %>%
  mutate(i = case_when(job_function == 4 & worker == 1 & signed_work_card == 2 ~ 1,
                       job_function == 4 & worker == 1 & signed_work_card == 1 ~ 0))

ui = mean(df$i, na.rm = T)
uui = sd(df$i, na.rm = T)
uuui = mean(df$monthly_work_income)
uuuui = sd(df$monthly_work_income)
fg = mean(df$years_of_study)
fgg = sd(df$years_of_study)
ty = mean(df$age)
tyy = sd(df$age)
er = mean(df$hours_worked)
err = sd(df$hours_worked)


df = data_2019 %>%
  select(monthly_work_income, years_of_study, age, hours_worked, job_function, worker,
         social_security_taxpayer) %>%
  filter(worker == 1 & job_function == 5 & social_security_taxpayer == 1) %>%
  filter(!is.na(years_of_study)) %>%
  filter(!is.na(hours_worked))

ui = mean(df$i, na.rm = T)
uui = sd(df$i, na.rm = T)
uuui = mean(df$monthly_work_income, na.rm = T)
uuuui = sd(df$monthly_work_income, na.rm = T)
fg = mean(df$years_of_study)
fgg = sd(df$years_of_study)
ty = mean(df$age)
tyy = sd(df$age)
er = mean(df$hours_worked)
err = sd(df$hours_worked)


df = data_2019 %>%
  select(monthly_work_income, years_of_study, age, hours_worked, job_function, worker,
         social_security_taxpayer) %>%
  filter(worker == 1 & job_function == 5 & social_security_taxpayer == 2) %>%
  filter(!is.na(years_of_study)) %>%
  filter(!is.na(hours_worked))

ui = mean(df$i, na.rm = T)
uui = sd(df$i, na.rm = T)
uuui = mean(df$monthly_work_income, na.rm = T)
uuuui = sd(df$monthly_work_income, na.rm = T)
fg = mean(df$years_of_study)
fgg = sd(df$years_of_study)
ty = mean(df$age)
tyy = sd(df$age)
er = mean(df$hours_worked)
err = sd(df$hours_worked)
#######################


df = data_2019 %>%
  filter(workforce_condition == 2) %>%
  select(workforce_condition, age, years_of_study)%>%
  filter(!is.na(years_of_study))

m = mean(df$age)
mm = sd(df$age)
mmm = mean(df$years_of_study)
mmmm = sd(df$years_of_study)

###############

df = data_2019 %>%
  mutate(negro = case_when(race == 2 | race == 4 | race == 5 ~ 1,
                           race == 1 | race == 3 ~ 0)) %>%
  mutate(branco = case_when(race == 2 | race == 4 | race == 5 ~ 0,
                           race == 1 | race == 3 ~ 1))

p = mean(df$negro, na.rm = T)
pp = sd(df$negro, na.rm = T)
ppp = mean(df$branco, na.rm = T)
pppp = sd(df$branco, na.rm = T)

###############

df = data_2019 %>%
  mutate(negro = case_when(race == 2 | race == 4 | race == 5 ~ 1,
                           race == 1 | race == 3 ~ 0)) %>%
  filter(worker == 1, signed_work_card == 1, negro == 1) %>%
  select(monthly_work_income, age, hours_worked, worker, social_security_taxpayer,
         signed_work_card, negro, years_of_study)

h = mean(df$monthly_work_income)
hh = sd(df$monthly_work_income)
hhh = mean(df$years_of_study)
hhhh = sd(df$years_of_study)
hhhhh = mean(df$age)
hhhhhh = sd(df$age)
s = mean(df$hours_worked)
ss = sd(df$hours_worked)


df = data_2019 %>%
  mutate(negro = case_when(race == 2 | race == 4 | race == 5 ~ 1,
                           race == 1 | race == 3 ~ 0)) %>%
  filter(worker == 1, signed_work_card == 1, negro == 0) %>%
  select(monthly_work_income, age, hours_worked, worker, social_security_taxpayer,
         signed_work_card, negro, years_of_study)

h = mean(df$monthly_work_income)
hh = sd(df$monthly_work_income)
hhh = mean(df$years_of_study)
hhhh = sd(df$years_of_study)
hhhhh = mean(df$age)
hhhhhh = sd(df$age)
s = mean(df$hours_worked)
ss = sd(df$hours_worked)


##############

df = data_2019 %>%
  select(worker, job_function, social_security_taxpayer,
         signed_work_card) %>%
  filter(worker == 1 & job_function == 4) %>%
  mutate(reg_pub_sec = case_when(job_function == 4 & worker == 1 &
                                   signed_work_card == 1 ~ 1,
                                 job_function == 4 & worker == 1 &
                                   signed_work_card == 2 ~ 0)) %>%
  mutate(no_reg_pub_sec = case_when(job_function == 4 & worker == 1 &
                                   signed_work_card == 1 ~ 0,
                                 job_function == 4 & worker == 1 &
                                   signed_work_card == 2 ~ 1)) %>%
  mutate(inss_pub_sec = case_when(job_function == 4 & worker == 1 &
                                   social_security_taxpayer == 1 ~ 1,
                                 job_function == 4 & worker == 1 &
                                   social_security_taxpayer == 2 ~ 0)) %>%
  mutate(no_inss_pub_sec = case_when(job_function == 4 & worker == 1 &
                                    social_security_taxpayer == 1 ~ 0,
                                  job_function == 4 & worker == 1 &
                                    social_security_taxpayer == 2 ~ 1))
m = mean(df$reg_pub_sec, na.rm = T)
mm = sd(df$reg_pub_sec, na.rm = T)
mmm = mean(df$no_reg_pub_sec, na.rm = T)
mmmm = sd(df$no_reg_pub_sec, na.rm = T)
a = mean(df$inss_pub_sec, na.rm = T)
aa = sd(df$inss_pub_sec, na.rm = T)

########
df = data_2019 %>%
  select(worker, job_function, social_security_taxpayer,
         signed_work_card) %>%
  filter(worker == 1 & job_function == 5) %>%
  mutate(inss_empl = case_when(job_function == 5 & worker == 1 &
                                    social_security_taxpayer == 1 ~ 1,
                                  job_function == 5 & worker == 1 &
                                    social_security_taxpayer == 2 ~ 0)) %>%
  mutate(no_inss_empl = case_when(job_function == 5 & worker == 1 &
                                       social_security_taxpayer == 1 ~ 0,
                                     job_function == 5 & worker == 1 &
                                       social_security_taxpayer == 2 ~ 1))

r = mean(df$inss_empl)
rr = sd(df$inss_empl)

#########
df = data_2019 %>%
  mutate(negro = case_when(race == 2 | race == 4 | race == 5 ~ 1,
                           race == 1 | race == 3 ~ 0)) %>%
  mutate(negro_work = case_when(negro == 1 & worker == 1 ~ 1,
                                negro == 1 & worker == 2 & workforce_condition == 1 ~ 0)) %>%
  mutate(negro_no_work = case_when(negro == 1 & worker == 1 ~ 0,
                                negro == 1 & worker == 2 & workforce_condition == 1 ~ 1)) %>%
  mutate(white_work = case_when(negro == 0 & worker == 1 ~ 1,
                                negro == 0 & worker == 2 & workforce_condition == 1 ~ 0)) %>%
  mutate(white_no_work = case_when(negro == 0 & worker == 1 ~ 0,
                                negro == 0 & worker == 2 & workforce_condition == 1 ~ 1)) %>%
  select(white_work, white_no_work,negro,negro_work, negro_no_work, worker, workforce_condition)
  


mi = mean(df$negro_no_work, na.rm = T)
mii = sd(df$negro_no_work, na.rm = T)
miii = mean(df$negro_work, na.rm = T)
miiii = sd(df$negro_work, na.rm = T)

t = mean(df$white_work, na.rm = T)
tt = sd(df$white_work, na.rm = T)
ttt = mean(df$white_no_work, na.rm = T)
ttt = sd(df$white_no_work, na.rm = T)
