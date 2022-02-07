
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
