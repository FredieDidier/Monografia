library(extrafont)

 ## Fazendo Gráfico - trabalhadores com\sem carteira assinada + escolaridade

f = data %>%
  filter(year_quarter %in% c("2019_1", "2019_2", "2019_3", "2019_4") &
           signed_work_card == 1) %>%
  select(signed_work_card, higher_educ_level, year_quarter) %>%
  mutate(higher_educ_label = case_when(higher_educ_level %in% c(1) ~"Uneducated",
            higher_educ_level %in% c(2) ~ "Primary School Incompleted",
            higher_educ_level %in% c(3) ~ "Primary School Completed",
            higher_educ_level %in% c(4) ~ "High School Incompleted",
            higher_educ_level %in% c(5) ~ "High School Completed",
            higher_educ_level %in% c(6) ~ "College Degree Incompleted",
            higher_educ_level %in% c(7) ~ "College Degree Completed")) 

 ggplot(f, aes(x = year_quarter, y = signed_work_card,
    fill = factor(higher_educ_label))) +
  geom_bar(stat = "identity") +
   scale_fill_manual(name = "Education Level", 
                     values = c("skyblue", "tomato3",
                                "grey50",
                                "seagreen3",
                                "darkblue",
                                "orange",
                                "darkgreen")) +
   labs(x =  "Quarter", y = "Formal Employee") +
   theme_minimal()
 
 
 f1 = data %>%
   filter(year_quarter %in% c("2019_1", "2019_2", "2019_3", "2019_4") &
            signed_work_card == 2) %>%
   select(signed_work_card, higher_educ_level, year_quarter) %>%
   mutate(higher_educ_label = case_when(higher_educ_level %in% c(1) ~"Uneducated",
                                        higher_educ_level %in% c(2) ~ "Primary School Incompleted",
                                        higher_educ_level %in% c(3) ~ "Primary School Completed",
                                        higher_educ_level %in% c(4) ~ "High School Incompleted",
                                        higher_educ_level %in% c(5) ~ "High School Completed",
                                        higher_educ_level %in% c(6) ~ "College Degree Incompleted",
                                        higher_educ_level %in% c(7) ~ "College Degree Completed")) 
 
 ggplot(f1, aes(x = year_quarter, y = signed_work_card,
               fill = factor(higher_educ_label))) +
   geom_bar(stat = "identity") +
   scale_fill_manual(name = "Education Level", 
                     values = c("slateblue", "dodgerblue",
                                "lightgreen",
                                "goldenrod",
                                "palevioletred",
                                "darkred",
                                "yellow")) +
   labs(x =  "Quarter", y = "Informal Employee") +
   theme_minimal()
 
  ## Desempregados durante a pandemia (sem carteira assinada)
 ff = merge_2 %>%
   filter(year_quarter %in% c("2020_1", "2020_2", "2020_3", "2020_4")) %>%
   select(unemployed, higher_educ_level, year_quarter) %>%
   filter(unemployed == 1)
 
 ggplot(ff, aes(x = year_quarter, y = unemployed,
                fill = factor(higher_educ_level))) +
   geom_bar(stat = "identity") +
   theme_minimal()
 
 
 ## Fazendo Gráfico - trabalhadores\não trabalhadores da última semana + escolaridade

 g = data %>%
   filter(year_quarter %in% c("2019_1", "2019_2", "2019_3", "2019_4") &
            worker == 1) %>%
   select(worker, higher_educ_level, year_quarter) %>%
   mutate(higher_educ_label = case_when(higher_educ_level %in% c(1) ~"Uneducated",
                                        higher_educ_level %in% c(2) ~ "Primary School Incompleted",
                                        higher_educ_level %in% c(3) ~ "Primary School Completed",
                                        higher_educ_level %in% c(4) ~ "High School Incompleted",
                                        higher_educ_level %in% c(5) ~ "High School Completed",
                                        higher_educ_level %in% c(6) ~ "College Degree Incompleted",
                                        higher_educ_level %in% c(7) ~ "College Degree Completed")) 
 
 ggplot(g, aes(x = year_quarter, y = worker,
               fill = factor(higher_educ_label))) +
   geom_bar(stat = "identity") +
   scale_fill_manual(name = "Education Level",
                     values = c("#ff66c4", "#7ed957",
                                "#5271ff",
                                "#35564f",
                                "#cb6ce6",
                                "#00c2cb",
                                "#935c25")) +
   labs(x = "Quarter", y = "Worker") +
   theme_minimal()
 
 
 g1 = data %>%
   filter(year_quarter %in% c("2019_1", "2019_2", "2019_3", "2019_4") &
            worker == 2) %>%
   select(worker, higher_educ_level, year_quarter) %>%
   mutate(higher_educ_label = case_when(higher_educ_level %in% c(1) ~"Uneducated",
                                        higher_educ_level %in% c(2) ~ "Primary School Incompleted",
                                        higher_educ_level %in% c(3) ~ "Primary School Completed",
                                        higher_educ_level %in% c(4) ~ "High School Incompleted",
                                        higher_educ_level %in% c(5) ~ "High School Completed",
                                        higher_educ_level %in% c(6) ~ "College Degree Incompleted",
                                        higher_educ_level %in% c(7) ~ "College Degree Completed")) 
 
 ggplot(g1, aes(x = year_quarter, y = worker,
               fill = factor(higher_educ_label))) +
   geom_bar(stat = "identity") +
   scale_fill_manual(name = "Education Level",
                     values = c("powderblue", "firebrick",
                                "deeppink",
                                "sandybrown",
                                "lawngreen",
                                "slategray",
                                "darkturquoise")) +
   labs(x = "Quarter", y = "Non-Worker") +
   theme_minimal()
 
 
 
 ## Desempregados última semana da pesquisa PNAD
 
 gg = merge %>%
   filter(year_quarter %in% c("2020_1", "2020_2", "2020_3", "2020_4")) %>%
   select(id_code, unemployed, higher_educ_level, year_quarter) %>%
   filter(unemployed == 1)
 
 ggplot(gg, aes(x = year_quarter, y = unemployed,
                fill = factor(higher_educ_level))) +
   geom_bar(stat = "identity") +
   theme_minimal()
 
 
 ## Fazendo Gráfico - trabalhadores que contribuem\não contribuem pro INSS + escolaridade
 
 h = data %>%
   filter(year_quarter %in% c("2019_1", "2019_2", "2019_3", "2019_4") &
            social_security_taxpayer == 1) %>%
   select(social_security_taxpayer, higher_educ_level, year_quarter) %>%
   mutate(higher_educ_label = case_when(higher_educ_level %in% c(1) ~"Uneducated",
                                        higher_educ_level %in% c(2) ~ "Primary School Incompleted",
                                        higher_educ_level %in% c(3) ~ "Primary School Completed",
                                        higher_educ_level %in% c(4) ~ "High School Incompleted",
                                        higher_educ_level %in% c(5) ~ "High School Completed",
                                        higher_educ_level %in% c(6) ~ "College Degree Incompleted",
                                        higher_educ_level %in% c(7) ~ "College Degree Completed")) 
 
 
 ggplot(h, aes(x = year_quarter, y = social_security_taxpayer,
               fill = factor(higher_educ_label))) +
   geom_bar(stat = "identity") +
   scale_fill_manual(name = "Education Level",
                     values = c("#004aad", "#7ed957",
                                "#1e332e",
                                "#03989e",
                                "#65121c",
                                "#a88f79",
                                "#ff914d")) +
   labs(x = "Quarter", y = "Social Security Contributors") +
   theme_minimal()
 
 
 h1 = data %>%
   filter(year_quarter %in% c("2019_1", "2019_2", "2019_3", "2019_4") &
            social_security_taxpayer == 2) %>%
   select(social_security_taxpayer, higher_educ_level, year_quarter) %>%
   mutate(higher_educ_label = case_when(higher_educ_level %in% c(1) ~"Uneducated",
                                        higher_educ_level %in% c(2) ~ "Primary School Incompleted",
                                        higher_educ_level %in% c(3) ~ "Primary School Completed",
                                        higher_educ_level %in% c(4) ~ "High School Incompleted",
                                        higher_educ_level %in% c(5) ~ "High School Completed",
                                        higher_educ_level %in% c(6) ~ "College Degree Incompleted",
                                        higher_educ_level %in% c(7) ~ "College Degree Completed")) 
 
 
 ggplot(h1, aes(x = year_quarter, y = social_security_taxpayer,
               fill = factor(higher_educ_label))) +
   geom_bar(stat = "identity") +
   scale_fill_manual(name = "Education Level",
                     values = c("midnightblue", "springgreen",
                                "rosybrown",
                                "darkviolet",
                                "aquamarine",
                                "wheat",
                                "gold")) +
   labs(x = "Quarter", y = "Non-Social Security Contributors") +
   theme_minimal()
 
 ## Desempregados (Não contribuem para o INSS)
 
 hh = merge_1 %>%
   filter(year_quarter %in% c("2020_1", "2020_2", "2020_3", "2020_4")) %>%
   select(id_code, unemployed, higher_educ_level, year_quarter) %>%
   filter(unemployed == 1)
 
 ggplot(hh, aes(x = year_quarter, y = unemployed,
                fill = factor(higher_educ_level))) +
   geom_bar(stat = "identity") +
   theme_minimal()
 
 
 ## Fazendo Gráfico - Conta Própria que contríbuia INSS
 
 i = data %>%
   filter(year_quarter %in% c("2019_1")) %>%
   select(social_security_taxpayer, higher_educ_level, year_quarter,
          job_function) %>%
   filter(job_function == 6)
 
 
 ggplot(i, aes(x = year_quarter, y = social_security_taxpayer,
               fill = factor(higher_educ_level))) +
   geom_bar(stat="identity") +
   coord_polar("y")
 
 
 ii = data %>%
   filter(year_quarter %in% c("2019_2")) %>%
   select(social_security_taxpayer, higher_educ_level, year_quarter,
          job_function) %>%
   filter(job_function == 6)
 
 ggplot(ii, aes(x = year_quarter, y = social_security_taxpayer,
               fill = factor(higher_educ_level))) +
   geom_bar(stat="identity") +
   coord_polar("y")
 
 
 iii = data %>%
   filter(year_quarter %in% c("2019_3")) %>%
   select(social_security_taxpayer, higher_educ_level, year_quarter,
          job_function) %>%
   filter(job_function == 6)
 
 ggplot(iii, aes(x = year_quarter, y = social_security_taxpayer,
                fill = factor(higher_educ_level))) +
   geom_bar(stat="identity") +
   coord_polar("y")
 
 
 iiii = data %>%
   filter(year_quarter %in% c("2019_4")) %>%
   select(social_security_taxpayer, higher_educ_level, year_quarter,
          job_function) %>%
   filter(job_function == 6)
 
 
 ggplot(iiii, aes(x = year_quarter, y = social_security_taxpayer,
                 fill = factor(higher_educ_level))) +
   geom_bar(stat="identity") +
   coord_polar("y")
 
 
 
   
 