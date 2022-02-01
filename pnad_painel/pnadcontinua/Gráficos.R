font_import(path = "C:/Users/Fredie/AppData/Local/Microsoft/Windows/Fonts")
y
loadfonts(device = "win")

library(ggsci)
library(RColorBrewer)
library(ghibli)
library(dichromat)
library(rcartocolor)
library(grid)
library(gridExtra)


 ## Fazendo Gráfico - trabalhadores com carteira assinada + escolaridade ###

trab_carteira_ass = data %>%
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

 graf.1 = ggplot(trab_carteira_ass, aes(x = year_quarter, y = signed_work_card,
    fill = factor(higher_educ_label))) +
  geom_bar(stat = "identity") +
   scale_fill_manual(name = "Education Level", 
                     values = carto_pal(name = "Geyser")) +
   labs(x =  "Quarter", y = "Signed Contract Workers",
        title = "Education Level of Registered Employees in 2019") +
   theme_minimal() +
   theme(text = element_text(family = "LM Roman 10"),
         plot.title = element_text(size = 13, face = "bold", hjust = 0.5))
 #########################################################################
 ######### Fazendo Gráfico trabalhadores sem carteira assinada (informais) + escolaridade ###
 
 trab_sem_carteira_ass = data %>%
   filter(year_quarter %in% c("2019_1", "2019_2", "2019_3", "2019_4") &
            worker == 1 & signed_work_card == 2 & social_security_taxpayer == 1) %>%
   select(worker, higher_educ_level, year_quarter, signed_work_card) %>%
   mutate(higher_educ_label = case_when(higher_educ_level %in% c(1) ~"Uneducated",
                                        higher_educ_level %in% c(2) ~ "Primary School Incompleted",
                                        higher_educ_level %in% c(3) ~ "Primary School Completed",
                                        higher_educ_level %in% c(4) ~ "High School Incompleted",
                                        higher_educ_level %in% c(5) ~ "High School Completed",
                                        higher_educ_level %in% c(6) ~ "College Degree Incompleted",
                                        higher_educ_level %in% c(7) ~ "College Degree Completed")) 
 
 inf1 = ggplot(trab_sem_carteira_ass, aes(x = year_quarter, y = worker,
                        fill = factor(higher_educ_label))) +
   geom_bar(stat = "identity") +
   scale_fill_manual(name = "Education Level",
                     values = carto_pal(name = "Safe")) +
   labs(x = "Quarter", y = "Workers",
        title = "Education Level of Non-Registered Workers in 2019") +
   theme_minimal() +
   theme(text = element_text(family = "LM Roman 10"),
         plot.title = element_text(size = 13, face = "bold", hjust = 0.5))
 #########################################################################
 #### Fazendo Gráfico de Desempregados #####
 
 desempregados = data %>%
   filter(year_quarter %in% c("2019_1", "2019_2", "2019_3", "2019_4") &
            worker == 2 & signed_work_card == 2 & social_security_taxpayer == 2) %>%
   select(signed_work_card, social_security_taxpayer,
          worker, higher_educ_level, year_quarter) %>%
   mutate(higher_educ_label = case_when(higher_educ_level %in% c(1) ~"Uneducated",
                                        higher_educ_level %in% c(2) ~ "Primary School Incompleted",
                                        higher_educ_level %in% c(3) ~ "Primary School Completed",
                                        higher_educ_level %in% c(4) ~ "High School Incompleted",
                                        higher_educ_level %in% c(5) ~ "High School Completed",
                                        higher_educ_level %in% c(6) ~ "College Degree Incompleted",
                                        higher_educ_level %in% c(7) ~ "College Degree Completed")) 
 
 desemp = ggplot(desempregados, aes(x = year_quarter, y = worker,
               fill = factor(higher_educ_label))) +
   geom_bar(stat = "identity") +
   scale_fill_manual(name = "Education Level",
                     values = carto_pal(name = "Prism")) +
   scale_y_continuous(labels = scales::comma) +
   labs(x = "Quarter", y = "Non-Workers",
        title = "Education Level of the Unemployed in 2019") +
   theme_minimal() +
   theme(text = element_text(family = "LM Roman 10"),
         plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
         legend.title = element_blank())
 #########################################################################
 ########## Fazendo Gráfico de trabalhadores sem carteira e que não contribuem pro INSS ###
 
 trab_sem_carteira_e_sem_INSS = data %>%
   filter(year_quarter %in% c("2019_1", "2019_2", "2019_3", "2019_4") &
            worker == 1 & signed_work_card == 2 & social_security_taxpayer == 2) %>%
   select(signed_work_card, social_security_taxpayer,
          worker, higher_educ_level, year_quarter) %>%
   mutate(higher_educ_label = case_when(higher_educ_level %in% c(1) ~"Uneducated",
                                        higher_educ_level %in% c(2) ~ "Primary School Incompleted",
                                        higher_educ_level %in% c(3) ~ "Primary School Completed",
                                        higher_educ_level %in% c(4) ~ "High School Incompleted",
                                        higher_educ_level %in% c(5) ~ "High School Completed",
                                        higher_educ_level %in% c(6) ~ "College Degree Incompleted",
                                        higher_educ_level %in% c(7) ~ "College Degree Completed")) 
 
 inf2 = ggplot(trab_sem_carteira_e_sem_INSS, aes(x = year_quarter, y = worker,
                         fill = factor(higher_educ_label))) +
   geom_bar(stat = "identity") +
   scale_fill_manual(name = "Education Level",
                     values = carto_pal(name = "Safe")) +
   scale_y_continuous(labels = scales::comma) +
   labs(x = "Quarter", y = "Non-Contributors",
        title = "Education Level of Non-Social Security Taxpayers Workers in 2019") +
   theme_minimal() +
   theme(text = element_text(family = "LM Roman 10"),
         plot.title = element_text(size = 13, face = "bold", hjust = 0.5))
 
 ## Fazendo Gráfico - trabalhadores que contribuem pro INSS + escolaridade
 
 trab_contribui_INSS = data %>%
   filter(year_quarter %in% c("2019_1", "2019_2", "2019_3", "2019_4") &
            social_security_taxpayer == 1) %>%
   select(signed_work_card, social_security_taxpayer, higher_educ_level, year_quarter) %>%
   mutate(higher_educ_label = case_when(higher_educ_level %in% c(1) ~"Uneducated",
                                        higher_educ_level %in% c(2) ~ "Primary School Incompleted",
                                        higher_educ_level %in% c(3) ~ "Primary School Completed",
                                        higher_educ_level %in% c(4) ~ "High School Incompleted",
                                        higher_educ_level %in% c(5) ~ "High School Completed",
                                        higher_educ_level %in% c(6) ~ "College Degree Incompleted",
                                        higher_educ_level %in% c(7) ~ "College Degree Completed"))
 
 
 graf.2 = ggplot(trab_contribui_INSS, aes(x = year_quarter, y = social_security_taxpayer,
               fill = factor(higher_educ_label))) +
   geom_bar(stat = "identity") +
   scale_fill_manual(name = "Education Level",
                     values = carto_pal(name = "Geyser")) +
   labs(x = "Quarter", y = "Contributors",
        title = "Education Level of Social Security Taxpayers in 2019") +
   theme_minimal()+
   theme(text = element_text(family = "LM Roman 10"),
         plot.title = element_text(size = 13, face = "bold", hjust = 0.5))
 ########################################################################
 ######################################################################

 ## Fazendo Gráfico - Conta Própria que contríbuia INSS
 
 trab_conta_propria_INSS = data %>%
   filter(year_quarter %in% c("2019_1")) %>%
   select(social_security_taxpayer, higher_educ_level, year_quarter,
          job_function) %>%
   filter(job_function == 6 & social_security_taxpayer == 1)  %>%
   mutate(higher_educ_label = case_when(higher_educ_level %in% c(1) ~"Uneducated",
                                        higher_educ_level %in% c(2) ~ "Primary School Incompleted",
                                        higher_educ_level %in% c(3) ~ "Primary School Completed",
                                        higher_educ_level %in% c(4) ~ "High School Incompleted",
                                        higher_educ_level %in% c(5) ~ "High School Completed",
                                        higher_educ_level %in% c(6) ~ "College Degree Incompleted",
                                        higher_educ_level %in% c(7) ~ "College Degree Completed"))  %>%
   group_by(higher_educ_level) %>% mutate(labels = n()) %>%
   mutate(labell = round(labels/nrow(.), digits = 2))
 
 trab_conta_propria_INSS$labell = trab_conta_propria_INSS$labell * 100 
 
 base = trab_conta_propria_INSS%>%
   select(year_quarter, higher_educ_label, labell) %>%
   distinct()
 
 
 
 graf_1 = ggplot(base, aes(x = year_quarter, y = labell,
               fill = factor(higher_educ_label))) +
   geom_bar(stat="identity") +
   scale_fill_manual(name = "Education Level",
                     values = carto_pal(name = "Temps")) +
   labs(x = "Quarter", y = "Self-Employed Taxpayers",
        title = "Education Level of Social Security Contributors in 2019.1") +
   theme_minimal() +
   theme(text = element_text(family = "LM Roman 10"),
         plot.title = element_text(size = 13, face = "bold", hjust = 0.5)) +
   geom_label(aes(x = 1.2, label = paste0(labell, "%")), position = position_stack(vjust = 0.5),
              show.legend = F)+
   coord_polar("y")
 
 ## Fazendo Gráfico - Conta Própria que não contribui para INSS
 
 trab_conta_propria_n_INSS = data %>%
   filter(year_quarter %in% c("2019_1")) %>%
   select(social_security_taxpayer, higher_educ_level, year_quarter,
          job_function) %>%
   filter(job_function == 6 & social_security_taxpayer == 2)  %>%
   mutate(higher_educ_label = case_when(higher_educ_level %in% c(1) ~"Uneducated",
                                        higher_educ_level %in% c(2) ~ "Primary School Incompleted",
                                        higher_educ_level %in% c(3) ~ "Primary School Completed",
                                        higher_educ_level %in% c(4) ~ "High School Incompleted",
                                        higher_educ_level %in% c(5) ~ "High School Completed",
                                        higher_educ_level %in% c(6) ~ "College Degree Incompleted",
                                        higher_educ_level %in% c(7) ~ "College Degree Completed"))  %>%
   group_by(higher_educ_level) %>% mutate(labels = n()) %>%
   mutate(labell = round(labels/nrow(.), digits = 2))
 
 trab_conta_propria_n_INSS$labell = trab_conta_propria_n_INSS$labell * 100 
 
 base_2 = trab_conta_propria_n_INSS%>%
   select(year_quarter, higher_educ_label, labell) %>%
   distinct()
 
 
 
 grafico1 = ggplot(base_2, aes(x = year_quarter, y = labell,
               fill = factor(higher_educ_label))) +
   geom_bar(stat="identity") +
   scale_fill_manual(name = "Education Level",
                     values = carto_pal(name = "Earth")) +
   labs(x = "Quarter", y = "Self-Employed Non-Taxpayers",
        title = "Education Level of Non-Social Security Contributors in 2019.1") +
   theme_minimal() +
   theme(text = element_text(family = "LM Roman 10"),
         plot.title = element_text(size = 13, face = "bold", hjust = 0.5)) +
   geom_label(aes(x = 1.2, label = paste0(labell, "%")), position = position_stack(vjust = 0.5),
              show.legend = F)+
   coord_polar("y")
 
 #### Fazendo gráfico - Conta Própria que contribuía pro INSS
 
 trab_conta_propria_INSS_2 = data %>%
   filter(year_quarter %in% c("2019_2")) %>%
   select(social_security_taxpayer, higher_educ_level, year_quarter,
          job_function) %>%
   filter(job_function == 6 & social_security_taxpayer == 1) %>%
   mutate(higher_educ_label = case_when(higher_educ_level %in% c(1) ~"Uneducated",
                                        higher_educ_level %in% c(2) ~ "Primary School Incompleted",
                                        higher_educ_level %in% c(3) ~ "Primary School Completed",
                                        higher_educ_level %in% c(4) ~ "High School Incompleted",
                                        higher_educ_level %in% c(5) ~ "High School Completed",
                                        higher_educ_level %in% c(6) ~ "College Degree Incompleted",
                                        higher_educ_level %in% c(7) ~ "College Degree Completed")) %>%
   group_by(higher_educ_level) %>% mutate(labels = n()) %>%
   mutate(labell = round(labels/nrow(.), digits = 2))
 
 trab_conta_propria_INSS_2$labell = trab_conta_propria_INSS_2$labell * 100 
 
 base_3 = trab_conta_propria_INSS_2 %>%
   select(year_quarter, higher_educ_label, labell) %>%
   distinct()
 
 graf_2 = ggplot(base_3, aes(x = year_quarter, y = labell,
               fill = factor(higher_educ_label))) +
   geom_bar(stat="identity") +
   scale_fill_manual(name = "Education Level",
                     values = carto_pal(name = "Temps"))+
   labs(x = "Quarter", y = "Self-Employed Taxpayers",
        title = "Education Level of Social Security Contributors in 2019.2") +
   theme_minimal() +
   theme(text = element_text(family = "LM Roman 10"),
         plot.title = element_text(size = 13, face = "bold", hjust = 0.5)) +
   geom_label(aes(x = 1.2, label = paste0(labell, "%")), position = position_stack(vjust = 0.5),
              show.legend = F)+
   coord_polar("y")
 
 #### Fazendo gráfico - Conta Própria que não contribuía pro INSS
 
 trab_conta_propria_n_INSS_2 = data %>%
   filter(year_quarter %in% c("2019_2")) %>%
   select(social_security_taxpayer, higher_educ_level, year_quarter,
          job_function) %>%
   filter(job_function == 6 & social_security_taxpayer == 2) %>%
   mutate(higher_educ_label = case_when(higher_educ_level %in% c(1) ~"Uneducated",
                                        higher_educ_level %in% c(2) ~ "Primary School Incompleted",
                                        higher_educ_level %in% c(3) ~ "Primary School Completed",
                                        higher_educ_level %in% c(4) ~ "High School Incompleted",
                                        higher_educ_level %in% c(5) ~ "High School Completed",
                                        higher_educ_level %in% c(6) ~ "College Degree Incompleted",
                                        higher_educ_level %in% c(7) ~ "College Degree Completed")) %>%
   group_by(higher_educ_level) %>% mutate(labels = n()) %>%
   mutate(labell = round(labels/nrow(.), digits = 2))
 
 trab_conta_propria_n_INSS_2$labell = trab_conta_propria_n_INSS_2$labell * 100 
 
 base_4 = trab_conta_propria_n_INSS_2 %>%
   select(year_quarter, higher_educ_label, labell) %>%
   distinct()
 
 grafico2 = ggplot(base_4, aes(x = year_quarter, y = labell,
                fill = factor(higher_educ_label))) +
   geom_bar(stat="identity") +
   scale_fill_manual(name = "Education Level",
                     values = carto_pal(name = "Earth"))+
   labs(x = "Quarter", y = "Self-Employed Non-Taxpayers",
        title = "Education Level of Non-Social Security Contributors in 2019.2") +
   theme_minimal() +
   theme(text = element_text(family = "LM Roman 10"),
         plot.title = element_text(size = 13, face = "bold", hjust = 0.5)) +
   geom_label(aes(x = 1.3, label = paste0(labell, "%")), position = position_stack(vjust = 0.5),
              show.legend = F)+
   coord_polar("y")
 
 ### Fazendo gráfico - Conta Própria que contribuía pro INSS
 
 trab_conta_propria_INSS_3 = data %>%
   filter(year_quarter %in% c("2019_3")) %>%
   select(social_security_taxpayer, higher_educ_level, year_quarter,
          job_function) %>%
   filter(job_function == 6 & social_security_taxpayer == 1) %>%
   mutate(higher_educ_label = case_when(higher_educ_level %in% c(1) ~"Uneducated",
                                        higher_educ_level %in% c(2) ~ "Primary School Incompleted",
                                        higher_educ_level %in% c(3) ~ "Primary School Completed",
                                        higher_educ_level %in% c(4) ~ "High School Incompleted",
                                        higher_educ_level %in% c(5) ~ "High School Completed",
                                        higher_educ_level %in% c(6) ~ "College Degree Incompleted",
                                        higher_educ_level %in% c(7) ~ "College Degree Completed")) %>%
   group_by(higher_educ_level) %>% mutate(labels = n()) %>%
   mutate(labell = round(labels/nrow(.), digits = 2))

 trab_conta_propria_INSS_3$labell = trab_conta_propria_INSS_3$labell * 100 
 
 base_5 = trab_conta_propria_INSS_3 %>%
   select(year_quarter, higher_educ_label, labell) %>%
   distinct()
 
 graf_3 = ggplot(base_5, aes(x = year_quarter, y = labell,
                 fill = factor(higher_educ_label))) +
   geom_bar(stat="identity") +
   scale_fill_manual(name = "Education Level",
                     values = carto_pal(name = "Temps")) +
   labs(x = "Quarter", y = "Self-Employed Taxpayers",
        title = "Education Level of Social Security Contributors in 2019.3") +
   theme_minimal() +
   theme(text = element_text(family = "LM Roman 10"),
         plot.title = element_text(size = 13, face = "bold", hjust = 0.5)) +
   geom_label(aes(x = 1.2, label = paste0(labell, "%")), position = position_stack(vjust = 0.5),
              show.legend = F)+
   coord_polar("y")
 
 
 ### Fazendo Gráfico - Conta Própria que não contribuía pro INSS
 
 trab_conta_propria_n_INSS_3 = data %>%
   filter(year_quarter %in% c("2019_3")) %>%
   select(social_security_taxpayer, higher_educ_level, year_quarter,
          job_function) %>%
   filter(job_function == 6 & social_security_taxpayer == 2) %>%
   mutate(higher_educ_label = case_when(higher_educ_level %in% c(1) ~"Uneducated",
                                        higher_educ_level %in% c(2) ~ "Primary School Incompleted",
                                        higher_educ_level %in% c(3) ~ "Primary School Completed",
                                        higher_educ_level %in% c(4) ~ "High School Incompleted",
                                        higher_educ_level %in% c(5) ~ "High School Completed",
                                        higher_educ_level %in% c(6) ~ "College Degree Incompleted",
                                        higher_educ_level %in% c(7) ~ "College Degree Completed"))%>%
   group_by(higher_educ_level) %>% mutate(labels = n()) %>%
   mutate(labell = round(labels/nrow(.), digits = 2))
 
 trab_conta_propria_n_INSS_3$labell = trab_conta_propria_n_INSS_3$labell * 100 
 
 base_6 = trab_conta_propria_n_INSS_3 %>%
   select(year_quarter, higher_educ_label, labell) %>%
   distinct()
 
 
 
 grafico3 = ggplot(base_6, aes(x = year_quarter, y = labell,
                 fill = factor(higher_educ_label))) +
   geom_bar(stat="identity") +
   scale_fill_manual(name = "Education Level",
                     values = carto_pal(name = "Earth")) +
   labs(x = "Quarter", y = "Self-Employed Non-Taxpayers",
        title = "Education Level of Non-Social Security Contributors in 2019.3") +
   theme_minimal() +
   theme(text = element_text(family = "LM Roman 10"),
         plot.title = element_text(size = 13, face = "bold", hjust = 0.5)) +
   geom_label(aes(x = 1.3, label = paste0(labell, "%")), position = position_stack(vjust = 0.5),
              show.legend = F)+
   coord_polar("y")
 
 ##### Fazendo Gráfico - Conta Própria que contribuía para INSS
 
 trab_conta_propria_INSS_4 = data %>%
   filter(year_quarter %in% c("2019_4")) %>%
   select(social_security_taxpayer, higher_educ_level, year_quarter,
          job_function) %>%
   filter(job_function == 6 & social_security_taxpayer == 1) %>%
   mutate(higher_educ_label = case_when(higher_educ_level %in% c(1) ~"Uneducated",
                                        higher_educ_level %in% c(2) ~ "Primary School Incompleted",
                                        higher_educ_level %in% c(3) ~ "Primary School Completed",
                                        higher_educ_level %in% c(4) ~ "High School Incompleted",
                                        higher_educ_level %in% c(5) ~ "High School Completed",
                                        higher_educ_level %in% c(6) ~ "College Degree Incompleted",
                                        higher_educ_level %in% c(7) ~ "College Degree Completed")) %>%
   group_by(higher_educ_level) %>% mutate(labels = n()) %>%
   mutate(labell = round(labels/nrow(.), digits = 2))
 
 trab_conta_propria_INSS_4$labell = trab_conta_propria_INSS_4$labell * 100
 
 base_7 = trab_conta_propria_INSS_4 %>%
   select(year_quarter, higher_educ_label, labell) %>%
   distinct()
 
 graf_4 = ggplot(base_7, aes(x = year_quarter, y = labell,
                  fill = factor(higher_educ_label))) +
   geom_bar(stat = "identity")+
   scale_fill_manual(name = "Education Level",
                     values = carto_pal(name = "Temps")) +
   labs(x = "Quarter", y = "Self-Employed Taxpayers",
        title = "Education Level of Social Security Contributors in 2019.4") +
   theme_minimal()+
   theme(text = element_text(family = "LM Roman 10"),
         plot.title = element_text(size = 13, face = "bold", hjust = 0.5)) +
   geom_label(aes(x = 1.4, label = paste0(labell, "%")), position = position_stack(vjust = 0.5),
              show.legend = F)+
   coord_polar("y")
 
 ### Fazendo Gráfico - Conta Própria que não contribuía pro INSS
 
 trab_conta_propria_n_INSS_4 = data %>%
   filter(year_quarter %in% c("2019_4")) %>%
   select(social_security_taxpayer, higher_educ_level, year_quarter,
          job_function) %>%
   filter(job_function == 6 & social_security_taxpayer == 2) %>%
   mutate(higher_educ_label = case_when(higher_educ_level %in% c(1) ~"Uneducated",
                                        higher_educ_level %in% c(2) ~ "Primary School Incompleted",
                                        higher_educ_level %in% c(3) ~ "Primary School Completed",
                                        higher_educ_level %in% c(4) ~ "High School Incompleted",
                                        higher_educ_level %in% c(5) ~ "High School Completed",
                                        higher_educ_level %in% c(6) ~ "College Degree Incompleted",
                                        higher_educ_level %in% c(7) ~ "College Degree Completed")) %>%
   group_by(higher_educ_level) %>% mutate(labels = n()) %>%
   mutate(labell = round(labels/nrow(.), digits = 2))
 
 trab_conta_propria_n_INSS_4$labell = trab_conta_propria_n_INSS_4$labell * 100
 
 
 base_8 = trab_conta_propria_n_INSS_4 %>%
   select(year_quarter, higher_educ_label, labell) %>%
   distinct()
 
 grafico4 = ggplot(base_8, aes(x = year_quarter, y = labell,
                  fill = higher_educ_label)) +
   geom_bar(stat = "identity")+
   scale_fill_manual(name = "Education Level",
                     values = carto_pal(name = "Earth")) +
   labs(x = "Quarter", y = "Self-Employed Non-Taxpayers",
        title = "Education Level of Non-Social Security Contributors in 2019.4") +
   theme_minimal() +
   theme(text = element_text(family = "LM Roman 10"),
         plot.title = element_text(size = 13, face = "bold", hjust = 0.5)) +
   geom_label(aes(x = 1.2, label = paste0(labell, "%")), position = position_stack(vjust = 0.5),
                    show.legend = F)+
   coord_polar("y")
 
 
 
   
 