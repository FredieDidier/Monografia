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
         signed_work_card == 1 & worker == 1) %>%
  select(signed_work_card, higher_educ_level, year_quarter, worker) %>%
  mutate(higher_educ_label = case_when(higher_educ_level %in% c(1) ~"Uneducated",
                                       higher_educ_level %in% c(2) ~ "Primary School Incompleted",
                                       higher_educ_level %in% c(3) ~ "Primary School Completed",
                                       higher_educ_level %in% c(4) ~ "High School Incompleted",
                                       higher_educ_level %in% c(5) ~ "High School Completed",
                                       higher_educ_level %in% c(6) ~ "College Degree Incompleted",
                                       higher_educ_level %in% c(7) ~ "College Degree Completed")) %>%
  group_by(higher_educ_level) %>% mutate(labels = n()) %>%
  mutate(labell = round(labels/nrow(.), digits = 2))

trab_carteira_ass$labell = trab_carteira_ass$labell * 100

trab_carteira_ass = trab_carteira_ass %>%
  distinct() %>%
  mutate(labelll = case_when(higher_educ_label == "Uneducated" & year_quarter == "2019_1" ~ 1,
                             higher_educ_label == "Primary School Incompleted" & year_quarter == "2019_1" ~ 18,
                             higher_educ_label == "Primary School Completed" & year_quarter == "2019_1" ~ 8,
                             higher_educ_label == "High School Incompleted" & year_quarter == "2019_1" ~ 7,
                             higher_educ_label == "High School Completed" & year_quarter == "2019_1" ~ 41,
                             higher_educ_label == "College Degree Incompleted" & year_quarter == "2019_1" ~ 7,
                             higher_educ_label == "College Degree Completed" & year_quarter == "2019_1" ~ 18)) %>%
  filter(!is.na(labelll))

graf.1 = ggplot(trab_carteira_ass, aes(x = "", y = labelll,
                                       fill = factor(higher_educ_label))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(name = "Education Level", 
                    values = carto_pal(name = "Geyser")) +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "", y = "", title = "Education Level of Registered Employees in 2019") +
  theme_minimal() +
  theme(text = element_text(family = "LM Roman 10"),
        plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
        legend.title = element_blank(),
        axis.text = element_blank()) +
  geom_label(aes(x = 1.2, label = paste0(labelll, "%")), position = position_stack(vjust = 0.5),
             show.legend = F)+
  coord_polar("y")
 #########################################################################
 ######### Fazendo Gráfico trabalhadores sem carteira assinada (informais) + escolaridade ###
 
trab_sem_carteira_ass = data %>%
  filter(year_quarter %in% c("2019_1", "2019_2", "2019_3", "2019_4") &
           worker == 1 & signed_work_card == 2 & social_security_taxpayer == 1) %>%
  select(worker, year_quarter, signed_work_card, higher_educ_level) %>%
  mutate(higher_educ_label = case_when(higher_educ_level %in% c(1) ~"Uneducated",
                                       higher_educ_level %in% c(2) ~ "Primary School Incompleted",
                                       higher_educ_level %in% c(3) ~ "Primary School Completed",
                                       higher_educ_level %in% c(4) ~ "High School Incompleted",
                                       higher_educ_level %in% c(5) ~ "High School Completed",
                                       higher_educ_level %in% c(6) ~ "College Degree Incompleted",
                                       higher_educ_level %in% c(7) ~ "College Degree Completed")) %>%
  group_by(higher_educ_level) %>% mutate(labels = n()) %>%
  mutate(labell = round(labels/nrow(.), digits = 2))

trab_sem_carteira_ass$labell = trab_sem_carteira_ass$labell * 100

trab_sem_carteira_ass = trab_sem_carteira_ass %>%
  select(year_quarter, higher_educ_label, higher_educ_level, labell) %>%
  distinct() %>%
  mutate(labelll = case_when(higher_educ_label == "Uneducated" & year_quarter == "2019_1" ~ 1,
                             higher_educ_label == "Primary School Incompleted" & year_quarter == "2019_1" ~ 18,
                             higher_educ_label == "Primary School Completed" & year_quarter == "2019_1" ~ 6,
                             higher_educ_label == "High School Incompleted" & year_quarter == "2019_1" ~ 5,
                             higher_educ_label == "High School Completed" & year_quarter == "2019_1" ~ 29,
                             higher_educ_label == "College Degree Incompleted" & year_quarter == "2019_1" ~ 9,
                             higher_educ_label == "College Degree Completed" & year_quarter == "2019_1" ~ 32)) %>%
  filter(!is.na(labelll))

inf1 = ggplot(trab_sem_carteira_ass, aes(x = "", y = labelll,
                                         fill = factor(higher_educ_label))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(name = "Education Level",
                    values = carto_pal(name = "Safe")) +
  labs(x = "", y = "",
       title = "Education Level of Non-Registered Workers in 2019") +
  theme_minimal() +
  theme(text = element_text(family = "LM Roman 10"),
        plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
        legend.title = element_blank(),
        axis.text = element_blank()) +
  geom_label(aes(x = 1.2, label = paste0(labell, "%")), position = position_stack(vjust = 0.5),
             show.legend = F)+
  coord_polar("y")
 #########################################################################
 #### Fazendo Gráfico de Desempregados #####
 
 desempregados = data_2019 %>%
   filter(workforce_condition == 1, occupation_condition == 2 & worker == 2) %>%
   select(workforce_condition, occupation_condition, worker, higher_educ_level, year_quarter) %>%
   mutate(higher_educ_label = case_when(higher_educ_level %in% c(1) ~"Uneducated",
                                        higher_educ_level %in% c(2) ~ "Primary School Incompleted",
                                        higher_educ_level %in% c(3) ~ "Primary School Completed",
                                        higher_educ_level %in% c(4) ~ "High School Incompleted",
                                        higher_educ_level %in% c(5) ~ "High School Completed",
                                        higher_educ_level %in% c(6) ~ "College Degree Incompleted",
                                        higher_educ_level %in% c(7) ~ "College Degree Completed")) %>%
  group_by(higher_educ_level) %>% mutate(labels = n()) %>%
  mutate(labell = round(labels/nrow(.), digits = 2))

desempregados$labell = desempregados$labell * 100

desempregados = desempregados %>%
  select(year_quarter, higher_educ_label, higher_educ_level, labell) %>%
  distinct() %>%
  mutate(labelll = case_when(higher_educ_label == "Uneducated" & year_quarter == "2019_1" ~ 2,
                             higher_educ_label == "Primary School Incompleted" & year_quarter == "2019_1" ~ 23,
                             higher_educ_label == "Primary School Completed" & year_quarter == "2019_1" ~ 9,
                             higher_educ_label == "High School Incompleted" & year_quarter == "2019_1" ~ 12,
                             higher_educ_label == "High School Completed" & year_quarter == "2019_1" ~ 37,
                             higher_educ_label == "College Degree Incompleted" & year_quarter == "2019_1" ~ 7,
                             higher_educ_label == "College Degree Completed" & year_quarter == "2019_1" ~ 9)) %>%
  filter(!is.na(labelll))
 
 desemp = ggplot(desempregados, aes(x = "", y = labelll,
               fill = factor(higher_educ_label))) +
   geom_bar(stat = "identity") +
   scale_fill_manual(name = "Education Level",
                     values = carto_pal(name = "Prism")) +
   labs(x = "", y = "",
        title = "Education Level of the Unemployed in 2019") +
   theme_minimal() +
   theme(text = element_text(family = "LM Roman 10"),
         plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
         legend.title = element_blank(),
         axis.text = element_blank()) +
   geom_label(aes(x = 1.2, label = paste0(labelll, "%")), position = position_stack(vjust = 0.5),
              show.legend = F)+
   coord_polar("y")
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
                                        higher_educ_level %in% c(7) ~ "College Degree Completed")) %>%
   group_by(higher_educ_level) %>% mutate(labels = n()) %>%
   mutate(labell = round(labels/nrow(.), digits = 2))
 
 trab_sem_carteira_e_sem_INSS$labell = trab_sem_carteira_e_sem_INSS$labell * 100
 
 trab_sem_carteira_e_sem_INSS = trab_sem_carteira_e_sem_INSS %>%
   select(year_quarter, higher_educ_label, higher_educ_level, labell) %>%
   distinct() %>%
   mutate(labelll = case_when(higher_educ_label == "Uneducated" & year_quarter == "2019_1" ~ 4,
                              higher_educ_label == "Primary School Incompleted" & year_quarter == "2019_1" ~ 37,
                              higher_educ_label == "Primary School Completed" & year_quarter == "2019_1" ~ 10,
                              higher_educ_label == "High School Incompleted" & year_quarter == "2019_1" ~ 10,
                              higher_educ_label == "High School Completed" & year_quarter == "2019_1" ~ 26,
                              higher_educ_label == "College Degree Incompleted" & year_quarter == "2019_1" ~ 6,
                              higher_educ_label == "College Degree Completed" & year_quarter == "2019_1" ~ 6)) %>%
   filter(!is.na(labelll))
 
 
 inf2 = ggplot(trab_sem_carteira_e_sem_INSS, aes(x = "", y = labelll,
                         fill = factor(higher_educ_label))) +
   geom_bar(stat = "identity") +
   scale_fill_manual(name = "Education Level",
                     values = carto_pal(name = "Safe")) +
   scale_y_continuous(labels = scales::comma) +
   labs(x = "", y = "",
        title = "Education Level of Non-Social Security Taxpayers Workers in 2019") +
   theme_minimal() +
   theme(text = element_text(family = "LM Roman 10"),
         plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
         legend.title = element_blank(),
         axis.text = element_blank()) +
   geom_label(aes(x = 1.2, label = paste0(labelll, "%")), position = position_stack(vjust = 0.5),
              show.legend = F)+
   coord_polar("y")
 
 ## Fazendo Gráfico - trabalhadores que contribuem pro INSS + escolaridade
 
 trab_contribui_INSS = data %>%
   filter(year_quarter %in% c("2019_1", "2019_2", "2019_3", "2019_4") &
            social_security_taxpayer == 1 & worker == 1 & signed_work_card == 2) %>%
   select(signed_work_card, social_security_taxpayer, higher_educ_level, year_quarter, worker) %>%
   mutate(higher_educ_label = case_when(higher_educ_level %in% c(1) ~"Uneducated",
                                        higher_educ_level %in% c(2) ~ "Primary School Incompleted",
                                        higher_educ_level %in% c(3) ~ "Primary School Completed",
                                        higher_educ_level %in% c(4) ~ "High School Incompleted",
                                        higher_educ_level %in% c(5) ~ "High School Completed",
                                        higher_educ_level %in% c(6) ~ "College Degree Incompleted",
                                        higher_educ_level %in% c(7) ~ "College Degree Completed")) %>%
   group_by(higher_educ_level) %>% mutate(labels = n()) %>%
   mutate(labell = round(labels/nrow(.), digits = 2))
 
 trab_contribui_INSS$labell = trab_contribui_INSS$labell * 100
 
 trab_contribui_INSS = trab_contribui_INSS %>%
   select(year_quarter, higher_educ_label, higher_educ_level, labell) %>%
   distinct() %>%
   mutate(labelll = case_when(higher_educ_label == "Uneducated" & year_quarter == "2019_1" ~ 1,
                              higher_educ_label == "Primary School Incompleted" & year_quarter == "2019_1" ~ 18,
                              higher_educ_label == "Primary School Completed" & year_quarter == "2019_1" ~ 6,
                              higher_educ_label == "High School Incompleted" & year_quarter == "2019_1" ~ 5,
                              higher_educ_label == "High School Completed" & year_quarter == "2019_1" ~ 29,
                              higher_educ_label == "College Degree Incompleted" & year_quarter == "2019_1" ~ 9,
                              higher_educ_label == "College Degree Completed" & year_quarter == "2019_1" ~ 32)) %>%
   filter(!is.na(labelll))
 
 
 graf.2 = ggplot(trab_contribui_INSS, aes(x = year_quarter, y = labelll,
               fill = factor(higher_educ_label))) +
   geom_bar(stat = "identity") +
   scale_fill_manual(name = "Education Level",
                     values = carto_pal(name = "Geyser")) +
   labs(x = "", y = "",
        title = "Education Level of Social Security Taxpayers in 2019") +
   theme_minimal()+
   theme(text = element_text(family = "LM Roman 10"),
         plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
         legend.title = element_blank(),
         axis.text = element_blank()) +
   geom_label(aes(x = 1.2, label = paste0(labelll, "%")), position = position_stack(vjust = 0.5),
              show.legend = F)+
   coord_polar("y")
 ########################################################################
 ######################################################################

 ## Fazendo Gráfico - Conta Própria que contríbuia INSS
 
 trab_conta_propria_INSS = data %>%
   filter(year_quarter %in% c("2019_1", "2019_2", "2019_3", "2019_4")) %>%
   select(social_security_taxpayer, higher_educ_level, year_quarter,
          job_function, worker) %>%
   filter(job_function == 6 & social_security_taxpayer == 1 & worker == 1)  %>%
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
 
 trab_conta_propria_INSS = trab_conta_propria_INSS %>%
   select(year_quarter, higher_educ_label, labell) %>%
   distinct() %>%
   mutate(labelll = case_when(higher_educ_label == "Uneducated" & year_quarter == "2019_1" ~ 2,
                                         higher_educ_label == "Primary School Incompleted" & year_quarter == "2019_1" ~ 32,
                                         higher_educ_label == "Primary School Completed" & year_quarter == "2019_1" ~ 9,
                                         higher_educ_label == "High School Incompleted" & year_quarter == "2019_1" ~ 5,
                                         higher_educ_label == "High School Completed" & year_quarter == "2019_1" ~ 30,
                                         higher_educ_label == "College Degree Incompleted" & year_quarter == "2019_1" ~ 4,
                                         higher_educ_label == "College Degree Completed" & year_quarter == "2019_1" ~ 18)) %>%
   filter(!is.na(labelll))
 
 graf_1 = ggplot(trab_conta_propria_INSS, aes(x = "", y = labelll,
               fill = factor(higher_educ_label))) +
   geom_bar(stat="identity") +
   scale_fill_manual(name = "Education Level",
                     values = carto_pal(name = "Temps")) +
   labs(x = "", y = "",
        title = "Education Level of Social Security Contributors in 2019") +
   theme_minimal() +
   theme(text = element_text(family = "LM Roman 10"),
         plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
         legend.title = element_blank(),
         axis.text = element_blank()) +
   geom_label(aes(x = 1.2, label = paste0(labelll, "%")), position = position_stack(vjust = 0.5),
              show.legend = F)+
   coord_polar("y")
 
 ## Fazendo Gráfico - Conta Própria que não contribui para INSS
 
 trab_conta_propria_n_INSS = data %>%
   filter(year_quarter %in% c("2019_1", "2019_2", "2019_3", "2019_4")) %>%
   select(social_security_taxpayer, higher_educ_level, year_quarter,
          job_function, worker) %>%
   filter(job_function == 6 & social_security_taxpayer == 2 & worker == 1)  %>%
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
 
 trab_conta_propria_n_INSS = trab_conta_propria_n_INSS%>%
   select(year_quarter, higher_educ_label, labell) %>%
   distinct() %>%
   mutate(labelll = case_when(higher_educ_label == "Uneducated" & year_quarter == "2019_1" ~ 6,
                              higher_educ_label == "Primary School Incompleted" & year_quarter == "2019_1" ~ 41,
                              higher_educ_label == "Primary School Completed" & year_quarter == "2019_1" ~ 10,
                              higher_educ_label == "High School Incompleted" & year_quarter == "2019_1" ~ 7,
                              higher_educ_label == "High School Completed" & year_quarter == "2019_1" ~ 26,
                              higher_educ_label == "College Degree Incompleted" & year_quarter == "2019_1" ~ 3,
                              higher_educ_label == "College Degree Completed" & year_quarter == "2019_1" ~ 8)) %>%
   filter(!is.na(labelll))
 
 grafico1 = ggplot(trab_conta_propria_n_INSS, aes(x = year_quarter, y = labelll,
               fill = factor(higher_educ_label))) +
   geom_bar(stat="identity") +
   scale_fill_manual(name = "Education Level",
                     values = carto_pal(name = "Temps")) +
   labs(x = "", y = "",
        title = "Education Level of Non-Social Security Contributors in 2019.") +
   theme_minimal() +
   theme(text = element_text(family = "LM Roman 10"),
         plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
         legend.title = element_blank(),
         axis.text = element_blank()) +
   geom_label(aes(x = 1.2, label = paste0(labell, "%")), position = position_stack(vjust = 0.5),
              show.legend = F)+
   coord_polar("y")
 
 ## Fazendo Gráfico Servidor Público que tem carteira assinada
 
 serv_cart = data %>%
   filter(year_quarter %in% c("2019_1", "2019_2", "2019_3", "2019_4")) %>%
   select(signed_work_card, higher_educ_level, year_quarter,
          job_function, worker) %>%
   filter(job_function == 4 & signed_work_card == 1 & worker == 1)  %>%
   mutate(higher_educ_label = case_when(higher_educ_level %in% c(1) ~"Uneducated",
                                        higher_educ_level %in% c(2) ~ "Primary School Incompleted",
                                        higher_educ_level %in% c(3) ~ "Primary School Completed",
                                        higher_educ_level %in% c(4) ~ "High School Incompleted",
                                        higher_educ_level %in% c(5) ~ "High School Completed",
                                        higher_educ_level %in% c(6) ~ "College Degree Incompleted",
                                        higher_educ_level %in% c(7) ~ "College Degree Completed"))  %>%
   group_by(higher_educ_level) %>% mutate(labels = n()) %>%
   mutate(labell = round(labels/nrow(.), digits = 2))

 serv_cart$labell = serv_cart$labell * 100 

 serv_cart = serv_cart %>%
   select(year_quarter, higher_educ_label, labell) %>%
   distinct() %>%
   mutate(labelll = case_when(higher_educ_label == "Uneducated" & year_quarter == "2019_1" ~ 1,
                              higher_educ_label == "Primary School Incompleted" & year_quarter == "2019_1" ~ 8,
                              higher_educ_label == "Primary School Completed" & year_quarter == "2019_1" ~ 4,
                              higher_educ_label == "High School Incompleted" & year_quarter == "2019_1" ~ 3,
                              higher_educ_label == "High School Completed" & year_quarter == "2019_1" ~ 29,
                              higher_educ_label == "College Degree Incompleted" & year_quarter == "2019_1" ~ 9,
                              higher_educ_label == "College Degree Completed" & year_quarter == "2019_1" ~ 47)) %>%
   filter(!is.na(labelll))
   
 n_grafico = ggplot(serv_cart, aes(x = "", y = labelll,
                               fill = higher_educ_label)) +
   geom_bar(stat = "identity")+
   scale_fill_manual(name = "Education Level",
                     values = carto_pal(name = "Sunset")) +
   labs(x = "", y = "",
        title = "Education Level of Registered Public Sector Employees in 2019") +
   theme_minimal() +
   theme(text = element_text(family = "LM Roman 10"),
         plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
         legend.title = element_blank(),
         axis.text = element_blank()) +
   geom_label(aes(x = 1.2, label = paste0(labelll, "%")), position = position_stack(vjust = 0.5),
              show.legend = F)+
   coord_polar("y")
 
 # Fazendo Gráfico Servidores Públicos sem Cart ass
 
 serv_s_cart = data %>%
   filter(year_quarter %in% c("2019_1", "2019_2", "2019_3", "2019_4")) %>%
   select(signed_work_card, higher_educ_level, year_quarter,
          job_function, worker) %>%
   filter(job_function == 4 & signed_work_card == 2 & worker == 1)  %>%
   mutate(higher_educ_label = case_when(higher_educ_level %in% c(1) ~"Uneducated",
                                        higher_educ_level %in% c(2) ~ "Primary School Incompleted",
                                        higher_educ_level %in% c(3) ~ "Primary School Completed",
                                        higher_educ_level %in% c(4) ~ "High School Incompleted",
                                        higher_educ_level %in% c(5) ~ "High School Completed",
                                        higher_educ_level %in% c(6) ~ "College Degree Incompleted",
                                        higher_educ_level %in% c(7) ~ "College Degree Completed"))  %>%
   group_by(higher_educ_level) %>% mutate(labels = n()) %>%
   mutate(labell = round(labels/nrow(.), digits = 2))
 
 serv_s_cart$labell = serv_s_cart$labell * 100 
 
 serv_s_cart = serv_s_cart %>%
   select(year_quarter, higher_educ_label, labell) %>%
   distinct() %>%
   mutate(labelll = case_when(higher_educ_label == "Uneducated" & year_quarter == "2019_1" ~ 1,
                              higher_educ_label == "Primary School Incompleted" & year_quarter == "2019_1" ~ 10,
                              higher_educ_label == "Primary School Completed" & year_quarter == "2019_1" ~ 3,
                              higher_educ_label == "High School Incompleted" & year_quarter == "2019_1" ~ 4,
                              higher_educ_label == "High School Completed" & year_quarter == "2019_1" ~ 27,
                              higher_educ_label == "College Degree Incompleted" & year_quarter == "2019_1" ~ 15,
                              higher_educ_label == "College Degree Completed" & year_quarter == "2019_1" ~ 38)) %>%
   filter(!is.na(labelll))
 
 
 nov_grafico = ggplot(serv_s_cart, aes(x = "", y = labelll,
                                    fill = higher_educ_label)) +
   geom_bar(stat = "identity")+
   scale_fill_manual(name = "Education Level",
                     values = carto_pal(name = "Sunset")) +
   labs(x = "", y = "",
        title = "Education Level of Non-Registered Public Sector Employees in 2019") +
   theme_minimal() +
   theme(text = element_text(family = "LM Roman 10"),
         plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
         legend.title = element_blank(),
         axis.text = element_blank()) +
   geom_label(aes(x = 1.2, label = paste0(labelll, "%")), position = position_stack(vjust = 0.5),
              show.legend = F)+
   coord_polar("y") 
 
 
 # Fazendo Gráfico Servidor Público que Contribui pro INSS
 
 serv_inss = data %>%
   filter(year_quarter %in% c("2019_1", "2019_2", "2019_3", "2019_4")) %>%
   select(signed_work_card, higher_educ_level, year_quarter,
          job_function, worker, social_security_taxpayer) %>%
   filter(job_function == 4 & social_security_taxpayer == 1 & worker == 1)  %>%
   mutate(higher_educ_label = case_when(higher_educ_level %in% c(1) ~"Uneducated",
                                        higher_educ_level %in% c(2) ~ "Primary School Incompleted",
                                        higher_educ_level %in% c(3) ~ "Primary School Completed",
                                        higher_educ_level %in% c(4) ~ "High School Incompleted",
                                        higher_educ_level %in% c(5) ~ "High School Completed",
                                        higher_educ_level %in% c(6) ~ "College Degree Incompleted",
                                        higher_educ_level %in% c(7) ~ "College Degree Completed"))  %>%
   group_by(higher_educ_level) %>% mutate(labels = n()) %>%
   mutate(labell = round(labels/nrow(.), digits = 2))
 
 serv_inss$labell = serv_inss$labell * 100 
 
 serv_inss = serv_inss %>%
   select(year_quarter, higher_educ_label, labell) %>%
   distinct() %>%
   mutate(labelll = case_when(higher_educ_label == "Uneducated" & year_quarter == "2019_1" ~ 1,
                              higher_educ_label == "Primary School Incompleted" & year_quarter == "2019_1" ~ 10,
                              higher_educ_label == "Primary School Completed" & year_quarter == "2019_1" ~ 4,
                              higher_educ_label == "High School Incompleted" & year_quarter == "2019_1" ~ 3,
                              higher_educ_label == "High School Completed" & year_quarter == "2019_1" ~ 28,
                              higher_educ_label == "College Degree Incompleted" & year_quarter == "2019_1" ~ 11,
                              higher_educ_label == "College Degree Completed" & year_quarter == "2019_1" ~ 44)) %>%
   filter(!is.na(labelll))
 
 
 novo_grafico = ggplot(serv_inss, aes(x = year_quarter, y = labelll,
                                    fill = higher_educ_label)) +
   geom_bar(stat = "identity")+
   scale_fill_manual(name = "Education Level",
                     values = carto_pal(name = "Tropic")) +
   labs(x = "", y = "",
        title = "Education Level of Public Sector Social Security Taxpayers in 2019") +
   theme_minimal() +
   theme(text = element_text(family = "LM Roman 10"),
         plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
         legend.title = element_blank(),
         axis.text = element_blank()) +
   geom_label(aes(x = 1.2, label = paste0(labelll, "%")), position = position_stack(vjust = 0.5),
              show.legend = F)+
   coord_polar("y") 

 
 # Fazendo Gráfico Servidores Públicos Não Contribuintes
 
 serv_n_inss = data %>%
   filter(year_quarter %in% c("2019_1", "2019_2", "2019_3", "2019_4")) %>%
   select(signed_work_card, higher_educ_level, year_quarter,
          job_function, worker, social_security_taxpayer) %>%
   filter(job_function == 4 & social_security_taxpayer == 2 & worker == 1)  %>%
   mutate(higher_educ_label = case_when(higher_educ_level %in% c(1) ~"Uneducated",
                                        higher_educ_level %in% c(2) ~ "Primary School Incompleted",
                                        higher_educ_level %in% c(3) ~ "Primary School Completed",
                                        higher_educ_level %in% c(4) ~ "High School Incompleted",
                                        higher_educ_level %in% c(5) ~ "High School Completed",
                                        higher_educ_level %in% c(6) ~ "College Degree Incompleted",
                                        higher_educ_level %in% c(7) ~ "College Degree Completed"))  %>%
   group_by(higher_educ_level) %>% mutate(labels = n()) %>%
   mutate(labell = round(labels/nrow(.), digits = 2))
 
 serv_n_inss$labell = serv_n_inss$labell * 100 
 
 serv_n_inss = serv_n_inss %>%
   select(year_quarter, higher_educ_label, labell) %>%
   distinct() %>%
   mutate(labelll = case_when(higher_educ_label == "Uneducated" & year_quarter == "2019_1" ~ 2,
                              higher_educ_label == "Primary School Incompleted" & year_quarter == "2019_1" ~ 12,
                              higher_educ_label == "Primary School Completed" & year_quarter == "2019_1" ~ 3,
                              higher_educ_label == "High School Incompleted" & year_quarter == "2019_1" ~ 7,
                              higher_educ_label == "High School Completed" & year_quarter == "2019_1" ~ 26,
                              higher_educ_label == "College Degree Incompleted" & year_quarter == "2019_1" ~ 25,
                              higher_educ_label == "College Degree Completed" & year_quarter == "2019_1" ~ 24)) %>%
   filter(!is.na(labelll))
 
 
 new_grafico = ggplot(serv_n_inss, aes(x = "", y = labelll,
                                      fill = higher_educ_label)) +
   geom_bar(stat = "identity")+
   scale_fill_manual(name = "Education Level",
                     values = carto_pal(name = "Tropic")) +
   labs(x = "", y = "",
        title = "Education Level of Public Sector Social Security Non-Taxpayers in 2019") +
   theme_minimal() +
   theme(text = element_text(family = "LM Roman 10"),
         plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
         legend.title = element_blank(),
         axis.text = element_blank()) +
   geom_label(aes(x = 1.2, label = paste0(labelll, "%")), position = position_stack(vjust = 0.5),
              show.legend = F)+
   coord_polar("y") 
 

 # Fazendo Gráfico Empregador Contribuinte
 
 empreg_inss = data %>%
   filter(year_quarter %in% c("2019_1", "2019_2", "2019_3", "2019_4")) %>%
   select(signed_work_card, higher_educ_level, year_quarter,
          job_function, worker, social_security_taxpayer) %>%
   filter(job_function == 5 & social_security_taxpayer == 1 & worker == 1)  %>%
   mutate(higher_educ_label = case_when(higher_educ_level %in% c(1) ~"Uneducated",
                                        higher_educ_level %in% c(2) ~ "Primary School Incompleted",
                                        higher_educ_level %in% c(3) ~ "Primary School Completed",
                                        higher_educ_level %in% c(4) ~ "High School Incompleted",
                                        higher_educ_level %in% c(5) ~ "High School Completed",
                                        higher_educ_level %in% c(6) ~ "College Degree Incompleted",
                                        higher_educ_level %in% c(7) ~ "College Degree Completed"))  %>%
   group_by(higher_educ_level) %>% mutate(labels = n()) %>%
   mutate(labell = round(labels/nrow(.), digits = 2))
 
 empreg_inss$labell = empreg_inss$labell * 100 
 
 empreg_inss = empreg_inss %>%
   select(year_quarter, higher_educ_label, labell) %>%
   distinct() %>%
   mutate(labelll = case_when(higher_educ_label == "Uneducated" & year_quarter == "2019_1" ~ 1,
                              higher_educ_label == "Primary School Incompleted" & year_quarter == "2019_1" ~ 13,
                              higher_educ_label == "Primary School Completed" & year_quarter == "2019_1" ~ 7,
                              higher_educ_label == "High School Incompleted" & year_quarter == "2019_1" ~ 4,
                              higher_educ_label == "High School Completed" & year_quarter == "2019_1" ~ 32,
                              higher_educ_label == "College Degree Incompleted" & year_quarter == "2019_1" ~ 7,
                              higher_educ_label == "College Degree Completed" & year_quarter == "2019_1" ~ 37)) %>%
   filter(!is.na(labelll))
 
 
 nuevo_grafico = ggplot(empreg_inss, aes(x = year_quarter, y = labelll,
                                    fill = higher_educ_label)) +
   geom_bar(stat = "identity")+
   scale_fill_manual(name = "Education Level",
                     values = carto_pal(name = "Earth")) +
   labs(x = "", y = "",
        title = "Education Level of Employers Social Security Taxpayers in 2019.") +
   theme_minimal() +
   theme(text = element_text(family = "LM Roman 10"),
         plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
         legend.title = element_blank(),
         axis.text = element_blank()) +
   geom_label(aes(x = 1.2, label = paste0(labelll, "%")), position = position_stack(vjust = 0.5),
              show.legend = F)+
   coord_polar("y")

 
 # Fazendo Gráfico Empregadores Não Contribuintes
 
 empreg_n_inss = data %>%
   filter(year_quarter %in% c("2019_1", "2019_2", "2019_3", "2019_4")) %>%
   select(signed_work_card, higher_educ_level, year_quarter,
          job_function, worker, social_security_taxpayer) %>%
   filter(job_function == 5 & social_security_taxpayer == 2 & worker == 1)  %>%
   mutate(higher_educ_label = case_when(higher_educ_level %in% c(1) ~"Uneducated",
                                        higher_educ_level %in% c(2) ~ "Primary School Incompleted",
                                        higher_educ_level %in% c(3) ~ "Primary School Completed",
                                        higher_educ_level %in% c(4) ~ "High School Incompleted",
                                        higher_educ_level %in% c(5) ~ "High School Completed",
                                        higher_educ_level %in% c(6) ~ "College Degree Incompleted",
                                        higher_educ_level %in% c(7) ~ "College Degree Completed"))  %>%
   group_by(higher_educ_level) %>% mutate(labels = n()) %>%
   mutate(labell = round(labels/nrow(.), digits = 2))
 
 empreg_n_inss$labell = empreg_n_inss$labell * 100 
 
 empreg_n_inss = empreg_n_inss %>%
   select(year_quarter, higher_educ_label, labell) %>%
   distinct() %>%
   mutate(labelll = case_when(higher_educ_label == "Uneducated" & year_quarter == "2019_1" ~ 3,
                              higher_educ_label == "Primary School Incompleted" & year_quarter == "2019_1" ~ 28,
                              higher_educ_label == "Primary School Completed" & year_quarter == "2019_1" ~ 8,
                              higher_educ_label == "High School Incompleted" & year_quarter == "2019_1" ~ 5,
                              higher_educ_label == "High School Completed" & year_quarter == "2019_1" ~ 30,
                              higher_educ_label == "College Degree Incompleted" & year_quarter == "2019_1" ~ 6,
                              higher_educ_label == "College Degree Completed" & year_quarter == "2019_1" ~ 20)) %>%
   filter(!is.na(labelll))
 
 
 nueva_grafico = ggplot(empreg_n_inss, aes(x = "", y = labelll,
                                        fill = higher_educ_label)) +
   geom_bar(stat = "identity")+
   scale_fill_manual(name = "Education Level",
                     values = carto_pal(name = "Earth")) +
   labs(x = "", y = "",
        title = "Education Level of Employers Social Security Non-Taxpayers in 2019") +
   theme_minimal() +
   theme(text = element_text(family = "LM Roman 10"),
         plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
         legend.title = element_blank(),
         axis.text = element_blank()) +
   geom_label(aes(x = 1.2, label = paste0(labelll, "%")), position = position_stack(vjust = 0.5),
              show.legend = F)+
   coord_polar("y") 

 
 # Fazendo Gráfico Inativos
 
 inativo = data %>%
   filter(year_quarter %in% c("2019_1", "2019_2", "2019_3", "2019_4")) %>%
   select(workforce_condition, higher_educ_level, year_quarter
          ) %>%
   filter(workforce_condition == 2)  %>%
   mutate(higher_educ_label = case_when(higher_educ_level %in% c(1) ~"Uneducated",
                                        higher_educ_level %in% c(2) ~ "Primary School Incompleted",
                                        higher_educ_level %in% c(3) ~ "Primary School Completed",
                                        higher_educ_level %in% c(4) ~ "High School Incompleted",
                                        higher_educ_level %in% c(5) ~ "High School Completed",
                                        higher_educ_level %in% c(6) ~ "College Degree Incompleted",
                                        higher_educ_level %in% c(7) ~ "College Degree Completed"))  %>%
   group_by(higher_educ_level) %>% mutate(labels = n()) %>%
   mutate(labell = round(labels/nrow(.), digits = 2))
 
 inativo$labell = inativo$labell * 100 
 
 inativo = inativo %>%
   select(year_quarter, higher_educ_label, labell) %>%
   distinct() %>%
   mutate(labelll = case_when(higher_educ_label == "Uneducated" & year_quarter == "2019_1" ~ 12,
                              higher_educ_label == "Primary School Incompleted" & year_quarter == "2019_1" ~ 44,
                              higher_educ_label == "Primary School Completed" & year_quarter == "2019_1" ~ 10,
                              higher_educ_label == "High School Incompleted" & year_quarter == "2019_1" ~ 9,
                              higher_educ_label == "High School Completed" & year_quarter == "2019_1" ~ 17,
                              higher_educ_label == "College Degree Incompleted" & year_quarter == "2019_1" ~ 3,
                              higher_educ_label == "College Degree Completed" & year_quarter == "2019_1" ~ 5)) %>%
   filter(!is.na(labelll))
 
 
 grafico_inat = ggplot(inativo, aes(x = "", y = labelll,
                                fill = higher_educ_label)) +
   geom_bar(stat = "identity")+
   scale_fill_manual(name = "Education Level",
                     values = carto_pal(name = "ArmyRose")) +
   labs(x = "", y = "",
        title = "Education Level of Inactives in 2019") +
   theme_minimal() +
   theme(text = element_text(family = "LM Roman 10"),
         plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
         legend.title = element_blank(),
         axis.text = element_blank()) +
   geom_label(aes(x = 1.2, label = paste0(labell, "%")), position = position_stack(vjust = 0.5),
              show.legend = F)+
   coord_polar("y")

 
 ###############
 #### Talvez Utilize #######
 sal_ocup = data_2019 %>%
   filter(worker == 1) %>%
   select(worker, monthly_work_income, job_function, higher_educ_level,
          year_quarter) %>%
   mutate(higher_educ_label = case_when(higher_educ_level %in% c(1) ~"Uneducated",
                                        higher_educ_level %in% c(2) ~ "Primary School Incompleted",
                                        higher_educ_level %in% c(3) ~ "Primary School Completed",
                                        higher_educ_level %in% c(4) ~ "High School Incompleted",
                                        higher_educ_level %in% c(5) ~ "High School Completed",
                                        higher_educ_level %in% c(6) ~ "College Degree Incompleted",
                                        higher_educ_level %in% c(7) ~ "College Degree Completed"))  %>%
   group_by(higher_educ_level) %>% mutate(labels = n()) %>%
   mutate(labell = round(labels/nrow(.), digits = 2))
 
 sal_ocup$labell = sal_ocup$labell * 100
 
 sal_ocup = sal_ocup %>%
   select(year_quarter, higher_educ_label, monthly_work_income, higher_educ_level, labell) %>%
   mutate(media = mean(monthly_work_income, na.rm = T)) %>%
   mutate(media = round(media, digits = 2)) %>%
   distinct() %>%
   mutate(labelll = case_when(higher_educ_label == "Uneducated" & year_quarter == "2019_1" ~ 852.15,
                              higher_educ_label == "Primary School Incompleted" & year_quarter == "2019_1" ~ 1191.23,
                              higher_educ_label == "Primary School Completed" & year_quarter == "2019_1" ~ 1311.58,
                              higher_educ_label == "High School Incompleted" & year_quarter == "2019_1" ~ 1450.06,
                              higher_educ_label == "High School Completed" & year_quarter == "2019_1" ~ 1708.93,
                              higher_educ_label == "College Degree Incompleted" & year_quarter == "2019_1" ~ 2076.44,
                              higher_educ_label == "College Degree Completed" & year_quarter == "2019_1" ~ 4421.48)) %>%
   filter(!is.na(labelll)) %>%
   select(higher_educ_level, higher_educ_label, labelll) %>%
   distinct()
 

 grafico_sal = ggplot(sal_ocup, aes(x = higher_educ_level, y = labelll,
                                        fill = higher_educ_label)) +
   geom_bar(stat = "identity")+
   scale_fill_manual(name = "Education Level",
                     values = carto_pal(name = "Vivid")) +
   labs(x = "", y = "",
        title = "Average Monthly Labor Earnings by Education Level in 2019") +
   theme_minimal() +
   theme(text = element_text(family = "LM Roman 10"),
         plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
         legend.title = element_blank(),
         axis.text = element_blank()) +
   geom_label(aes(label = paste0("R$",labelll)), position = position_dodge(width = 0.9), vjust = -0.25,
              show.legend = F)

 
 #####
 
 sal_ocupacoes = data_2019 %>%
   filter(worker == 1) %>%
   select(worker, monthly_work_income, job_function, higher_educ_level,
          year_quarter) %>%
   mutate(higher_educ_label = case_when(higher_educ_level %in% c(1) ~"Uneducated",
                                        higher_educ_level %in% c(2) ~ "Primary School Incompleted",
                                        higher_educ_level %in% c(3) ~ "Primary School Completed",
                                        higher_educ_level %in% c(4) ~ "High School Incompleted",
                                        higher_educ_level %in% c(5) ~ "High School Completed",
                                        higher_educ_level %in% c(6) ~ "College Degree Incompleted",
                                        higher_educ_level %in% c(7) ~ "College Degree Completed"))  %>%
   group_by(higher_educ_level) %>% mutate(labels = n()) %>%
   mutate(labell = round(labels/nrow(.), digits = 2))
 
 sal_ocupacoes$labell = sal_ocupacoes$labell * 100
 
 sal_ocupacoes = sal_ocupacoes %>%
   select(year_quarter, job_function, higher_educ_label, monthly_work_income, labell) %>%
   group_by(job_function) %>% mutate(labelss = n())%>%
   mutate(media = mean(monthly_work_income, na.rm = T)) %>%
   mutate(media = round(media, digits = 2)) %>%
   mutate(position = case_when(job_function %in% c(1) ~ "Domestic",
                               job_function %in% c(2) ~ "Military",
                               job_function %in% c(3) ~ "Private Sector",
                               job_function %in% c(4) ~ "Public Sector",
                               job_function %in% c(5) ~ "Employers",
                               job_function %in% c(6) ~ "Self-Employed")) %>%
   mutate(position_money = case_when(position == "Domestic" ~ 876.26,
                                     position == "Military" ~ 5017.31,
                                     position == "Private Sector" ~ 1777.25,
                                     position == "Public Sector" ~ 3329.98,
                                     position == "Employers" ~ 5165.38,
                                     position == "Self-Employed" ~ 1543.77))
 
 sal_ocupacoes = sal_ocupacoes %>%
   select(position, position_money) %>%
   distinct()
 
 
 grafico_sal_ocupacoes = ggplot(sal_ocupacoes, aes(x = job_function, y = position_money,
                                                   fill = position)) +
   geom_bar(stat = "identity")+
   scale_fill_manual(name = "Education Level",
                     values = carto_pal(name = "Pastel")) +
   labs(x = "", y = "",
        title = "Average Monthly Labor Earnings by Job Occupation in 2019") +
   theme_minimal() +
   theme(text = element_text(family = "LM Roman 10"),
         plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
         legend.title = element_blank(),
         axis.text = element_blank()) +
   geom_label(aes(label = paste0("R$",position_money)), position = position_dodge(width = 0.9), vjust = -0.25
              ,show.legend = F)
 
 
 