library(extrafont)

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

 #### Fazendo Gr?fico de Desempregados #####
 
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
 ########################################################################
 ######################################################################

 ## Fazendo Gr?fico - Conta Pr?pria que contr?buia INSS
 
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
                     values = carto_pal(name = "Prism")) +
   labs(x = "", y = "",
        title = "Education Level of Self-Employed Social Security Contributors in 2019") +
   theme_minimal() +
   theme(text = element_text(family = "LM Roman 10"),
         plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
         legend.title = element_blank(),
         axis.text = element_blank()) +
   geom_label(aes(x = 1.2, label = paste0(labelll, "%")), position = position_stack(vjust = 0.5),
              show.legend = F)+
   coord_polar("y")
 
 ## Fazendo Gr?fico - Conta Pr?pria que n?o contribui para INSS
 
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
                     values = carto_pal(name = "Prism")) +
   labs(x = "", y = "",
        title = "Education Level of Self-Employed Non-Social Security Contributors in 2019") +
   theme_minimal() +
   theme(text = element_text(family = "LM Roman 10"),
         plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
         legend.title = element_blank(),
         axis.text = element_blank()) +
   geom_label(aes(x = 1.2, label = paste0(labell, "%")), position = position_stack(vjust = 0.5),
              show.legend = F)+
   coord_polar("y")
 
 ## Fazendo Gr?fico Servidor P?blico que tem carteira assinada
 
 serv_cart = data %>%
   filter(year_quarter %in% c("2019_1", "2019_2", "2019_3", "2019_4")) %>%
   select(signed_work_card, higher_educ_level, year_quarter,
          job_function, worker, work_category) %>%
   filter(job_function == 4 & work_category %in% c(5,7) & worker == 1)  %>%
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
                     values = carto_pal(name = "Prism")) +
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
 
 # Fazendo Gr?fico Servidores P?blicos sem Cart ass
 
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
                     values = carto_pal(name = "Prism")) +
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
 

 # Fazendo Gr?fico Empregador Formal
 
 empreg_formal = data %>%
   filter(year_quarter %in% c("2019_1", "2019_2", "2019_3", "2019_4")) %>%
   select(signed_work_card, higher_educ_level, year_quarter,
          job_function, worker, cnpj) %>%
   filter(job_function == 5 & cnpj == 1 & worker == 1)  %>%
   mutate(higher_educ_label = case_when(higher_educ_level %in% c(1) ~"Uneducated",
                                        higher_educ_level %in% c(2) ~ "Primary School Incompleted",
                                        higher_educ_level %in% c(3) ~ "Primary School Completed",
                                        higher_educ_level %in% c(4) ~ "High School Incompleted",
                                        higher_educ_level %in% c(5) ~ "High School Completed",
                                        higher_educ_level %in% c(6) ~ "College Degree Incompleted",
                                        higher_educ_level %in% c(7) ~ "College Degree Completed"))  %>%
   group_by(higher_educ_level) %>% mutate(labels = n()) %>%
   mutate(labell = round(labels/nrow(.), digits = 2))
 
 empreg_formal$labell = empreg_formal$labell * 100 
 
 empreg_formal = empreg_formal %>%
   select(year_quarter, higher_educ_label, labell) %>%
   distinct() %>%
   mutate(labelll = case_when(higher_educ_label == "Uneducated" & year_quarter == "2019_1" ~ 1,
                              higher_educ_label == "Primary School Incompleted" & year_quarter == "2019_1" ~ 12,
                              higher_educ_label == "Primary School Completed" & year_quarter == "2019_1" ~ 7,
                              higher_educ_label == "High School Incompleted" & year_quarter == "2019_1" ~ 4,
                              higher_educ_label == "High School Completed" & year_quarter == "2019_1" ~ 32,
                              higher_educ_label == "College Degree Incompleted" & year_quarter == "2019_1" ~ 7,
                              higher_educ_label == "College Degree Completed" & year_quarter == "2019_1" ~ 37)) %>%
   filter(!is.na(labelll))
 
 
 nuevo_grafico = ggplot(empreg_formal, aes(x = year_quarter, y = labelll,
                                    fill = higher_educ_label)) +
   geom_bar(stat = "identity")+
   scale_fill_manual(name = "Education Level",
                     values = carto_pal(name = "Prism")) +
   labs(x = "", y = "",
        title = "Education Level of Formal Employers in 2019") +
   theme_minimal() +
   theme(text = element_text(family = "LM Roman 10"),
         plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
         legend.title = element_blank(),
         axis.text = element_blank()) +
   geom_label(aes(x = 1.2, label = paste0(labelll, "%")), position = position_stack(vjust = 0.5),
              show.legend = F)+
   coord_polar("y")

 
 # Fazendo Gr?fico Empregadores Informais
 
 empreg_informal = data %>%
   filter(year_quarter %in% c("2019_1", "2019_2", "2019_3", "2019_4")) %>%
   select(signed_work_card, higher_educ_level, year_quarter,
          job_function, worker, cnpj) %>%
   filter(job_function == 5 & cnpj == 2 & worker == 1)  %>%
   mutate(higher_educ_label = case_when(higher_educ_level %in% c(1) ~"Uneducated",
                                        higher_educ_level %in% c(2) ~ "Primary School Incompleted",
                                        higher_educ_level %in% c(3) ~ "Primary School Completed",
                                        higher_educ_level %in% c(4) ~ "High School Incompleted",
                                        higher_educ_level %in% c(5) ~ "High School Completed",
                                        higher_educ_level %in% c(6) ~ "College Degree Incompleted",
                                        higher_educ_level %in% c(7) ~ "College Degree Completed"))  %>%
   group_by(higher_educ_level) %>% mutate(labels = n()) %>%
   mutate(labell = round(labels/nrow(.), digits = 2))
 
 empreg_informal$labell = empreg_informal$labell * 100 
 
 empreg_informal = empreg_informal %>%
   select(year_quarter, higher_educ_label, labell) %>%
   distinct() %>%
   mutate(labelll = case_when(higher_educ_label == "Uneducated" & year_quarter == "2019_1" ~ 3,
                              higher_educ_label == "Primary School Incompleted" & year_quarter == "2019_1" ~ 36,
                              higher_educ_label == "Primary School Completed" & year_quarter == "2019_1" ~ 9,
                              higher_educ_label == "High School Incompleted" & year_quarter == "2019_1" ~ 6,
                              higher_educ_label == "High School Completed" & year_quarter == "2019_1" ~ 27,
                              higher_educ_label == "College Degree Incompleted" & year_quarter == "2019_1" ~ 3,
                              higher_educ_label == "College Degree Completed" & year_quarter == "2019_1" ~ 16)) %>%
   filter(!is.na(labelll))
 
 
 nueva_grafico = ggplot(empreg_informal, aes(x = "", y = labelll,
                                        fill = higher_educ_label)) +
   geom_bar(stat = "identity")+
   scale_fill_manual(name = "Education Level",
                     values = carto_pal(name = "Prism")) +
   labs(x = "", y = "",
        title = "Education Level of Informal Employers in 2019") +
   theme_minimal() +
   theme(text = element_text(family = "LM Roman 10"),
         plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
         legend.title = element_blank(),
         axis.text = element_blank()) +
   geom_label(aes(x = 1.2, label = paste0(labelll, "%")), position = position_stack(vjust = 0.5),
              show.legend = F)+
   coord_polar("y") 

 
 # Fazendo Gr?fico Inativos
 
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
                     values = carto_pal(name = "Prism")) +
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
 
 
 ## Fazendo gr?fico setor privado formal
 
 privado_formal = data %>%
   filter(year_quarter %in% c("2019_1", "2019_2", "2019_3", "2019_4")) %>%
   select(signed_work_card, higher_educ_level, year_quarter,
          job_function, worker) %>%
   filter(job_function == 3 & signed_work_card == 1 & worker == 1)  %>%
   mutate(higher_educ_label = case_when(higher_educ_level %in% c(1) ~"Uneducated",
                                        higher_educ_level %in% c(2) ~ "Primary School Incompleted",
                                        higher_educ_level %in% c(3) ~ "Primary School Completed",
                                        higher_educ_level %in% c(4) ~ "High School Incompleted",
                                        higher_educ_level %in% c(5) ~ "High School Completed",
                                        higher_educ_level %in% c(6) ~ "College Degree Incompleted",
                                        higher_educ_level %in% c(7) ~ "College Degree Completed"))  %>%
   group_by(higher_educ_level) %>% mutate(labels = n()) %>%
   mutate(labell = round(labels/nrow(.), digits = 2))
 
 privado_formal$labell = privado_formal$labell * 100 
 
privado_formal = privado_formal %>%
   select(year_quarter, higher_educ_label, labell) %>%
   distinct() %>%
   mutate(labelll = case_when(higher_educ_label == "Uneducated" & year_quarter == "2019_1" ~ 1,
                              higher_educ_label == "Primary School Incompleted" & year_quarter == "2019_1" ~ 17,
                              higher_educ_label == "Primary School Completed" & year_quarter == "2019_1" ~ 8,
                              higher_educ_label == "High School Incompleted" & year_quarter == "2019_1" ~ 7,
                              higher_educ_label == "High School Completed" & year_quarter == "2019_1" ~ 42,
                              higher_educ_label == "College Degree Incompleted" & year_quarter == "2019_1" ~ 8,
                              higher_educ_label == "College Degree Completed" & year_quarter == "2019_1" ~ 18)) %>%
   filter(!is.na(labelll))
 
 
 priv_graf = ggplot(privado_formal, aes(x = "", y = labelll,
                                             fill = higher_educ_label)) +
   geom_bar(stat = "identity")+
   scale_fill_manual(name = "Education Level",
                     values = carto_pal(name = "Prism")) +
   labs(x = "", y = "",
        title = "Education Level of Formal Private Sector Employees in 2019") +
   theme_minimal() +
   theme(text = element_text(family = "LM Roman 10"),
         plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
         legend.title = element_blank(),
         axis.text = element_blank()) +
   geom_label(aes(x = 1.2, label = paste0(labelll, "%")), position = position_stack(vjust = 0.5),
              show.legend = F)+
   coord_polar("y")
 
 
 ## Fazendo gr?fico setor privado informal
 
 privado_informal = data %>%
   filter(year_quarter %in% c("2019_1", "2019_2", "2019_3", "2019_4")) %>%
   select(signed_work_card, higher_educ_level, year_quarter,
          job_function, worker) %>%
   filter(job_function == 3 & signed_work_card == 2 & worker == 1)  %>%
   mutate(higher_educ_label = case_when(higher_educ_level %in% c(1) ~"Uneducated",
                                        higher_educ_level %in% c(2) ~ "Primary School Incompleted",
                                        higher_educ_level %in% c(3) ~ "Primary School Completed",
                                        higher_educ_level %in% c(4) ~ "High School Incompleted",
                                        higher_educ_level %in% c(5) ~ "High School Completed",
                                        higher_educ_level %in% c(6) ~ "College Degree Incompleted",
                                        higher_educ_level %in% c(7) ~ "College Degree Completed"))  %>%
   group_by(higher_educ_level) %>% mutate(labels = n()) %>%
   mutate(labell = round(labels/nrow(.), digits = 2))
 
 privado_informal$labell = privado_informal$labell * 100 
 
 privado_informal = privado_informal %>%
   select(year_quarter, higher_educ_label, labell) %>%
   distinct() %>%
   mutate(labelll = case_when(higher_educ_label == "Uneducated" & year_quarter == "2019_1" ~ 4,
                              higher_educ_label == "Primary School Incompleted" & year_quarter == "2019_1" ~ 33,
                              higher_educ_label == "Primary School Completed" & year_quarter == "2019_1" ~ 10,
                              higher_educ_label == "High School Incompleted" & year_quarter == "2019_1" ~ 10,
                              higher_educ_label == "High School Completed" & year_quarter == "2019_1" ~ 28,
                              higher_educ_label == "College Degree Incompleted" & year_quarter == "2019_1" ~ 6,
                              higher_educ_label == "College Degree Completed" & year_quarter == "2019_1" ~ 9)) %>%
   filter(!is.na(labelll))
 
 
 priv_inf_graf = ggplot(privado_informal, aes(x = "", y = labelll,
                                        fill = higher_educ_label)) +
   geom_bar(stat = "identity")+
   scale_fill_manual(name = "Education Level",
                     values = carto_pal(name = "Prism")) +
   labs(x = "", y = "",
        title = "Education Level of Informal Private Sector Employees in 2019") +
   theme_minimal() +
   theme(text = element_text(family = "LM Roman 10"),
         plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
         legend.title = element_blank(),
         axis.text = element_blank()) +
   geom_label(aes(x = 1.2, label = paste0(labelll, "%")), position = position_stack(vjust = 0.5),
              show.legend = F)+
   coord_polar("y")

 
 ###############
 #### Talvez Utilize #######
 
 ### Gr?fico Sal?rio por educa??o
 
 
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
   mutate(labelll = case_when(higher_educ_label == "Uneducated" & year_quarter == "2019_1" ~ 830.65,
                              higher_educ_label == "Primary School Incompleted" & year_quarter == "2019_1" ~ 1185.57,
                              higher_educ_label == "Primary School Completed" & year_quarter == "2019_1" ~ 1319.31,
                              higher_educ_label == "High School Incompleted" & year_quarter == "2019_1" ~ 1463.82,
                              higher_educ_label == "High School Completed" & year_quarter == "2019_1" ~ 1755.71,
                              higher_educ_label == "College Degree Incompleted" & year_quarter == "2019_1" ~ 2140.51,
                              higher_educ_label == "College Degree Completed" & year_quarter == "2019_1" ~ 4609.40)) %>%
   filter(!is.na(labelll)) %>%
   select(higher_educ_level, higher_educ_label, labelll) %>%
   distinct()
 

 grafico_sal = ggplot(sal_ocup, aes(x = higher_educ_level, y = labelll,
                                        fill = higher_educ_label)) +
   geom_bar(stat = "identity")+
   scale_fill_manual(name = "Education Level",
                     values = carto_pal(name = "Prism")) +
   labs(x = "", y = "",
        title = "Average Monthly Labor Earnings by Education Level in 2019") +
   theme_minimal() +
   theme(text = element_text(family = "LM Roman 10"),
         plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
         legend.title = element_blank(),
         axis.text = element_blank()) +
   geom_label(aes(label = paste0("R$",labelll)), position = position_dodge(width = 0.9), vjust = -0.25,
              show.legend = F)

 
 ##### Gr?fico sal?rio por ocupa??o
 
 sal_ocupacoes = data_2019 %>%
   filter(worker == 1) %>%
   select(worker, monthly_work_income, job_function, higher_educ_level,
          year_quarter, work_category) %>%
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
   select(year_quarter, job_function, higher_educ_label, monthly_work_income, labell,
          work_category) %>%
   group_by(job_function) %>% mutate(labelss = n())%>%
   mutate(media = mean(monthly_work_income, na.rm = T)) %>%
   mutate(media = round(media, digits = 2)) %>%
   mutate(position = case_when(job_function %in% c(1) ~ "Domestic",
                               job_function %in% c(3) ~ "Private Sector",
                               job_function %in% c(4) & work_category %in% c(5,7) ~ "Public Sector",
                               job_function %in% c(5) ~ "Employers",
                               job_function %in% c(6) ~ "Self-Employed")) %>%
   mutate(position_money = case_when(position == "Domestic" ~ 894.41,
                                     position == "Private Sector" ~ 1872.02,
                                     position == "Public Sector" ~ 3525.95,
                                     position == "Employers" ~ 5093.79,
                                     position == "Self-Employed" ~ 1489.25))
 
 sal_ocupacoes = sal_ocupacoes %>%
   select(position, position_money) %>%
   distinct()
 
 
 grafico_sal_ocupacoes = ggplot(sal_ocupacoes, aes(x = job_function, y = position_money,
                                                   fill = position)) +
   geom_bar(stat = "identity")+
   scale_fill_manual(name = "Education Level",
                     values = carto_pal(name = "Prism")) +
   labs(x = "", y = "",
        title = "Average Monthly Labor Earnings by Job Occupation in 2019") +
   theme_minimal() +
   theme(text = element_text(family = "LM Roman 10"),
         plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
         legend.title = element_blank(),
         axis.text = element_blank()) +
   geom_label(aes(label = paste0("R$",position_money)), position = position_dodge(width = 0.9), vjust = -0.25
              ,show.legend = F)
 
 
 
 ## Gráfico Matrizes
 
 matrizes = read.csv("./Transitions Matrices/transicoes_por_educ.csv")
 
 ## Formal - Desempregado
 
 matriz1 = matrizes %>%
   filter(posicao_inicial == 1 &
            posicao_final == 4) %>%
   group_by(trim, educ) %>%
   summarise(transition = sum(transition)) %>%
   mutate(educ = as.character(educ))
 
 graf_f_d = ggplot(matriz1, aes(x = trim, y = transition, 
                                color = educ, group = educ)) +
   scale_color_manual(labels = c("Uneducated and Incompleted Primary School",
                                 "Completed Primary School and Incompleted High School",
                                 "Completed High School and Incompleted College Degree",
                                 "Completed College Degree"), values = carto_pal(name = "Vivid")) +
   geom_line(size = 2) +
   geom_point(size = 2) +
   labs(x = "", y = "", title = "Formal to Unemployed") +
   theme_minimal() +
   theme(text = element_text(family = "LM Roman 10"),
         plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
         legend.title = element_blank()) +
   scale_x_discrete(breaks = paste0(2012:2021, "_1"),
                    labels = 2012:2021) +
   scale_y_continuous(labels = scales::percent)
 
 
 
 ## Informal - Desempregado
 
 matriz2 = matrizes %>%
   filter(posicao_inicial == 2 &
            posicao_final == 4) %>%
   group_by(trim, educ) %>%
   summarise(transition = sum(transition)) %>%
   mutate(educ = as.character(educ))
 
 graf_i_d = ggplot(matriz2, aes(x = trim, y = transition, 
                                color = educ, group = educ)) +
   scale_color_manual(labels = c("Uneducated and Incompleted Primary School",
                                 "Completed Primary School and Incompleted High School",
                                 "Completed High School and Incompleted College Degree",
                                 "Completed College Degree"), values = carto_pal(name = "Vivid")) +
   geom_line(size = 2) +
   geom_point(size = 2) +
   labs(x = "", y = "", title = "Informal to Unemployed") +
   theme_minimal() +
   theme(text = element_text(family = "LM Roman 10"),
         plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
         legend.title = element_blank()) +
   scale_x_discrete(breaks = paste0(2012:2021, "_1"),
                    labels = 2012:2021) +
   scale_y_continuous(labels = scales::percent)
 
 
 
 ## Formal - Informal
 
 matriz3 = matrizes %>%
   filter(posicao_inicial == 1 &
            posicao_final == 2) %>%
   group_by(trim, educ) %>%
   summarise(transition = sum(transition)) %>%
   mutate(educ = as.character(educ))
 
 graf_f_i = ggplot(matriz3, aes(x = trim, y = transition, 
                                color = educ, group = educ)) +
   scale_color_manual(labels = c("Uneducated and Incompleted Primary School",
                                 "Completed Primary School and Incompleted High School",
                                 "Completed High School and Incompleted College Degree",
                                 "Completed College Degree"), values = carto_pal(name = "Vivid")) +
   geom_line(size = 2) +
   geom_point(size = 2) +
   labs(x = "", y = "", title = "Formal to Informal") +
   theme_minimal() +
   theme(text = element_text(family = "LM Roman 10"),
         plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
         legend.title = element_blank()) +
   scale_x_discrete(breaks = paste0(2012:2021, "_1"),
                    labels = 2012:2021) +
   scale_y_continuous(labels = scales::percent)
 
 
 ## Formal - Inativo
 
 matriz4 = matrizes %>%
   filter(posicao_inicial == 1 &
            posicao_final == 3) %>%
   group_by(trim, educ) %>%
   summarise(transition = sum(transition)) %>%
   mutate(educ = as.character(educ))
 
 graf_f_inat = ggplot(matriz4, aes(x = trim, y = transition, 
                                   color = educ, group = educ)) +
   scale_color_manual(labels = c("Uneducated and Incompleted Primary School",
                                 "Completed Primary School and Incompleted High School",
                                 "Completed High School and Incompleted College Degree",
                                 "Completed College Degree"), values = carto_pal(name = "Vivid")) +
   geom_line(size = 2) +
   geom_point(size = 2) +
   labs(x = "", y = "", title = "Formal to Inactive") +
   theme_minimal() +
   theme(text = element_text(family = "LM Roman 10"),
         plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
         legend.title = element_blank()) +
   scale_x_discrete(breaks = paste0(2012:2021, "_1"),
                    labels = 2012:2021) +
   scale_y_continuous(labels = scales::percent)
 
 
 ## Informal - Inativo
 
 matriz5 = matrizes %>%
   filter(posicao_inicial == 2 &
            posicao_final == 3) %>%
   group_by(trim, educ) %>%
   summarise(transition = sum(transition)) %>%
   mutate(educ = as.character(educ))
 
 graf_i_inat = ggplot(matriz5, aes(x = trim, y = transition, 
                                   color = educ, group = educ)) +
   scale_color_manual(labels = c("Uneducated and Incompleted Primary School",
                                 "Completed Primary School and Incompleted High School",
                                 "Completed High School and Incompleted College Degree",
                                 "Completed College Degree"), values = carto_pal(name = "Vivid")) +
   geom_line(size = 2) +
   geom_point(size = 2) +
   labs(x = "", y = "", title = "Informal to Inactive") +
   theme_minimal() +
   theme(text = element_text(family = "LM Roman 10"),
         plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
         legend.title = element_blank()) +
   scale_x_discrete(breaks = paste0(2012:2021, "_1"),
                    labels = 2012:2021) +
   scale_y_continuous(labels = scales::percent)
 
 