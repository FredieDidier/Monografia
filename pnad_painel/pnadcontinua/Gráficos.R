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

 graf.1 = ggplot(f, aes(x = year_quarter, y = signed_work_card,
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
 
 gg = data %>%
   filter(year_quarter %in% c("2019_1", "2019_2", "2019_3", "2019_4") &
            worker == 1 & signed_work_card == 2) %>%
   select(worker, higher_educ_level, year_quarter, signed_work_card) %>%
   mutate(higher_educ_label = case_when(higher_educ_level %in% c(1) ~"Uneducated",
                                        higher_educ_level %in% c(2) ~ "Primary School Incompleted",
                                        higher_educ_level %in% c(3) ~ "Primary School Completed",
                                        higher_educ_level %in% c(4) ~ "High School Incompleted",
                                        higher_educ_level %in% c(5) ~ "High School Completed",
                                        higher_educ_level %in% c(6) ~ "College Degree Incompleted",
                                        higher_educ_level %in% c(7) ~ "College Degree Completed")) 
 
 inf1 = ggplot(gg, aes(x = year_quarter, y = worker,
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
 
 g1 = data %>%
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
 
 desemp = ggplot(g1, aes(x = year_quarter, y = worker,
               fill = factor(higher_educ_label))) +
   geom_bar(stat = "identity") +
   scale_fill_manual(name = "Education Level",
                     values = carto_pal(name = "Prism")) +
   scale_y_continuous(labels = scales::comma) +
   labs(x = "Quarter", y = "Non-Workers",
        title = "Education Level of the Unemployed in 2019") +
   theme_minimal() +
   theme(text = element_text(family = "LM Roman 10"),
         plot.title = element_text(size = 13, face = "bold", hjust = 0.5))
 #########################################################################
 ########## Fazendo Gráfico de trabalhadores sem carteira e que não contribuem pro INSS ###
 
 g2 = data %>%
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
 
 inf2 = ggplot(g2, aes(x = year_quarter, y = worker,
                         fill = factor(higher_educ_label))) +
   geom_bar(stat = "identity") +
   scale_fill_manual(name = "Education Level",
                     values = carto_pal(name = "Prism")) +
   scale_y_continuous(labels = scales::comma) +
   labs(x = "Quarter", y = "Non-Workers",
        title = "Education Level of Informal Workers in 2019") +
   theme_minimal() +
   theme(text = element_text(family = "LM Roman 10"),
         plot.title = element_text(size = 13, face = "bold", hjust = 0.5))
 
 ## Fazendo Gráfico - trabalhadores que contribuem pro INSS + escolaridade
 
 h = data %>%
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
 
 
 graf.2 = ggplot(h, aes(x = year_quarter, y = social_security_taxpayer,
               fill = factor(higher_educ_label))) +
   geom_bar(stat = "identity") +
   scale_fill_manual(name = "Education Level",
                     values = carto_pal(name = "Bold")) +
   labs(x = "Quarter", y = "Contributors",
        title = "Education Level of Social Security Taxpayers in 2019") +
   theme_minimal()+
   theme(text = element_text(family = "LM Roman 10"),
         plot.title = element_text(size = 13, face = "bold", hjust = 0.5))
 ########################################################################
 ######### Fazendo Gráfico trabalhadores que não contribuem para o INSS (informais) ###
 h1 = data %>%
   filter(year_quarter %in% c("2019_1", "2019_2", "2019_3", "2019_4") &
            worker == 1 & social_security_taxpayer == 2) %>%
   select(worker, social_security_taxpayer, higher_educ_level, year_quarter) %>%
   mutate(higher_educ_label = case_when(higher_educ_level %in% c(1) ~"Uneducated",
                                        higher_educ_level %in% c(2) ~ "Primary School Incompleted",
                                        higher_educ_level %in% c(3) ~ "Primary School Completed",
                                        higher_educ_level %in% c(4) ~ "High School Incompleted",
                                        higher_educ_level %in% c(5) ~ "High School Completed",
                                        higher_educ_level %in% c(6) ~ "College Degree Incompleted",
                                        higher_educ_level %in% c(7) ~ "College Degree Completed")) 
 
 
 inf3 = ggplot(h1, aes(x = year_quarter, y = social_security_taxpayer,
               fill = factor(higher_educ_label))) +
   geom_bar(stat = "identity") +
   scale_fill_manual(name = "Education Level",
                     values = carto_pal(name = "Vivid")) +
   labs(x = "Quarter", y = "Non-Contributors",
        title = "Education Level of Non-Social Security Taxpayers Workers") +
   theme_minimal()+
   theme(text = element_text(family = "LM Roman 10"),
         plot.title = element_text(size = 13, face = "bold", hjust = 0.5))
 ######################################################################

 ## Fazendo Gráfico - Conta Própria que contríbuia INSS
 
 i = data %>%
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
   mutate(labell = round(labels/nrow(i), digits = 2))
 
 i$labell = i$labell * 100 
 
 dddd = i%>%
   select(year_quarter, higher_educ_label, labell) %>%
   distinct()
 
 
 
 graf_1 = ggplot(dddd, aes(x = year_quarter, y = labell,
               fill = factor(higher_educ_label))) +
   geom_bar(stat="identity") +
   scale_fill_startrek(name = "Education Level") +
   labs(x = "Quarter", y = "Self-Employed Taxpayers",
        title = "Education Level of Social Security Contributors in 2019.1") +
   theme_minimal() +
   theme(text = element_text(family = "LM Roman 10"),
         plot.title = element_text(size = 13, face = "bold", hjust = 0.5)) +
   geom_label(aes(label = paste0(labell, "%")), position = position_stack(vjust = 0.5),
              show.legend = F)+
   coord_polar("y")
 
 ## Fazendo Gráfico - Conta Própria que não contribui para INSS
 
 i1 = data %>%
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
   mutate(labell = round(labels/nrow(i1), digits = 2))
 
 i1$labell = i1$labell * 100 
 
 ddd = i1%>%
   select(year_quarter, higher_educ_label, labell) %>%
   distinct()
 
 
 
 grafico1 = ggplot(ddd, aes(x = year_quarter, y = labell,
               fill = factor(higher_educ_label))) +
   geom_bar(stat="identity") +
   scale_fill_brewer(name = "Education Level",
                     palette = "Dark2") +
   labs(x = "Quarter", y = "Self-Employed Non-Taxpayers",
        title = "Education Level of Non-Social Security Contributors in 2019.1") +
   theme_minimal() +
   theme(text = element_text(family = "LM Roman 10"),
         plot.title = element_text(size = 13, face = "bold", hjust = 0.5)) +
   geom_label(aes(label = paste0(labell, "%")), position = position_stack(vjust = 0.5),
              show.legend = F)+
   coord_polar("y")
 
 #### Fazendo gráfico - Conta Própria que contribuía pro INSS
 
 ii = data %>%
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
   mutate(labell = round(labels/nrow(ii), digits = 2))
 
 ii$labell = ii$labell * 100 
 
 dd = ii %>%
   select(year_quarter, higher_educ_label, labell) %>%
   distinct()
 
 graf_2 = ggplot(dd, aes(x = year_quarter, y = labell,
               fill = factor(higher_educ_label))) +
   geom_bar(stat="identity") +
   scale_fill_rickandmorty(name = "Education Level")+
   labs(x = "Quarter", y = "Self-Employed Taxpayers",
        title = "Education Level of Social Security Contributors in 2019.2") +
   theme_minimal() +
   theme(text = element_text(family = "LM Roman 10"),
         plot.title = element_text(size = 13, face = "bold", hjust = 0.5)) +
   geom_label(aes(label = paste0(labell, "%")), position = position_stack(vjust = 0.5),
              show.legend = F)+
   coord_polar("y")
 
 #### Fazendo gráfico - Conta Própria que não contribuía pro INSS
 
 ii1 = data %>%
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
   mutate(labell = round(labels/nrow(ii1), digits = 2))
 
 ii1$labell = ii1$labell * 100 
 
 eeee = ii1 %>%
   select(year_quarter, higher_educ_label, labell) %>%
   distinct()
 
 grafico2 = ggplot(eeee, aes(x = year_quarter, y = labell,
                fill = factor(higher_educ_label))) +
   geom_bar(stat="identity") +
   scale_fill_manual(name = "Education Level",
                     values = ghibli_palette("MarnieMedium1"))+
   labs(x = "Quarter", y = "Self-Employed Non-Taxpayers",
        title = "Education Level of Non-Social Security Contributors in 2019.2") +
   theme_minimal() +
   theme(text = element_text(family = "LM Roman 10"),
         plot.title = element_text(size = 13, face = "bold", hjust = 0.5)) +
   geom_label(aes(label = paste0(labell, "%")), position = position_stack(vjust = 0.5),
              show.legend = F)+
   coord_polar("y")
 
 ### Fazendo gráfico - Conta Própria que contribuía pro INSS
 
 iii = data %>%
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
   mutate(labell = round(labels/nrow(iii), digits = 2))

 iii$labell = iii$labell * 100 
 
 eee = iii %>%
   select(year_quarter, higher_educ_label, labell) %>%
   distinct()
 
 graf_3 = ggplot(eee, aes(x = year_quarter, y = labell,
                 fill = factor(higher_educ_label))) +
   geom_bar(stat="identity") +
   scale_fill_locuszoom(name = "Education Level") +
   labs(x = "Quarter", y = "Self-Employed Taxpayers",
        title = "Education Level of Social Security Contributors in 2019.3") +
   theme_minimal() +
   theme(text = element_text(family = "LM Roman 10"),
         plot.title = element_text(size = 13, face = "bold", hjust = 0.5)) +
   geom_label(aes(label = paste0(labell, "%")), position = position_stack(vjust = 0.5),
              show.legend = F)+
   coord_polar("y")
 
 
 ### Fazendo Gráfico - Conta Própria que não contribuía pro INSS
 
 iii1 = data %>%
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
   mutate(labell = round(labels/nrow(iii1), digits = 2))
 
 iii1$labell = iii1$labell * 100 
 
 ee = iii1 %>%
   select(year_quarter, higher_educ_label, labell) %>%
   distinct()
 
 
 
 grafico3 = ggplot(ee, aes(x = year_quarter, y = labell,
                 fill = factor(higher_educ_label))) +
   geom_bar(stat="identity") +
   scale_fill_brewer(name = "Education Level",
                     palette = "Accent" ) +
   labs(x = "Quarter", y = "Self-Employed Non-Taxpayers",
        title = "Education Level of Non-Social Security Contributors in 2019.3") +
   theme_minimal() +
   theme(text = element_text(family = "LM Roman 10"),
         plot.title = element_text(size = 13, face = "bold", hjust = 0.5)) +
   geom_label(aes(label = paste0(labell, "%")), position = position_stack(vjust = 0.5),
              show.legend = F)+
   coord_polar("y")
 
 ##### Fazendo Gráfico - Conta Própria que contribuía para INSS
 
 iiii = data %>%
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
   mutate(labell = round(labels/nrow(iiii), digits = 2))
 
 iiii$labell = iiii$labell * 100
 
 e = iiii %>%
   select(year_quarter, higher_educ_label, labell) %>%
   distinct()
 
 graf_4 = ggplot(e, aes(x = year_quarter, y = labell,
                  fill = factor(higher_educ_label))) +
   geom_bar(stat = "identity")+
   scale_fill_manual(name = "Education Level",
                     values = carto_pal(name = "Temps")) +
   labs(x = "Quarter", y = "Self-Employed Taxpayers",
        title = "Education Level of Social Security Contributors in 2019.4") +
   theme_minimal()+
   theme(text = element_text(family = "LM Roman 10"),
         plot.title = element_text(size = 13, face = "bold", hjust = 0.5)) +
   geom_label(aes(label = paste0(labell, "%")), position = position_stack(vjust = 0.5),
              show.legend = F)+
   coord_polar("y")
 
 ### Fazendo Gráfico - Conta Própria que não contribuía pro INSS
 
 iiii1 = data %>%
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
   mutate(labell = round(labels/nrow(iiii1), digits = 2))
 
 iiii1$labell = iiii1$labell * 100
 
 
 d = iiii1 %>%
   select(year_quarter, higher_educ_label, labell) %>%
   distinct()
 
 grafico4 = ggplot(d, aes(x = year_quarter, y = labell,
                  fill = higher_educ_label)) +
   geom_bar(stat = "identity")+
   scale_fill_manual(name = "Education Level",
                     values = carto_pal(name = "Earth")) +
   labs(x = "Quarter", y = "Self-Employed Non-Taxpayers",
        title = "Education Level of Non-Social Security Contributors in 2019.4") +
   theme_minimal() +
   theme(text = element_text(family = "LM Roman 10"),
         plot.title = element_text(size = 13, face = "bold", hjust = 0.5)) +
   geom_label(aes(label = paste0(labell, "%")), position = position_stack(vjust = 0.5),
                    show.legend = F)+
   coord_polar("y")
 
 
 
   
 