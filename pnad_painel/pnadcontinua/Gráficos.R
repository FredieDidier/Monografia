font_import(path = "C:/Users/Fredie/AppData/Local/Microsoft/Windows/Fonts")
y
loadfonts(device = "win")


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
   theme(text = element_text(family = "LM Roman 10"))
 
 
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
   theme(text = element_text(family = "LM Roman 10"))
 
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
   theme(text = element_text(family = "LM Roman 10"))
 
 
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
   theme(text = element_text(family = "LM Roman 10"))
 
 
 
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
   theme(text = element_text(family = "LM Roman 10"))
 
 
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
   theme(text = element_text(family = "LM Roman 10"))
 
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
 
 
 
 ggplot(dddd, aes(x = year_quarter, y = labell,
               fill = factor(higher_educ_label))) +
   geom_bar(stat="identity") +
   scale_fill_manual(name = "Education Level",
                     values = c("#936D0E",
                                "#4B0082", "#FF4500",
                                "#00EA04",
                                "darkcyan","#DC143C",
                                "#6B8E23")) +
   labs(x = "Quarter", y = "Self-Employed Taxpayers") +
   theme(text = element_text(family = "LM Roman 10")) +
   geom_label(aes(label = paste0(labell, "%")), position = position_stack(vjust = 0.35),
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
 
 
 
 ggplot(ddd, aes(x = year_quarter, y = labell,
               fill = factor(higher_educ_label))) +
   geom_bar(stat="identity") +
   scale_fill_manual(name = "Education Level",
                     values = c("#EA0700",
                                "#808000", "palegreen",
                                "#663399",
                                "#008080","lightcoral",
                                "hotpink")) +
   labs(x = "Quarter", y = "Self-Employed Non-Taxpayers") +
   theme(text = element_text(family = "LM Roman 10")) +
   geom_label(aes(label = paste0(labell, "%")), position = position_stack(vjust = 0.35),
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
 
 ggplot(dd, aes(x = year_quarter, y = labell,
               fill = factor(higher_educ_label))) +
   geom_bar(stat="identity") +
   scale_fill_manual(name = "Education Level",
                     values = c("#45B39D",
                                "#DE3163", "#6495ED",
                                "#EA8A00",
                                "#D3F707","#5307F7",
                                "#E507F7")) +
   labs(x = "Quarter", y = "Self-Employed Taxpayers") +
   theme(text = element_text(family = "LM Roman 10")) +
   geom_label(aes(label = paste0(labell, "%")), position = position_stack(vjust = 0.35),
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
 
 ggplot(eeee, aes(x = year_quarter, y = labell,
                fill = factor(higher_educ_label))) +
   geom_bar(stat="identity") +
   scale_fill_manual(name = "Education Level",
                     values = c("#0778F7",
                                "#F74507", "#07F724",
                                "#CB07F7",
                                "#DAF707","#F70728",
                                "#4107F7")) +
   labs(x = "Quarter", y = "Self-Employed Non-Taxpayers") +
   theme(text = element_text(family = "LM Roman 10")) +
   geom_label(aes(label = paste0(labell, "%")), position = position_stack(vjust = 0.35),
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
 
 ggplot(eee, aes(x = year_quarter, y = labell,
                 fill = factor(higher_educ_label))) +
   geom_bar(stat="identity") +
   scale_fill_manual(name = "Education Level",
                     values = c("#F72B07",
                                "#F7B907", "#0EF707",
                                "#1607F7",
                                "#F707C0","#F70748",
                                "#07F7B2")) +
   labs(x = "Quarter", y = "Self-Employed Taxpayers") +
   theme(text = element_text(family = "LM Roman 10")) +
   geom_label(aes(label = paste0(labell, "%")), position = position_stack(vjust = 0.35),
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
 
 
 
 ggplot(ee, aes(x = year_quarter, y = labell,
                 fill = factor(higher_educ_label))) +
   geom_bar(stat="identity") +
   scale_fill_manual(name = "Education Level",
                     values = c("#A3A3C2",
                                "#5454A2", "#33C148",
                                "#C13380",
                                "#EAE429","#C13333",
                                "#33BDC1")) +
   labs(x = "Quarter", y = "Self-Employed Non-Taxpayers") +
   theme(text = element_text(family = "LM Roman 10")) +
   geom_label(aes(label = paste0(labell, "%")), position = position_stack(vjust = 0.35),
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
 
 e = iiii1 %>%
   select(year_quarter, higher_educ_label, labell) %>%
   distinct()
 
 ggplot(e, aes(x = year_quarter, y = labell,
                  fill = factor(higher_educ_label))) +
   geom_bar(stat = "identity")+
   scale_fill_manual(name = "Education Level",
                     values = c("#F2162A",
                                "#59F216", "#169FF2",
                                "#F216E5",
                                "#F21687","#8E16F2",
                                "#EAEA29")) +
   labs(x = "Quarter", y = "Self-Employed Taxpayers") +
   theme(text = element_text(family = "LM Roman 10")) +
   geom_label(aes(label = paste0(labell, "%")), position = position_stack(vjust = 0.45),
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
 
 ggplot(d, aes(x = year_quarter, y = labell,
                  fill = higher_educ_label)) +
   geom_bar(stat = "identity")+
   scale_fill_manual(name = "Education Level",
                     values = c("#EAE429",
                                "#C129EA", "#3529EA",
                                "#EA9229",
                                "#29EA9E","#CF477B",
                                "#93CF47")) +
   labs(x = "Quarter", y = "Self-Employed Non-Taxpayers") +
   theme(text = element_text(family = "LM Roman 10")) +
   geom_label(aes(label = paste0(labell, "%")), position = position_stack(vjust = 0.3),
                    show.legend = F)+
   coord_polar("y")
 
 
 
   
 