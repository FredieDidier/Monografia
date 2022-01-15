

library(ggpubr)


ggarrange(graf_1, graf_2, graf_3, graf_4,
          common.legend = TRUE,
          legend = "bottom")


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
  scale_fill_manual(name = "Education Level",
                    values = carto_pal(name = "Temps")) +
  labs(x = "Quarter", y = "Self-Employed Taxpayers",
       title = "Education Level of Social Security Contributors in 2019.1") +
  theme_minimal() +
  theme(text = element_text(family = "LM Roman 10"),
        plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
        legend.position = "none",
        legend.title = element_blank(),
        axis.text = element_blank()) +
  geom_label(aes(x = 1.2, label = paste0(labell, "%")), position = position_stack(vjust = 0.5),
             show.legend = F)+
  coord_polar("y")


#### 

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
  scale_fill_manual(name = "Education Level",
                    values = carto_pal(name = "Temps"))+
  labs(x = "Quarter", y = "Self-Employed Taxpayers",
       title = "Education Level of Social Security Contributors in 2019.2") +
  theme_minimal() +
  theme(text = element_text(family = "LM Roman 10"),
        plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
        legend.position = "none",
        legend.title = element_blank(),
        axis.text = element_blank()) +
  geom_label(aes(x = 1.2, label = paste0(labell, "%")), position = position_stack(vjust = 0.5),
             show.legend = F)+
  coord_polar("y")

####

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
  scale_fill_manual(name = "Education Level",
                    values = carto_pal(name = "Temps")) +
  labs(x = "Quarter", y = "Self-Employed Taxpayers",
       title = "Education Level of Social Security Contributors in 2019.3") +
  theme_minimal() +
  theme(text = element_text(family = "LM Roman 10"),
        plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
        legend.position = "none",
        legend.title = element_blank(),
        axis.text = element_blank()) +
  geom_label(aes(x =  1.2 ,label = paste0(labell, "%")), position = position_stack(vjust = 0.5),
             show.legend = F)+
  coord_polar("y")

#### 

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
        plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
        legend.position = "none",
        legend.title = element_blank(),
        axis.text = element_blank()) +
  geom_label(aes(x = 1.2, label = paste0(labell, "%")), position = position_stack(vjust = 0.5),
             show.legend = F)+
  coord_polar("y")

###############################

ggarrange(grafico1, grafico2, grafico3, grafico4, 
          common.legend = T, legend = "bottom")


grafico1 = ggplot(ddd, aes(x = year_quarter, y = labell,
                           fill = factor(higher_educ_label))) +
  geom_bar(stat="identity") +
  scale_fill_manual(name = "Education Level",
                    values = carto_pal(name = "Earth")) +
  labs(x = "Quarter", y = "Self-Employed Non-Taxpayers",
       title = "Education Level of Non-Social Security Contributors in 2019.1") +
  theme_minimal() +
  theme(text = element_text(family = "LM Roman 10"),
        plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
        axis.text = element_blank(),
        legend.title = element_blank()) +
  geom_label(aes(x = 1.2, label = paste0(labell, "%")),
             position = position_stack(vjust = 0.5),
             show.legend = F)+
  coord_polar("y")


grafico2 = ggplot(eeee, aes(x = year_quarter, y = labell,
                            fill = factor(higher_educ_label))) +
  geom_bar(stat="identity") +
  scale_fill_manual(name = "Education Level",
                    values = carto_pal(name = "Earth"))+
  labs(x = "Quarter", y = "Self-Employed Non-Taxpayers",
       title = "Education Level of Non-Social Security Contributors in 2019.2") +
  theme_minimal() +
  theme(text = element_text(family = "LM Roman 10"),
        plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
        axis.text = element_blank(),
        legend.title = element_blank()) +
  geom_label(aes(x = 1.3, label = paste0(labell, "%")), position = position_stack(vjust = 0.5),
             show.legend = F)+
  coord_polar("y")


grafico3 = ggplot(ee, aes(x = year_quarter, y = labell,
                          fill = factor(higher_educ_label))) +
  geom_bar(stat="identity") +
  scale_fill_manual(name = "Education Level",
                    values = carto_pal(name = "Earth")) +
  labs(x = "Quarter", y = "Self-Employed Non-Taxpayers",
       title = "Education Level of Non-Social Security Contributors in 2019.3") +
  theme_minimal() +
  theme(text = element_text(family = "LM Roman 10"),
        plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
        axis.text = element_blank(),
        legend.title = element_blank()) +
  geom_label(aes(x = 1.3, label = paste0(labell, "%")), position = position_stack(vjust = 0.5),
             show.legend = F)+
  coord_polar("y")


grafico4 = ggplot(d, aes(x = year_quarter, y = labell,
                         fill = higher_educ_label)) +
  geom_bar(stat = "identity")+
  scale_fill_manual(name = "Education Level",
                    values = carto_pal(name = "Earth")) +
  labs(x = "Quarter", y = "Self-Employed Non-Taxpayers",
       title = "Education Level of Non-Social Security Contributors in 2019.4") +
  theme_minimal() +
  theme(text = element_text(family = "LM Roman 10"),
        plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
        axis.text = element_blank(),
        legend.title = element_blank()) +
  geom_label(aes(x = 1.2, label = paste0(labell, "%")), position = position_stack(vjust = 0.5),
             show.legend = F)+
  coord_polar("y")

########################################################

ggarrange(graf.1, graf.2,
          common.legend = T,
          legend = "bottom")

graf.1 = ggplot(f, aes(x = year_quarter, y = signed_work_card,
                       fill = factor(higher_educ_label))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(name = "Education Level", 
                    values = carto_pal(name = "Geyser")) +
  labs(x =  "Quarter", y = "Signed Contract Workers",
       title = "Education Level of Registered Employees in 2019") +
  theme_minimal() +
  theme(text = element_text(family = "LM Roman 10"),
        plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
        legend.title = element_blank())


graf.2 = ggplot(h, aes(x = year_quarter, y = social_security_taxpayer,
                       fill = factor(higher_educ_label))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(name = "Education Level",
                    values = carto_pal(name = "Geyser")) +
  labs(x = "Quarter", y = "Contributors",
       title = "Education Level of Social Security Taxpayers in 2019") +
  theme_minimal()+
  theme(text = element_text(family = "LM Roman 10"),
        plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
        legend.title = element_blank())
########################################################################

ggarrange(inf1, inf2, inf3,
          common.legend = T,
          legend = "bottom")

inf1 = ggplot(gg, aes(x = year_quarter, y = worker,
                      fill = factor(higher_educ_label))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(name = "Education Level",
                    values = carto_pal(name = "Pastel")) +
  labs(x = "Quarter", y = "Non-Reported",
       title = "Education Level of Non-Registered Workers in 2019") +
  theme_minimal() +
  theme(text = element_text(family = "LM Roman 10"),
        plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
        legend.title = element_blank())


inf2 = ggplot(g2, aes(x = year_quarter, y = worker,
                      fill = factor(higher_educ_label))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(name = "Education Level",
                    values = carto_pal(name = "Pastel")) +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "Quarter", y = "Non-Workers",
       title = "Education Level of Informal Workers in 2019") +
  theme_minimal() +
  theme(text = element_text(family = "LM Roman 10"),
        plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
        legend.title = element_blank())


inf3 = ggplot(h1, aes(x = year_quarter, y = social_security_taxpayer,
                      fill = factor(higher_educ_label))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(name = "Education Level",
                    values = carto_pal(name = "Pastel")) +
  labs(x = "Quarter", y = "Non-Contributors",
       title = "Education Level of Non-Social Security Taxpayers Workers") +
  theme_minimal()+
  theme(text = element_text(family = "LM Roman 10"),
        plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
        legend.title = element_blank())
