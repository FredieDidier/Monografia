library(rcartocolor)
library(extrafont)

df = read_rds("./input/painel_2019.rds")

trab_conta_propria_n_INSS = df %>%
  filter(year_quarter %in% c("2019_1", "2019_2", "2019_3", "2019_4")) %>%
  select(social_security_taxpayer, higher_educ_level, year_quarter,
         job_function, worker, weights) %>%
  filter(job_function == 6 & social_security_taxpayer == 2 & worker == 1)  %>%
  mutate(higher_educ_label = case_when(higher_educ_level %in% c(1,2) ~"Uneducated and Primary School Incomplete",
                                       higher_educ_level %in% c(3,4) ~ "Primary School Complete and Incomplete High School",
                                       higher_educ_level %in% c(5,6) ~ "Complete High School and Incomplete College Degree",
                                       higher_educ_level %in% c(7) ~ "Complete College Degree"))   %>%
  group_by(higher_educ_level) %>% mutate(labels = sum(weights)) %>%
  ungroup() %>%
  mutate(labell = round(labels/sum(weights), digits = 2))

trab_conta_propria_n_INSS$labell = trab_conta_propria_n_INSS$labell * 100 

trab_conta_propria_n_INSS = trab_conta_propria_n_INSS %>%
  select(year_quarter, higher_educ_label, higher_educ_level, labell) %>%
  distinct() %>%
  mutate(labelll = case_when(higher_educ_label == "Uneducated and Primary School Incomplete" & year_quarter == "2019_1" ~ 39,
                             higher_educ_label == "Primary School Complete and Incomplete High School" & year_quarter == "2019_1" ~ 18,
                             higher_educ_label == "Complete High School and Incomplete College Degree" & year_quarter == "2019_1" ~ 33,
                             higher_educ_label == "Complete College Degree" & year_quarter == "2019_1" ~ 10)) %>%
  filter(!is.na(labelll))

trab_conta_propria_n_INSS = trab_conta_propria_n_INSS[c(2,3,4,6),]

grafico1 = ggplot(trab_conta_propria_n_INSS, aes(x = year_quarter, y = labelll,
                                                 fill = factor(higher_educ_level))) +
  geom_bar(stat="identity") +
  scale_fill_manual(name = "Education Level",
                    values = carto_pal(name = "Prism"), 
                    labels = c("Uneducated and Incomplete Primary School",
                               "Complete Primary School and Incomplete High School",
                               "Complete High School and Incomplete College Degree",
                               "Complete College Degree")) +
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
