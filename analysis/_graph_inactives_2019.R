library(rcartocolor)
library(extrafont)

df = read_rds("./input/painel_2019.rds")

# Fazendo Grafico Inativos

inativo = df %>%
  filter(year_quarter %in% c("2019_1", "2019_2", "2019_3", "2019_4")) %>%
  select(workforce_condition, higher_educ_level, year_quarter, weights
  ) %>%
  filter(workforce_condition == 2)  %>%
  mutate(higher_educ_label = case_when(higher_educ_level %in% c(1,2) ~"Uneducated and Primary School Incompleted",
                                       higher_educ_level %in% c(3,4) ~ "Primary School Completed and Incompleted High School",
                                       higher_educ_level %in% c(5,6) ~ "Completed High School and Incompleted College Degree",
                                       higher_educ_level %in% c(7) ~ "Completed College Degree"))  %>%
  group_by(higher_educ_level) %>% mutate(labels = sum(weights)) %>%
  ungroup() %>%
  mutate(labell = round(labels/sum(weights), digits = 2))

inativo$labell = inativo$labell * 100 

inativo = inativo %>%
  select(year_quarter, higher_educ_label, higher_educ_level, labell) %>%
  distinct() %>%
  mutate(labelll = case_when(higher_educ_label == "Uneducated and Primary School Incompleted" & year_quarter == "2019_1" ~ 51,
                             higher_educ_label == "Primary School Completed and Incompleted High School" & year_quarter == "2019_1" ~ 20,
                             higher_educ_label == "Completed High School and Incompleted College Degree" & year_quarter == "2019_1" ~ 22,
                             higher_educ_label == "Completed College Degree" & year_quarter == "2019_1" ~ 7)) %>%
  filter(!is.na(labelll))

inativo = inativo[c(2,3,1,7),]


grafico_inat = ggplot(inativo, aes(x = "", y = labelll,
                                   fill = factor(higher_educ_level))) +
  geom_bar(stat = "identity")+
  scale_fill_manual(name = "Education Level",
                    values = carto_pal(name = "Safe"), 
                    labels = c("Incomplete Primary School",
                               "Incomplete High School",
                               "Incomplete College",
                               "Complete College")) +
  labs(x = "", y = "",
       title = "Inactive in 2019") +
  theme_minimal() +
  theme(text = element_text(family = "LM Roman 10"),
        plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
        legend.title = element_blank(),
        axis.text = element_blank(),
        legend.text = element_text(size = 20)) +
  geom_label(aes(x = 1.2, label = paste0(labelll, "%")), position = position_stack(vjust = 0.5),
             show.legend = F)+
  coord_polar("y") +
  guides(fill=guide_legend(nrow=2,byrow=TRUE))
