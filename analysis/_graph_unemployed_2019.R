library(rcartocolor)
library(extrafont)

df = read_rds("./input/painel_2019.rds")

# Grafico de Desempregados em 2019

desempregados = df %>%
  filter(workforce_condition == 1, occupation_condition == 2 & worker == 2) %>%
  select(workforce_condition, occupation_condition, worker, higher_educ_level, year_quarter,
         weights) %>%
  mutate(higher_educ_label = case_when(higher_educ_level %in% c(1,2) ~"Uneducated and Primary School Incomplete",
                                       higher_educ_level %in% c(3,4) ~ "Primary School Complete and Incomplete High School",
                                       higher_educ_level %in% c(5,6) ~ "Complete High School and Incomplete College Degree",
                                       higher_educ_level %in% c(7) ~ "Complete College Degree")) %>%
  group_by(higher_educ_level) %>% mutate(labels = sum(weights)) %>%
  ungroup() %>%
  mutate(labell = round(labels/sum(weights), digits = 2))

desempregados$labell = desempregados$labell * 100

desempregados = desempregados %>%
  select(year_quarter, higher_educ_label, higher_educ_level, labell) %>%
  distinct() %>%
  mutate(labelll = case_when(higher_educ_label == "Uneducated and Primary School Incomplete" & year_quarter == "2019_1" ~ 22,
                             higher_educ_label == "Primary School Complete and Incomplete High School" & year_quarter == "2019_1" ~ 22,
                             higher_educ_label == "Complete High School and Incomplete College Degree" & year_quarter == "2019_1" ~ 46,
                             higher_educ_label == "Complete College Degree" & year_quarter == "2019_1" ~ 10)) %>%
  filter(!is.na(labelll))

desempregados = desempregados[c(2,5,6,3),]

desemp = ggplot(desempregados, aes(x = "", y = labelll,
                                   fill = factor(higher_educ_level))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(name = "Education Level",
                    values = carto_pal(name = "Safe"), 
                    labels = c("Incomplete Primary School",
                               "Incomplete High School",
                               "Incomplete College",
                               "Complete College")) +
  labs(x = "", y = "",
       title = "Unemployed in 2019") +
  
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
