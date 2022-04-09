library(rcartocolor)
library(extrafont)

font_import(path = "C:/Users/Fredie/AppData/Local/Microsoft/Windows/Fonts")
y
loadfonts(device = "win")

df = read_rds("./input/painel_2019.rds")

# Grafico Conta Propria Contribuinte em 2019

trab_conta_propria_INSS = df %>%
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
