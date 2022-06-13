library(rcartocolor)
library(extrafont)

df = read_rds("./input/painel_2019.rds")

### Grafico Salario por educacao

sal_ocup = df %>%
  filter(worker == 1) %>%
  select(worker, monthly_work_income, job_function, higher_educ_level,
         year_quarter, weights) %>%
  mutate(higher_educ_label = case_when(higher_educ_level %in% c(1) ~"Uneducated",
                                       higher_educ_level %in% c(2) ~ "Primary School Incomplete",
                                       higher_educ_level %in% c(3) ~ "Primary School Complete",
                                       higher_educ_level %in% c(4) ~ "High School Incomplete",
                                       higher_educ_level %in% c(5) ~ "High School Complete",
                                       higher_educ_level %in% c(6) ~ "College Degree Incomplete",
                                       higher_educ_level %in% c(7) ~ "College Degree Complete"))  %>%
  group_by(higher_educ_level) %>% mutate(labels = sum(weights)) %>%
  ungroup() %>%
  mutate(labell = round(labels/sum(weights), digits = 2))

sal_ocup$labell = sal_ocup$labell * 100

sal_ocup = sal_ocup %>%
  select(year_quarter, higher_educ_label, monthly_work_income, higher_educ_level, labell, weights,
         labels) %>%
  filter(!is.na(monthly_work_income)) %>%
  group_by(labels) %>%
  mutate(media = wtd.mean(monthly_work_income, weights = weights)) %>%
  mutate(media = round(media, digits = 2)) %>%
  distinct() %>%
  mutate(labelll = case_when(higher_educ_label == "Uneducated" & year_quarter == "2019_1" ~ 931.37,
                             higher_educ_label == "Primary School Incomplete" & year_quarter == "2019_1" ~ 1245.76,
                             higher_educ_label == "Primary School Complete" & year_quarter == "2019_1" ~ 1378.01,
                             higher_educ_label == "High School Incomplete" & year_quarter == "2019_1" ~ 1509.52,
                             higher_educ_label == "High School Complete" & year_quarter == "2019_1" ~ 1819.98,
                             higher_educ_label == "College Degree Incomplete" & year_quarter == "2019_1" ~ 2250.92,
                             higher_educ_label == "College Degree Complete" & year_quarter == "2019_1" ~ 5039.78)) %>%
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

