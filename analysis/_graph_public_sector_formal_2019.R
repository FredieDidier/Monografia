library(rcartocolor)
library(extrafont)

df = read_rds("./input/painel_2019.rds")

## Fazendo Grafico Servidor Publico que tem carteira assinada

serv_cart = df %>%
  filter(year_quarter %in% c("2019_1", "2019_2", "2019_3", "2019_4")) %>%
  select(signed_work_card, higher_educ_level, year_quarter,
         job_function, worker, work_category, weights) %>%
  filter(job_function == 4 & work_category %in% c(5,7) & worker == 1)  %>%
  mutate(higher_educ_label = case_when(higher_educ_level %in% c(1,2) ~"Uneducated and Primary School Incomplete",
                                       higher_educ_level %in% c(3,4) ~ "Primary School Complete and Incomplete High School",
                                       higher_educ_level %in% c(5,6) ~ "Complete High School and Incomplete College Degree",
                                       higher_educ_level %in% c(7) ~ "Complete College Degree"))  %>%
  group_by(higher_educ_level) %>% mutate(labels = sum(weights)) %>%
  ungroup() %>%
  mutate(labell = round(labels/sum(weights), digits = 2))


serv_cart$labell = serv_cart$labell * 100 

serv_cart = serv_cart %>%
  select(year_quarter, higher_educ_level, higher_educ_label, labell) %>%
  distinct() %>%
  mutate(labelll = case_when(higher_educ_label == "Uneducated and Primary School Incomplete" & year_quarter == "2019_1" ~ 5,
                             higher_educ_label == "Primary School Complete and Incomplete High School" & year_quarter == "2019_1" ~ 5,
                             higher_educ_label == "Complete High School and Incomplete College Degree" & year_quarter == "2019_1" ~ 31,
                             higher_educ_label == "Complete College Degree" & year_quarter == "2019_1" ~ 59)) %>%
  filter(!is.na(labelll))

serv_cart = serv_cart[c(5,6,2,1),]

n_grafico = ggplot(serv_cart, aes(x = "", y = labelll,
                                  fill = factor(higher_educ_level))) +
  geom_bar(stat = "identity")+
  scale_fill_manual(name = "Education Level",
                    values = carto_pal(name = "Prism"),
                    labels = c("Uneducated and Incomplete Primary School",
                               "Complete Primary School and Incomplete High School",
                               "Complete High School and Incomplete College Degree",
                               "Complete College Degree")) +
  labs(x = "", y = "",
       title = "Education Level of Formal Public Sector Employees in 2019") +
  theme_minimal() +
  theme(text = element_text(family = "LM Roman 10"),
        plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
        legend.title = element_blank(),
        axis.text = element_blank()) +
  geom_label(aes(x = 1.2, label = paste0(labelll, "%")), position = position_stack(vjust = 0.5),
             show.legend = F)+
  coord_polar("y")
