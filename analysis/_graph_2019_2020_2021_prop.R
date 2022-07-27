library(rcartocolor)
library(tidyverse)
library(extrafont)


 df1 = read_rds("./input/trimestre_2019_1_1.rds")
 df2 = read_rds("./input/trimestre_2019_1_2.rds")
 df3 = read_rds("./input/trimestre_2019_1_3.rds")
 df4 = read_rds("./input/trimestre_2019_1_4.rds")
 df5 = read_rds("./input/trimestre_2019_2_1.rds")
 df6 = read_rds("./input/trimestre_2019_2_2.rds")
 df7 = read_rds("./input/trimestre_2019_2_3.rds")
 df8 = read_rds("./input/trimestre_2019_2_4.rds")
 df9 = read_rds("./input/trimestre_2019_3_1.rds")
 df10 = read_rds("./input/trimestre_2019_3_2.rds")
 df11 = read_rds("./input/trimestre_2019_3_3.rds")
 df12 = read_rds("./input/trimestre_2019_3_4.rds")
 df13 = read_rds("./input/trimestre_2019_4_1.rds")
 df14 = read_rds("./input/trimestre_2019_4_2.rds")
 df15 = read_rds("./input/trimestre_2019_4_3.rds")
 df16 = read_rds("./input/trimestre_2019_4_4.rds")
 df17 = read_rds("./input/trimestre_2020_1_1.rds")
 df18 = read_rds("./input/trimestre_2020_1_2.rds")
 df19 = read_rds("./input/trimestre_2020_1_3.rds")
 df20 = read_rds("./input/trimestre_2020_1_4.rds")
 df21 = read_rds("./input/trimestre_2020_2_1.rds")
 df22 = read_rds("./input/trimestre_2020_2_2.rds")
 df23 = read_rds("./input/trimestre_2020_2_3.rds")
 df24 = read_rds("./input/trimestre_2020_2_4.rds")
 df25 = read_rds("./input/trimestre_2020_3_1.rds")
 df26 = read_rds("./input/trimestre_2020_3_2.rds")
 df27 = read_rds("./input/trimestre_2020_3_3.rds")
 df28 = read_rds("./input/trimestre_2020_3_4.rds")
 df29 = read_rds("./input/trimestre_2020_4_1.rds")
 df30 = read_rds("./input/trimestre_2020_4_2.rds")
 df31 = read_rds("./input/trimestre_2020_4_3.rds")
 df32 = read_rds("./input/trimestre_2020_4_4.rds")
 df33 = read_rds("./input/trimestre_2021_1_1.rds")
 df34 = read_rds("./input/trimestre_2021_1_2.rds")
 df35 = read_rds("./input/trimestre_2021_1_3.rds")
 df36 = read_rds("./input/trimestre_2021_1_4.rds")
 df37 = read_rds("./input/trimestre_2021_2_1.rds")
 df38 = read_rds("./input/trimestre_2021_2_2.rds")
 df39 = read_rds("./input/trimestre_2021_2_3.rds")
 df40 = read_rds("./input/trimestre_2021_2_4.rds")
 df41 = read_rds("./input/trimestre_2021_3_1.rds")
 df42 = read_rds("./input/trimestre_2021_3_2.rds")
 df43 = read_rds("./input/trimestre_2021_3_3.rds")
 df44 = read_rds("./input/trimestre_2021_3_4.rds")
 
 df = bind_rows(df1,df2,df3,df4,df5,df6,df7,df8,df9,df10,
                                  df11,df12,df13,df14,df15,df16,df17,df18,df19,df20,
                                  df21,df22,df23,df24,df25,df26,df27,df28,df29,df30,
                                  df31,df32,df33,df34,df35,df36,df37,df38,df39,df40,
                                  df41,df42,df43,df44)

 df = df %>%
   select(position, social_security_taxpayer, higher_educ_level, year_quarter,
          job_function, worker, weights) %>%
   mutate(higher_educ_label = case_when(higher_educ_level %in% c(1,2) ~"Uneducated and Primary School Incomplete",
                                        higher_educ_level %in% c(3,4) ~ "Primary School Complete and Incomplete High School",
                                        higher_educ_level %in% c(5,6) ~ "Complete High School and Incomplete College Degree",
                                        higher_educ_level %in% c(7) ~ "Complete College Degree"))   %>%
   group_by(year_quarter,higher_educ_level, position) %>% mutate(labels = sum(weights)) %>%
   ungroup() %>%
   mutate(labell = round(labels/sum(weights), digits = 2))

df$labell = df$labell * 100 

trab_conta_propria_n_INSS = trab_conta_propria_n_INSS %%
  select(year_quarter, higher_educ_label, higher_educ_level, labell) %%
  distinct() %%
  mutate(labelll = case_when(higher_educ_label == "Uneducated and Primary School Incomplete" & year_quarter == "2019_1" ~ 39,
                             higher_educ_label == "Primary School Complete and Incomplete High School" & year_quarter == "2019_1" ~ 18,
                             higher_educ_label == "Complete High School and Incomplete College Degree" & year_quarter == "2019_1" ~ 33,
                             higher_educ_label == "Complete College Degree" & year_quarter == "2019_1" ~ 10)) %%
  filter(!is.na(labelll))

trab_conta_propria_n_INSS = trab_conta_propria_n_INSS[c(2,3,4,6),]

grafico1 = ggplot(trab_conta_propria_n_INSS, aes(x = year_quarter, y = labelll,
                                                 fill = factor(higher_educ_level))) +
  geom_bar(stat="identity") +
  scale_fill_manual(name = "Education Level",
                    values = carto_pal(name = "Safe"), 
                    labels = c("Incomplete Primary School",
                               "Incomplete High School",
                               "Incomplete College",
                               "Complete College")) +
  labs(x = "", y = "",
       title = "Self-Employed Non-Social Security Contributors") +
  theme_minimal() +
  theme(text = element_text(family = "LM Roman 10"),
        plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
        legend.title = element_blank(),
        axis.text = element_blank(), 
        legend.text = element_text(size = 20)) +
  geom_label(aes(x = 1.2, label = paste0(labell, "%")), position = position_stack(vjust = 0.5),
             show.legend = F)+
  coord_polar("y") +
  guides(fill=guide_legend(nrow=2,byrow=TRUE))
