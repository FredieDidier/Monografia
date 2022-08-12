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
df45 = read_rds("./input/trimestre_2021_4_1.rds")
df46 = read_rds("./input/trimestre_2021_4_2.rds")
df47 = read_rds("./input/trimestre_2021_4_3.rds")
df48 = read_rds("./input/trimestre_2021_4_4.rds")
df49 = read_rds("./input/trimestre_2022_1_1.rds")
df50 = read_rds("./input/trimestre_2022_1_2.rds")
df51 = read_rds("./input/trimestre_2022_1_3.rds")
df52 = read_rds("./input/trimestre_2022_1_4.rds")
df = bind_rows(df1,df2,df3,df4,df5,df6,df7,df8,df9,df10,
               df11,df12,df13,df14,df15,df16,df17,df18,df19,df20,
               df21,df22,df23,df24,df25,df26,df27,df28,df29,df30,
               df31,df32,df33,df34,df35,df36,df37,df38,df39,df40,
               df41,df42,df43,df44, df45, df46, df47, df48,
               df49, df50, df51, df52)
df = df %>%
  select(position, social_security_taxpayer, higher_educ_level, year_quarter,
         job_function, worker, weights, monthly_work_income) %>%
  mutate(higher_educ_label = case_when(higher_educ_level %in% c(1,2) ~"Incomplete Primary School",
                                       higher_educ_level %in% c(3,4) ~ "Incomplete High School",
                                       higher_educ_level %in% c(5,6) ~ "Incomplete College",
                                       higher_educ_level %in% c(7) ~ "Complete College"))   %>%
  mutate(higher_educ_label = factor(higher_educ_label, ordered = TRUE, levels = c("Incomplete Primary School",
                                                                                  "Incomplete High School",
                                                                                  "Incomplete College",
                                                                                  "Complete College"))) %>%
  group_by(year_quarter,higher_educ_label, position) %>% 
  summarise(labels = sum(weights), wage = sum(monthly_work_income*weights, na.rm = TRUE)/sum(weights)) %>%
  mutate(labels = 100 * labels/sum(labels)) %>%
  rename(proportion = labels)

position_labels <- c(
  "1" = "Inactive",
  "2" = "Unemployed",
  "3" = "Formal Private Sector",
  "4" = "Informal Private Sector",
  "5" = "Formal Self-Employed",
  "6" = "Informal Self-Employed",
  "7" = "Formal Employers",
  "8" = "Informal Employers",
  "9" = "Formal Public Sector",
  "10" = "Informal Public Sector"
) 

compare_positions <- function(pos1, pos2, var){
  
  df %>%
    filter(position %in% c(pos1, pos2)) %>%
    mutate(position = as.ordered(position)) %>%
    ggplot(aes(x = year_quarter, y = get(var), group = higher_educ_label, color = higher_educ_label)) +
    geom_point(size = 3) +
    geom_line(size = 2) +
    
    scale_color_manual(name = "Education Level",
                       values = carto_pal(name = "Safe")) +
    
    labs(x = "", y = "") +
    
    theme_minimal() +
    theme(text = element_text(family = "LM Roman 10"),
          plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
          legend.title = element_blank(), 
          legend.text = element_text(size = 20),
          legend.position = "bottom",
          strip.text = element_text(size = 13, face = "bold", hjust = 0.5)) +
    
    scale_x_discrete(breaks = paste0(2019:2022, "_1"),
                     labels = 2019:2022) +
    
    facet_wrap(~ position, labeller = as_labeller(position_labels))
  
}
