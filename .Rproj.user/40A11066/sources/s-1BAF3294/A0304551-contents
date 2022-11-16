library(rcartocolor)
library(tidyverse)
library(extrafont)

trimestres = c("2019_1", "2019_2", "2019_3", "2019_4",
               "2020_1", "2020_2", "2020_3", "2020_4",
               "2021_1", "2021_2", "2021_3", "2021_4", "2022_1")

trimestres <- rep(trimestres, 4)

educ <- c(
  rep(1, 13),
  rep(2, 13),
  rep(3, 13),
  rep(4, 13)
)

df = map2_dfr(trimestres, educ,
              
              function(trim, educ){
                
                message(paste0("Downloading", trim, "\n educ ", educ))
                
                df <- readr::read_rds(
                  paste0("input/trimestre_",
                         trim,
                         "_",
                         educ,
                         ".rds")
                  
                  
                  
                )
                df
              }
)
                  
                  
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
  group_by(year_quarter, position, higher_educ_label) %>% 
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
    filter(position %in% c(pos1, pos2))%>%
    ggplot(aes(x = year_quarter, y = get(var), group = higher_educ_label, color = higher_educ_label)) +
    geom_point(size = 3) +
    geom_line(size = 2) +
    
    scale_color_manual(name = "Education Level",
                       values = carto_pal(name = "Safe")) +
    
    labs(x = "Year", y = "Proportion") +
    
    theme_minimal() +
    theme(text = element_text(family = "LM Roman 10"),
          plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
          legend.title = element_blank(), 
          legend.text = element_text(size = 20),
          legend.position = "bottom",
          axis.title = element_text(size = 18, face = "bold", hjust = 0.5),
          strip.text = element_text(size = 18, face = "bold", hjust = 0.5),
          axis.line = element_line(size = 0.75, colour = "black"),
          axis.text = element_text(
            family = "Helvetica",
            colour = "red",
            size = rel(1.2)
          )) +
    
    scale_x_discrete(breaks = paste0(2019:2022, "_1"),
                     labels = 2019:2022) +
    
    facet_wrap(~ position, labeller = as_labeller(position_labels))
  
}
