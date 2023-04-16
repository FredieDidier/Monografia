library(rcartocolor)
library(tidyverse)
library(showtext)
library(fs)

font_add_google(name = "Roboto", family = "roboto")
showtext_auto()

file_list <- dir_ls("build/output/trimestres")
df <- file_list %>% 
  map_dfr(~ get(load(.)))
                  
                  
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
    
    labs(x = "Year", y = var) +
    
    theme_minimal() +
    theme(text = element_text(family = "roboto"),
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

compare_positions(5, 6, "wage")
