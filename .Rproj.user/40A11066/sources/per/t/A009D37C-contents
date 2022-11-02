library(tidyverse)
library(rcartocolor)
library(extrafont)

base = haven::read_dta("./input/reg_grafico.dta")

base = base %>%
  mutate(educ = case_when(educ == 1 ~ "Incomplete Primary School",
                          educ == 2 ~ "Incomplete High School",
                          educ == 3 ~ "Incomplete College",
                          educ == 4 ~ "Complete College")) %>%
  mutate(educ = factor(educ, ordered = TRUE, levels = c("Incomplete Primary School",
                                                                                 "Incomplete High School",
                                                                                 "Incomplete College",
                                                                                 "Complete College")))
  

transition_labels = c(
  "0" = "Formal to Formal",
  "1" = "Formal to Informal",
  "2" = "Formal to Non-Employed",
  "3" = "Informal to Formal",
  "4" = "Informal to Informal",
  "5" = "Informal to Non-Employed"
)


df = base

df = df %>%
  mutate(year_quarter = as.character(year_quarter))


compare_transitions = function(df, t1, t2, t3){

df %>%
  filter(transition %in% c(t1,t2,t3)) %>%
  mutate(transition = as.ordered(transition)) %>%
  ggplot(aes(x = year_quarter, y = coef, group = educ, color = educ))  +
  geom_point(size = 2) +
  geom_line(size = 2) +
  
  scale_color_manual(name = "Education Level",
                     values = carto_pal(name = "Safe")) +
  
  labs(x = "Year", y = "Coefficient") +
  
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
  
    scale_x_discrete(breaks = paste0(seq(2013, 2021, 2), "1"),
                    labels = seq(2013,2021, 2)) +
  
  facet_wrap(~ transition, labeller = as_labeller(transition_labels))

}


compare_transitions(df, 0, 1, 2) # Formal
compare_transitions(df, 3, 4, 5) # Informal



    