library(showtext)
library(tidyverse)
library(rcartocolor)
library(haven)

data = read_dta("build/output/regression/main_data.dta")

font_add_google(name = "Open Sans", family = "Open Sans")
showtext_auto()

df = data %>%
  select(year_quarter, position_names, position_transition, position, educ, weights)

df = df %>%
  mutate(denominador = case_when(position_names == "Formal" ~ 1,
                               position_names == "Informal" ~ 1))

df = df %>%
  mutate(numerador = case_when(position_transition == "Formal to Non-Employed" ~ 1,
                                 position_transition == "Informal to Non-Employed" ~ 1))


df = df %>%
  mutate(num_weights = numerador * weights) %>%
  mutate(den_weights = denominador * weights)

df = df %>%
  group_by(educ) %>%
  summarize(num_weights = sum(num_weights, na.rm = TRUE),
            den_weights = sum(den_weights, na.rm = TRUE))

df = df %>%
  mutate(job_loss = (num_weights/den_weights)*100)

df = df %>%
  mutate(educ = as.factor(educ))

educ_levels = c(
  
  "Incomplete Primary School",
  "Incomplete High School",
  "Incomplete College",
  "Complete College"
  
)

educ_levels = factor(educ_levels, ordered = T, levels = c("Incomplete Primary School",
                                                       "Incomplete High School",
                                                       "Incomplete College",
                                                       "Complete College"))


graph = ggplot(df, aes(x = reorder(educ, -job_loss))) +
  geom_bar(aes(y = job_loss, fill = educ_levels), stat = "identity") +
  scale_fill_manual(name = "Education Level",
                    values = carto_pal(name = "Safe")) +
  labs(x = "", y = "Job Loss %") + ggtitle("Job Loss by Education Level") +
  theme_minimal() +
  theme(text = element_text(family = "Open Sans"),
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 18, face = "bold"), 
        legend.text = element_text(size = 20),
        legend.position = "right",
        axis.title = element_text(size = 18, face = "bold", hjust = 0.5),
        strip.text = element_text(size = 18, face = "bold", hjust = 0.5),
        axis.line = element_line(size = 0.75, colour = "black"),
        axis.text.x = element_blank(),
        axis.text.y = element_text(
          family = "Helvetica",
          colour = "black",
          size = rel(1.2)
        )) +
  guides(fill = guide_legend(nrow = 4, byrow = TRUE))

