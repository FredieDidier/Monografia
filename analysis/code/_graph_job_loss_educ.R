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
  summarise(num_weights = sum(num_weights, na.rm = TRUE),
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
  labs(x = "", y = "Job Loss %") +
  theme_bw() +
  theme(text = element_text(family = "Open Sans"),
        legend.key.size = unit(1, "cm"),
        legend.key.height = unit(1, "cm"),
        legend.key.width = unit(4, "cm"),
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 22, face = "bold"), 
        legend.text = element_text(size = 16),
        legend.position = c(0.85,0.85),
        axis.title = element_text(size = 18, face = "bold", hjust = 0.5),
        strip.text = element_text(size = 18, face = "bold", hjust = 0.5),
        axis.line = element_line(linewidth = 0.75, colour = "black"),
        axis.text.x = element_blank(),
        axis.title.y = element_text(size = 40),
        axis.text.y = element_text(
          family = "Helvetica",
          colour = "black",
          size = rel(4.0)
        )) +
  guides(fill = guide_legend(nrow = 4, byrow = TRUE))

graph

ggsave(filename = here(wd, "analysis", "output", "graph", "_graph_job_loss_educ.png"),
       width = 9, height = 12, device = "png", dpi = 300)
