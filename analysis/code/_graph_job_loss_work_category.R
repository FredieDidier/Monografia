library(showtext)
library(haven)
library(tidyverse)
library(RColorBrewer)
library(here)

data = read_dta("build/output/regression/main_data.dta")

font_add_google(name = "Open Sans", family = "Open Sans")
showtext_auto()

df = data %>%
  select(year_quarter, position_names, position_transition, position, work_category, weights)

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
  filter(position >= 3) %>%
  group_by(position) %>%
  summarise(num_weights = sum(num_weights, na.rm = TRUE),
            den_weights = sum(den_weights, na.rm = TRUE))

df = df %>%
  mutate(job_loss = (num_weights/den_weights)*100)

df = df %>%
  mutate(position = as.factor(position))

work_category = c(
  
  "Formal Private Sector",
  "Informal Private Sector",
  "Formal Self-Employed",
  "Informal Self-Employed",
  "Formal Employer",
  "Informal Employer",
  "Formal Public Sector",
  "Informal Public Sector"
  
)

graph = ggplot(df, aes(x = reorder(position, -job_loss))) +
  geom_bar(aes(y = job_loss, fill = work_category), stat = "identity") +
  scale_fill_manual(name = "Work Category",
                    values = brewer.pal(8, name = "Set1")) +
  labs(x = "", y = "Job Loss %") +
  
  theme_minimal() +
  theme(text = element_text(family = "Open Sans"),
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 22, face = "bold"), 
        legend.text = element_text(size = 20),
        legend.position = c(0.75,0.75),
        axis.title.y = element_text(size = 40),
        axis.title = element_text(size = 18, face = "bold", hjust = 0.5),
        strip.text = element_text(size = 18, face = "bold", hjust = 0.5),
        axis.line = element_line(linewidth = 0.75, colour = "black"),
        axis.text.x = element_blank(),
        axis.text.y = element_text(
          family = "Helvetica",
          colour = "black",
          size = rel(1.2)
        )) +
  guides(fill = guide_legend(nrow = 4, byrow = TRUE))

graph

ggsave(filename = here(wd, "analysis", "output", "graph", "_graph_job_loss_work_category.png"),
       width = 9, height = 12, device = "png", dpi = 300)
