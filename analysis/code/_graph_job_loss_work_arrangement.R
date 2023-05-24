library(showtext)
library(tidyverse)
library(rcartocolor)
library(haven)

data = read_dta("build/output/regression/main_data.dta")

font_add_google(name = "Open Sans", family = "Open Sans")
showtext_auto()

df = data %>%
  select(year_quarter, position_names, position_transition, position, temporary_worker,
         monthly_work_income, labor_status, weights)

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
  group_by(labor_status) %>%
  summarise(num_weights = sum(num_weights, na.rm = TRUE),
            den_weights = sum(den_weights, na.rm = TRUE))

df = df %>%
  mutate(job_loss = (num_weights/den_weights)*100)

df = df %>%
  mutate(labor_status = as.factor(labor_status))

df = df %>%
  filter(!labor_status == "NA")

df = df %>%
  mutate(labor = case_when(labor_status == "Not Salaried" ~ 1,
                           labor_status == "Permanent" ~ 2,
                           labor_status == "Salaried" ~ 3,
                           labor_status == "Temporary" ~ 4))

graph = ggplot(df, aes(x = reorder(labor, -job_loss))) +
  geom_bar(aes(y = job_loss, fill = labor_status), stat = "identity") +
  scale_fill_manual(name = "Labor Status",
                    values = carto_pal(name = "Vivid")) +
  labs(x = "", y = "Job Loss %") +
  theme_minimal() +
  theme(text = element_text(family = "Open Sans"),
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 22, face = "bold"), 
        legend.text = element_text(size = 20),
        legend.position = c(0.85, 0.85),
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

ggsave(filename = here(wd, "analysis", "output", "graph", "_graph_job_loss_work_arrangement.png"),
       width = 9, height = 12, device = "png", dpi = 300)
