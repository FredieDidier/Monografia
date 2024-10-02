library(showtext)
library(haven)
library(tidyverse)

data = read_dta("build/output/regression/main_data.dta")

font_add_google(name = "Roboto", family = "roboto")
showtext_auto()

df = data %>%
  select(year_quarter, position_names, position_transition, position, occupation_code, weights)

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
  group_by(occupation_code) %>%
  summarise(num_weights = sum(num_weights, na.rm = TRUE),
            den_weights = sum(den_weights, na.rm = TRUE))

df = df %>%
  mutate(job_loss = (num_weights/den_weights)*100)

df = df %>%
  mutate(occupation_code = as.factor(occupation_code))

df = df %>%
  filter(occupation_code!= "NA")

df = df %>%
  mutate(occupation = case_when(occupation_code == "Dirigentes e Gerentes" ~
                              "Directors and Managers",
                            occupation_code == "Ocupacoes Elementares" ~
                              "Elementary Occupations",
                            occupation_code == "Operadores de Instalacoes e Maquinas e Montadoras" ~ "Installation and Machine Operators and Assemblers",
                            occupation_code == "Profissionais das Ciencias e Intelectuais" ~
                              "Science and Intellectual Professionals",
                            occupation_code == "Tecnicos e Profissionais de Nivel Medio" ~ "Mid-Level Technicians and Professionals",
                            occupation_code == "Trabalhadores Qualificados da Agropecuaria, Florestais, da caca e da pesca" ~
                              "Qualified Agricultural, Forestry, Hunting and Fishing Workers",
                            occupation_code == "Trabalhadores Qualificados, Operarios e Artesaos da Construcao, Das Artes Mecanicas e Outros Oficios" ~
                              "Skilled Workers, Construction Workers and Craftsmen, Mechanical Arts and Other Crafts",
                            occupation_code == "Trabalhadores de Apoio Administrativo" ~ "Administrative Support Workers",
                            occupation_code == "Trabalhadores dos Servicos, Vendedores dos Comercios e Mercados" ~ 
                              "Service Workers, Trade and Market Sellers",
                            occupation_code == "Membros das Forcas Armadas, Policiais e Bombeiros Militares" ~ "Members of the Armed Forces, Police and Military Firefighters"
                            
  ))

df = df %>%
  mutate(occupation = as.factor(occupation))

df = df %>%
  arrange(desc(job_loss))

graph = ggplot(df, aes(x = reorder(occupation, job_loss))) +
  geom_bar(aes(y = job_loss, fill = job_loss), stat = "identity") +
  coord_flip() +
  labs(x = "", y = "Job Loss %") +
  theme_bw() +
  theme(text = element_text(family = "roboto"),
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        legend.title = element_blank(), 
        legend.text = element_text(size = 20),
        legend.position = "none",
        axis.title = element_text(size = 18, face = "bold", hjust = 0.5),
        strip.text = element_text(size = 18, face = "bold", hjust = 0.5),
        axis.title.y = element_text(size = 40),
        axis.line = element_line(linewidth = 0.75, colour = "black"),
        axis.text = element_text(
          family = "Helvetica",
          colour = "black",
          size = rel(1.2),
          face = "bold"
        ))

graph

ggsave(filename = here(wd, "analysis", "output", "graph", "_graph_job_loss_occupation.png"),
       width = 9, height = 12, device = "png", dpi = 300)

