library(showtext)
library(haven)
library(tidyverse)

data = read_dta("build/output/regression/main_data.dta")

font_add_google(name = "Roboto", family = "roboto")
showtext_auto()

df = data %>%
  select(year_quarter, position_names, position_transition, position, sector_code, weights)

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
  group_by(sector_code) %>%
  summarise(num_weights = sum(num_weights, na.rm = TRUE),
            den_weights = sum(den_weights, na.rm = TRUE))

df = df %>%
  mutate(job_loss = (num_weights/den_weights)*100)

df = df %>%
  mutate(sector_code = as.factor(sector_code))

df = df %>%
  filter(sector_code!= "NA")

df = df %>%
  mutate(sector = case_when(sector_code == "Administracao Publica, Defesa e Seguridade Social" ~
          "Public Administration, Defense, and Social Security",
                            sector_code == "Agricultura, Pecuaria, Producao Florestal e Pesca" ~
                            "Agriculture, Livestock, Forestry Production, Fishing, and Aquaculture",
                            sector_code == "Agropecuaria" ~ "Farming",
                            sector_code == "Agua, Esgoto, Gestao de Residuos e Descontaminacao" ~
                              "Water, Sewage, Waste Management, and Decontamination",
                            sector_code == "Alojamento e Alimentacao" ~ "Lodging and Food Service",
                            sector_code == "Artes, Cultura, Esportes e Recreacao" ~
                              "Arts, Culture, Sports, and Recreation",
                            sector_code == "Atividades Administrativas e Servicos Complementares" ~
                              "Administrative and Support Services",
                            sector_code == "Atividades Financeiras e de Seguros" ~ "Financial and Insurance Activities",
                            sector_code == "Atividades Imobiliarias" ~ "Real Estate Activities",
                            sector_code == "Atividades Mal Definidas" ~ "Undefined Activities",
                            sector_code == "Atividades Profissionais, Cientificas e Tecnicas" ~
                              "Professional, Scientific, and Technical Activities",
                            sector_code == "Comercio e Reparacao de Veiculos Automotores" ~
                              "Trade and Repair of Motor Vehicles",
                            sector_code == "Comercio, exceto de Veiculos Automotores e Motocicletas" ~
                              "Trade, Except for Motor Vehicles and Motorcycles",
                            sector_code == "Construcao" ~ "Construction",
                            sector_code == "Educacao" ~ "Education",
                            sector_code == "Eletricidade e Gas" ~ "Electricity and Gas",
                            sector_code == "Industrias de Transformacao" ~ "Manufacturing Industries",
                            sector_code == "Industrias Extrativas" ~ "Extractive Industries",
                            sector_code == "Informacao e Comunicacao" ~ "Information and Communication",
                            sector_code == "Organismos Internacionais" ~ "International Organizations",
                            sector_code == "Outras Atividades de Servicos" ~ "Other Service Activities",
                            sector_code == "Saude Humana e Servicos Sociais" ~ "Human Health and Social Services",
                            sector_code == "Transporte, Armazenagem e Correio" ~ "Transportation, Storage, and Postal Services",
                            sector_code == "Servicos Domesticos" ~ "Domestic Services"
                            ))

df = df %>%
  mutate(sector = as.factor(sector))

df = df %>%
  arrange(desc(job_loss))

graph = ggplot(df, aes(x = reorder(sector, job_loss))) +
  geom_bar(aes(y = job_loss, fill = job_loss), stat = "identity") +
  coord_flip() +
  labs(x = "Sectors", y = "Job Loss %") + 
  theme_bw() +
  theme(text = element_text(family = "roboto"),
        legend.title = element_blank(), 
        legend.text = element_text(size = 20),
        legend.position = "none",
        axis.title = element_text(size = 80, hjust = 0.5),
        strip.text = element_text(size = 18, face = "bold", hjust = 0.5),
        axis.line = element_line(linewidth = 0.75, colour = "black"),
        axis.text.x = element_text(family = "Helvetica",
                                   colour = "black",
                                   size = rel(3.0),
                                   face = "bold"),
        axis.text.y = element_text(
          family = "Helvetica",
          colour = "black",
          size = rel(3.0),
          face = "bold"
        ))

graph

ggsave(filename = here(wd, "analysis", "output", "graph", "_graph_job_loss_sectors.png"),
       width = 9, height = 12, device = "png", dpi = 300)
