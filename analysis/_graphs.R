library(extrafont)

font_import(path = "C:/Users/Fredie/AppData/Local/Microsoft/Windows/Fonts")
y
loadfonts(device = "win")

library(ggsci)
library(RColorBrewer)
library(ghibli)
library(dichromat)
library(rcartocolor)
library(grid)
library(gridExtra)

 ## Gráfico Matrizes
 
 matrizes = read.csv("./input/transicoes_por_educ_3_x_3.csv")
 
 ## Formal - Não empregado
 
 matriz_formal_n_empreg = matrizes %>%
   filter(posicao_inicial == 1 &
            posicao_final == 3) %>%
   mutate(educ = as.character(educ))
 
 graf_f_n_empreg = ggplot(matriz_formal_n_empreg, aes(x = trim, y = 100*transition, 
                                color = educ, group = educ)) +
   scale_color_manual(labels = c("Incomplete Primary School",
                                 "Incomplete High School",
                                 "Incomplete College",
                                 "Complete College"), values = carto_pal(name = "Safe")) +
   geom_line(size = 2) +
   geom_point(size = 2) +
   labs(x = "", y = "", title = "Formal to Non-Employee (%)") +
   theme_minimal() +
   theme(text = element_text(family = "LM Roman 10"),
         plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
         legend.title = element_blank(),
         legend.text = element_text(size = 20)) +
   scale_x_discrete(breaks = paste0(2012:2021, "_1"),
                    labels = 2012:2021) +
   guides(fill=guide_legend(nrow=4,byrow=TRUE))
 
 
 ## Informal - Não empregado
 
 matriz_informal_n_empreg = matrizes %>%
   filter(posicao_inicial == 2 &
            posicao_final == 3) %>%
   mutate(educ = as.character(educ))
 
 graf_i_n_empreg = ggplot(matriz_informal_n_empreg, aes(x = trim, y = 100*transition, 
                                color = educ, group = educ)) +
   scale_color_manual(labels = c("Incomplete Primary School",
                                 "Incomplete High School",
                                 "Incomplete College",
                                 "Complete College"), values = carto_pal(name = "Safe")) +
   geom_line(size = 2) +
   geom_point(size = 2) +
   labs(x = "", y = "", title = "Informal to Non-Employee (%)") +
   theme_minimal() +
   theme(text = element_text(family = "LM Roman 10"),
         plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
         legend.title = element_blank(),
         legend.text = element_text(size = 20)) +
   scale_x_discrete(breaks = paste0(2012:2021, "_1"),
                    labels = 2012:2021)+
   guides(fill=guide_legend(nrow=4,byrow=TRUE))
 
 
 
 ## Ocupado - Não empregado
 
 matrizes = read.csv("./input/transicoes_por_educ_2_x_2.csv")
 
 matriz_ocupado_n_empreg = matrizes %>%
   filter(posicao_inicial == 1 &
            posicao_final == 2) %>%
   mutate(educ = as.character(educ))
 
 graf_occupied_n_empreg = ggplot(matriz_ocupado_n_empreg, aes(x = trim, y = 100*transition, 
                                color = educ, group = educ)) +
   scale_color_manual(labels = c("Incomplete Primary School",
                                 "Incomplete High School",
                                 "Incomplete College",
                                 "Complete College"), values = carto_pal(name = "Safe")) +
   geom_line(size = 2) +
   geom_point(size = 2) +
   labs(x = "", y = "", title = "Occupied to Non-Employee (%)") +
   theme_minimal() +
   theme(text = element_text(family = "LM Roman 10"),
         plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
         legend.title = element_blank(),
         legend.text = element_text(size = 20)) +
   scale_x_discrete(breaks = paste0(2012:2021, "_1"),
                    labels = 2012:2021) +
   guides(fill=guide_legend(nrow=4,byrow=TRUE))
 
 
 
 # Ocupados self employed formal e informal
 
 base = read.csv("./input/prop_self_employed_inss_ocupados_por_educ.csv")
 
 base = base %>%
   mutate(educ_level = case_when(educ == 1 ~ 1,
                                 educ == 2 ~ 2,
                                 educ == 3 ~ 3,
                                 educ == 4 ~ 4)) %>%
   select(-c(X))
 
 base2 = read.csv("./input/prop_self_employed_s_inss_ocupados_por_educ.csv")
 
 base2 = base2 %>%
   mutate(educ_level = case_when(educ == 1 ~ 5,
                                 educ == 2 ~ 6,
                                 educ == 3 ~ 7,
                                 educ == 4 ~ 8)) %>%
   select(-c(X))
 
 
 base_final = bind_rows(base, base2) 
 
 base_final = base_final %>% distinct()
 
 
 ocup_self_employed = base_final %>%
   mutate(educ_level = as.character(educ_level))
 
 graf_occupied_self_employed = ggplot(ocup_self_employed, aes(x = year_quarter, y = 100*proporcao, 
                                                                                     color = educ_level, group = educ_level)) +
   scale_color_manual(labels = c("(Formal) Incomplete Primary School",
                                 "(Formal) Incomplete High School",
                                 "(Formal) Incomplete College",
                                 "(Formal) Complete College",
                                 "(Informal) Incomplete Primary School",
                                 "(Informal) Incomplete High School",
                                 "(Informal) Incomplete College",
                                 "(Informal) Complete College"), values = c(scales::seq_gradient_pal("#8AC5FF", "#0661BB")(seq(0,1, length.out = 4)),
                                                                         scales::seq_gradient_pal("#FE7070", "#BC0404")(seq(0,1, length.out = 4)))) +
   geom_line(size = 2) +
   geom_point(size = 2) +
   labs(x = "", y = "", title = "Self-Employed (%)") +
   theme_minimal() +
   theme(text = element_text(family = "LM Roman 10"),
         plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
         legend.title = element_blank(),
         axis.text = element_blank(), 
         legend.text = element_text(size = 15)) +
   scale_x_discrete(breaks = paste0(2012:2021, "_1"),
                    labels = 2012:2021) +
   guides(fill=guide_legend(nrow=2,byrow=TRUE))
 
 
 source("./analysis/_prop_occupied.R")

 
 # Ocupados carteira assinada e sem carteira assinada 2012-2021 
 
 base = read.csv("./input/prop_cart_ass_ocupados_por_educ.csv")
 
 base = base %>%
   mutate(educ_level = case_when(educ == 1 ~ 1,
                                 educ == 2 ~ 2,
                                 educ == 3 ~ 3,
                                 educ == 4 ~ 4))
 
 base2 = read.csv("./input/prop_s_cart_ass_ocupados_por_educ.csv")
 
 base2 = base2 %>%
   mutate(educ_level = case_when(educ == 1 ~ 5,
                                 educ == 2 ~ 6,
                                 educ == 3 ~ 7,
                                 educ == 4 ~ 8))
 
 
 base_final = bind_rows(base, base2)
 
 base_final = base_final %>% distinct()

 
 ocup_private_public_workers = base_final %>%
   mutate(educ_level = as.character(educ_level))
 
 graf_occupied_private_public_workers = ggplot(ocup_private_public_workers, aes(x = year_quarter, y = 100*proporcao, 
                                                              color = educ_level, group = educ_level)) +
   scale_color_manual(labels = c("(Formal) Incomplete Primary School",
                                 "(Formal) Incomplete High School",
                                 "(Formal) Incomplete College",
                                 "(Formal) Complete College",
                                 "(Informal) Incomplete Primary School",
                                 "(Informal) Incomplete High School",
                                 "(Informal) Incomplete College",
                                 "(Informal) Complete College"), values = c(scales::seq_gradient_pal("#8AC5FF", "#0661BB")(seq(0,1, length.out = 4)),
                                                                                    scales::seq_gradient_pal("#FE7070", "#BC0404")(seq(0,1, length.out = 4)))) +
   geom_line(size = 2) +
   geom_point(size = 2) +
   labs(x = "", y = "", title = "Private and Public Workers (%)") +
   theme_minimal() +
   theme(text = element_text(family = "LM Roman 10"),
         plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
         legend.title = element_blank(), 
         legend.text = element_text(size = 15)) +
   scale_x_discrete(breaks = paste0(2012:2021, "_1"),
                    labels = 2012:2021) +
   guides(fill=guide_legend(nrow=2,byrow=TRUE))
 