library(extrafont)
library(tidyverse)
library(showtext)
library(ggsci)
library(RColorBrewer)
library(ghibli)
library(dichromat)
library(rcartocolor)
library(grid)
library(gridExtra)
library(ggthemes)

 ## Gráfico Matrizes

matrizes = read.csv("./output/transicoes_por_educ_2_x_2.csv")
 
matrizes1 = read.csv("./output/transicoes_por_educ_3_x_3.csv")

font_add_google(name = "Roboto", family = "roboto")
showtext_auto()


changing_df = function(df){
  
df = df %>%
  mutate(higher_educ_label = case_when(educ %in% c(1) ~"Incomplete Primary School",
                                educ %in% c(2) ~ "Incomplete High School",
                                educ %in% c(3) ~ "Incomplete College",
                               educ %in% c(4) ~ "Complete College"))%>%
  mutate(higher_educ_label = factor(higher_educ_label, ordered = TRUE, levels = c("Incomplete Primary School",
                                                                                  "Incomplete High School",
                                                                                  "Incomplete College",
                                                                                  "Complete College"))) %>%
  mutate(transition = transition*100)
    
}

matrizes = changing_df(matrizes)
matrizes1 = changing_df(matrizes1)

compare_transitions = function(df, pos1, pos2, var){
   
   df %>%
     filter(posicao_inicial == pos1,
            posicao_final == pos2) %>%
     ggplot(aes(x = trim, y = get(var), group = higher_educ_label, color = higher_educ_label)) +
     geom_point(size = 3) +
     geom_line(size = 2) +
     
     scale_color_manual(name = "Education Level",
                        values = carto_pal(name = "Safe")) +
     
     labs(x = "Year", y = "Proportion") + ggtitle("") +
     
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
     
     scale_x_discrete(breaks = paste0(2012:2022, "_1"),
                      labels = 2012:2022) +
    guides(fill = guide_legend(nrow = 4, byrow = TRUE))
   
}

compare_transitions(matrizes, 1, 2, "transition")
 