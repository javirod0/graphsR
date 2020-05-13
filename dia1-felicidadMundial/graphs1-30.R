install.packages("readr")
library(tidyr)
library(tidyverse)
library(dplyr)
library(readxl)
library(scales)
library(ggplot2)

felicidad <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-08-07/felicidad.csv")


df<-as.data.frame(felicidad) #convierto a dataframe

anio18<-df[df[, "anio"] == 2018,] #me quedo con año 2018


#grafico mas felices

tophapp18<-head(anio18[order(anio18$escalera_vida,decreasing = TRUE),],10) #me quedo con los 10 mas felices
ggplot(tophapp18,mapping=aes(x=reorder(tophapp18$pais,-tophapp18$escalera_vida),y=tophapp18$escalera_vida)) + 
  geom_col(width = 0.6) + 
  geom_text(position="identity", aes(label=round(tophapp18$escalera_vida,2),
                                     x=reorder(tophapp18$pais,tophapp18$escalera_vida),y=tophapp18$escalera_vida), size = 3, col = "white", family = "Teen Regular") + 
  ylim(0,10) +
  labs(title = "10 paises mas felices del mundo",
       caption = "Fuente: World Happiness Report 2020",
       x = "Paises") + 
  scale_fill_manual(values = tophapp18$escalera_vida, guide = guide_legend(reverse = FALSE)) +
  theme_minimal(base_family = "Teen Regular", base_size = 10) +
  theme(plot.background = element_rect(fill = "#252a32", color = "black"),
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5, colour = "white", margin = margin(t = 5, b = 10)),
        plot.caption = element_text(size = 5, colour = "white"),
        axis.title.y = element_blank(),
        axis.title.x = element_text(colour = "gray90", size = 12, hjust = 1),
        axis.text = element_text(colour = "gray90"),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(colour = "gray30", size = 0.1),
        panel.grid.minor.x = element_line(colour = "gray30", size = 0.1),
        legend.position="bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 12, family = "Teen Regular", face = "bold", colour = "lightblue3"),
        legend.key.size = unit(.2, "cm"))
  


#grafico menos felices
pophapp18<-head(anio18[order(anio18$escalera_vida),],10) #me quedo con los 10 menos felices
ggplot(pophapp18,
       mapping=aes(x=reorder(pophapp18$pais,-pophapp18$escalera_vida),y=pophapp18$escalera_vida)) +
  geom_col(width = 0.6) + geom_text(position="identity",aes(label=round(pophapp18$escalera_vida,2),x=reorder(pophapp18$pais,pophapp18$escalera_vida),y=pophapp18$escalera_vida), size = 3, col = "white", family = "Teen Regular") +
  ylim(0,10) +
  labs(title = "10 paises menos felices del mundo",
       caption = "Fuente: World Happiness Report 2020",
       x = "Paises") + 
  scale_fill_manual(values = pophapp18$escalera_vida, guide = guide_legend(reverse = FALSE)) +
  theme_minimal(base_family = "Teen Regular", base_size = 10) +
  theme(plot.background = element_rect(fill = "#252a32", color = "black"),
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5, colour = "white", margin = margin(t = 5, b = 10)),
        plot.caption = element_text(size = 5, colour = "white"),
        axis.title.y = element_blank(),
        axis.title.x = element_text(colour = "gray90", size = 12, hjust = 1),
        axis.text = element_text(colour = "gray90"),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(colour = "gray30", size = 0.1),
        panel.grid.minor.x = element_line(colour = "gray30", size = 0.1),
        legend.position="bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 12, family = "Teen Regular", face = "bold", colour = "lightblue3"),
        legend.key.size = unit(.2, "cm"))
























 

