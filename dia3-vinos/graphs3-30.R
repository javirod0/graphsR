vinos <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-06-12/vinos.csv")

library(tidyverse)
library(extrafont)
library('unikn')    


#Defino paleta de colores
palette30<-usecol(pal_unikn_pair)


font_import()
 # la primera vez ejecutar font_import()
font<- "Trebuchet MS" #Fuente que voy a utlizar
#Defino variedades a estudiar
variedades<-c("Merlot", "Malbec", "Malbec-Carmenère", "Cabernet Sauvignon", "Carmenère-Cabernet Sauvignon", "Cabernet Sauvignon-Carmenère", "Syrah","Chardonnay", "Cabernet Sauvignon-Syrah", 
              "Malbec-Cabernet", "Malbec-Cabernet Franc", "Malbec-Cabernet Sauvignon", "Malbec-Petit Verdot", "Tannat", "Tannat-Cabernet Franc", "Tannat-Merlot", "Tannat-Syrah", "Cabernet Franc",
              "Carmenère", "Sauvignon Blanc", "Ensamblaje Tinto", "Albariño", "Rosé", "Pinot Noir", "Tempranillo-Tannat")

#Filtro por pais, puntajes y variedades
chilenos<-vinos%>%filter(pais=="Chile"&puntos>90&variedad%in%variedades)
argentinos<-vinos%>%filter(pais=="Argentina"&puntos>90&variedad%in%variedades)
uruguayos<-vinos%>%filter(pais=="Uruguay"&puntos>70&variedad%in%variedades)

#grafico. cambiar tabla base para cada caso
chilenos%>%
  ggplot(aes(x= precio, y = puntos, color=variedad)) + geom_jitter(alpha=0.5,size=4) +
  theme_bw() +
  xlim(0, 450)  + ylim(90,94) +
  theme_minimal(base_family = "Teen Regular", base_size = 10) +
  theme(plot.background = element_rect(fill = "#252a32", color = "white"),
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5, colour = "white", margin = margin(t = 5, b = 10)),
        plot.caption = element_text(size = 5, colour = "white"),
        axis.title.x = element_text(colour = "gray90", size = 10, hjust = 1),
        axis.title.y = element_text(colour = "gray90", size = 10, hjust = 0.5),
        axis.text = element_text(colour = "gray90"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_line(colour = "gray30", size = 0.1),
        panel.grid.major.x = element_line(colour = "gray30", size = 0.1),
        panel.grid.minor.x = element_line(colour = "gray30", size = 0.1),
        legend.position="bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 8, family = "Teen Regular", face = "bold", colour = "lightblue3"),
        legend.key.size = unit(.2, "cm")) +
  scale_color_manual(values = palette30) +
  labs(title= "Mejores vinos Chilenos",  
       y = "Puntuación 0-100", 
       x = "US$", 
       caption = "Fuente https://www.kaggle.com/zynicide/wine-reviews/ \n
       Hecho por: @javirodriguezu")
