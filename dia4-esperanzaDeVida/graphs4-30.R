
library(tidyverse)
library(extrafont)
font_import()
# la primera vez ejecutar font_import()
font<- "Trebuchet MS"
gapminder <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-04-24/gapminder_es.csv")

ggplot(gapminder, aes(anio, esperanza_de_vida)) +
  geom_point(size = 0.7, alpha = 0.4, color="#ffffff") +
  geom_smooth(method = "loess",se=FALSE) +
  scale_color_discrete(guide = "none") +
  scale_y_continuous("Esperanza de vida 1-100")  +
  facet_wrap(~continente, scale = "fixed") + xlab("Año (1950 - 2006)")+
  theme_bw() +
  theme_minimal(base_family = "Teen Regular", base_size = 12) +
  theme(plot.background = element_rect(fill = "#252a32", color = "white"),
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5, colour = "white", margin = margin(t = 5, b = 10)),
        plot.caption = element_text(size = 6, colour = "white"),
        axis.title.x = element_text(colour = "gray90", size = 10, hjust = 1),
        axis.title.y = element_text(colour = "gray90", size = 10, hjust = 0.5),
        axis.text = element_text(colour = "gray90"),
        strip.text=element_text(colour="gray90"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_line(colour = "gray30", size = 0.1),
        panel.grid.major.x = element_line(colour = "gray30", size = 0.1),
        panel.grid.minor.x = element_line(colour = "gray30", size = 0.1),
        legend.position="bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 8, family = "Teen Regular", face = "bold", colour = "lightblue3"),
        legend.key.size = unit(.2, "cm")) +
  labs(title= "Esperanza de vida al nacer", 
       caption = "Fuente: Banco Mundial \n
       Hecho por: @javirodriguezu")




paises<-c("Zimbabwe","Ruanda","Lesotho","Botswana")

africa<-gapminder%>%filter(continente=="Africa"&pais%in%paises)
ggplot(africa, aes(anio, esperanza_de_vida)) +
  geom_point(size = 0.7, alpha = 0.4, color="#ffffff") +
  geom_smooth(method = "loess",se=FALSE) +
  scale_color_discrete(guide = "none") +
  scale_y_continuous("Esperanza de vida 1-100")  +
  facet_wrap(~pais, scale = "fixed") + xlab("Año (1950 - 2006)")+
  theme_bw() +
  theme_minimal(base_family = "Teen Regular", base_size = 12) +
  theme(plot.background = element_rect(fill = "#252a32", color = "white"),
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5, colour = "white", margin = margin(t = 5, b = 10)),
        plot.caption = element_text(size = 6, colour = "white"),
        axis.title.x = element_text(colour = "gray90", size = 10, hjust = 1),
        axis.title.y = element_text(colour = "gray90", size = 10, hjust = 0.5),
        axis.text = element_text(colour = "gray90"),
        strip.text=element_text(colour="gray90"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_line(colour = "gray30", size = 0.1),
        panel.grid.major.x = element_line(colour = "gray30", size = 0.1),
        panel.grid.minor.x = element_line(colour = "gray30", size = 0.1),
        legend.position="bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 8, family = "Teen Regular", face = "bold", colour = "lightblue3"),
        legend.key.size = unit(.2, "cm")) +
  labs(title= "Esperanza de vida al nacer en paises Africanos", 
       caption = "Fuente: Banco Mundial \n
       Hecho por: @javirodriguezu")



paises_as<-c("Camboya","China")

asia<-gapminder%>%filter(continente=="Asia"&pais%in%paises_as)
ggplot(asia, aes(anio, esperanza_de_vida)) +
  geom_point(size = 0.7, alpha = 0.4, color="#ffffff") +
  geom_smooth(method = "loess",se=FALSE) +
  scale_color_discrete(guide = "none") +
  scale_y_continuous("Esperanza de vida 1-100")  +
  facet_wrap(~pais, scale = "fixed") + xlab("Año (1950 - 2006)")+
  theme_bw() +
  theme_minimal(base_family = "Teen Regular", base_size = 12) +
  theme(plot.background = element_rect(fill = "#252a32", color = "white"),
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5, colour = "white", margin = margin(t = 5, b = 10)),
        plot.caption = element_text(size = 6, colour = "white"),
        axis.title.x = element_text(colour = "gray90", size = 10, hjust = 1),
        axis.title.y = element_text(colour = "gray90", size = 10, hjust = 0.5),
        axis.text = element_text(colour = "gray90"),
        strip.text=element_text(colour="gray90"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_line(colour = "gray30", size = 0.1),
        panel.grid.major.x = element_line(colour = "gray30", size = 0.1),
        panel.grid.minor.x = element_line(colour = "gray30", size = 0.1),
        legend.position="bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 8, family = "Teen Regular", face = "bold", colour = "lightblue3"),
        legend.key.size = unit(.2, "cm")) +
  labs(title= "Esperanza de vida al nacer en paises Asiaticos", 
       caption = "Fuente: Banco Mundial \n
       Hecho por: @javirodriguezu")











