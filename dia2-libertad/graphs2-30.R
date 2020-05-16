library(tidyverse)
library(plyr)
library(dplyr)
library(ggplot2)
library(plotly)
library(tidyr)
library(tidyverse)
library(dplyr)
library(readxl)
library(scales)
library(ggplot2)
libertad <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-08-14/libertad.csv")
df<-as.data.frame(libertad) #convierto a dataframe

#agrupo las regiones
regionEuropa<-c("Europa del Este", "Europa Occidental")
regionAsia<-c("Sur de Asia", "Cáucaso y Asia Central","Asia oriental")
regionAfrica<-c("Oriente Medio y Norte de África","África Subsahariana")

#creo tabla de regiones
america<-df%>%
  filter(region == "Latinoamérica y el Caribe")
oceania<-df%>%
  filter(region=="Oceanía")
europa<-df%>%
  filter(region%in%regionEuropa)
asia<-df%>%
  filter(region%in%regionAsia)
africa<-df%>%
  filter(region%in%regionAfrica)
#creo vector de años

cont<-c("America","Asia","Oceania","Europa","Africa")

am10<-america%>%filter(anio=="2010")%>%summarise(media=mean(libertad_personal_puntaje,na.rm=TRUE))
am11<-america%>%filter(anio=="2011")%>%summarise(media=mean(libertad_personal_puntaje,na.rm=TRUE))
am12<-america%>%filter(anio=="2012")%>%summarise(media=mean(libertad_personal_puntaje,na.rm=TRUE))
am13<-america%>%filter(anio=="2013")%>%summarise(media=mean(libertad_personal_puntaje,na.rm=TRUE))
am14<-america%>%filter(anio=="2014")%>%summarise(media=mean(libertad_personal_puntaje,na.rm=TRUE))
am15<-america%>%filter(anio=="2015")%>%summarise(media=mean(libertad_personal_puntaje,na.rm=TRUE))
am16<-america%>%filter(anio=="2016")%>%summarise(media=mean(libertad_personal_puntaje,na.rm=TRUE))

oc10<-oceania%>%filter(anio=="2010")%>%summarise(media=mean(libertad_personal_puntaje,na.rm=TRUE))
oc11<-oceania%>%filter(anio=="2011")%>%summarise(media=mean(libertad_personal_puntaje,na.rm=TRUE))
oc12<-oceania%>%filter(anio=="2012")%>%summarise(media=mean(libertad_personal_puntaje,na.rm=TRUE))
oc13<-oceania%>%filter(anio=="2013")%>%summarise(media=mean(libertad_personal_puntaje,na.rm=TRUE))
oc14<-oceania%>%filter(anio=="2014")%>%summarise(media=mean(libertad_personal_puntaje,na.rm=TRUE))
oc15<-oceania%>%filter(anio=="2015")%>%summarise(media=mean(libertad_personal_puntaje,na.rm=TRUE))
oc16<-oceania%>%filter(anio=="2016")%>%summarise(media=mean(libertad_personal_puntaje,na.rm=TRUE))

eu10<-europa%>%filter(anio=="2010")%>%summarise(media=mean(libertad_personal_puntaje,na.rm=TRUE))
eu11<-europa%>%filter(anio=="2011")%>%summarise(media=mean(libertad_personal_puntaje,na.rm=TRUE))
eu12<-europa%>%filter(anio=="2012")%>%summarise(media=mean(libertad_personal_puntaje,na.rm=TRUE))
eu13<-europa%>%filter(anio=="2013")%>%summarise(media=mean(libertad_personal_puntaje,na.rm=TRUE))
eu14<-europa%>%filter(anio=="2014")%>%summarise(media=mean(libertad_personal_puntaje,na.rm=TRUE))
eu15<-europa%>%filter(anio=="2015")%>%summarise(media=mean(libertad_personal_puntaje,na.rm=TRUE))
eu16<-europa%>%filter(anio=="2016")%>%summarise(media=mean(libertad_personal_puntaje,na.rm=TRUE))

as10<-asia%>%filter(anio=="2010")%>%summarise(media=mean(libertad_personal_puntaje,na.rm=TRUE))
as11<-asia%>%filter(anio=="2011")%>%summarise(media=mean(libertad_personal_puntaje,na.rm=TRUE))
as12<-asia%>%filter(anio=="2012")%>%summarise(media=mean(libertad_personal_puntaje,na.rm=TRUE))
as13<-asia%>%filter(anio=="2013")%>%summarise(media=mean(libertad_personal_puntaje,na.rm=TRUE))
as14<-asia%>%filter(anio=="2014")%>%summarise(media=mean(libertad_personal_puntaje,na.rm=TRUE))
as15<-asia%>%filter(anio=="2015")%>%summarise(media=mean(libertad_personal_puntaje,na.rm=TRUE))
as16<-asia%>%filter(anio=="2016")%>%summarise(media=mean(libertad_personal_puntaje,na.rm=TRUE))

af10<-africa%>%filter(anio=="2010")%>%summarise(media=mean(libertad_personal_puntaje,na.rm=TRUE))
af11<-africa%>%filter(anio=="2011")%>%summarise(media=mean(libertad_personal_puntaje,na.rm=TRUE))
af12<-africa%>%filter(anio=="2012")%>%summarise(media=mean(libertad_personal_puntaje,na.rm=TRUE))
af13<-africa%>%filter(anio=="2013")%>%summarise(media=mean(libertad_personal_puntaje,na.rm=TRUE))
af14<-africa%>%filter(anio=="2014")%>%summarise(media=mean(libertad_personal_puntaje,na.rm=TRUE))
af15<-africa%>%filter(anio=="2015")%>%summarise(media=mean(libertad_personal_puntaje,na.rm=TRUE))
af16<-africa%>%filter(anio=="2016")%>%summarise(media=mean(libertad_personal_puntaje,na.rm=TRUE))

am<-c(am10$media,am11$media,am12$media,am13$media,am14$media,am15$media,am16$media)
af<-c(af11$media,af11$media,af12$media,af13$media,af14$media,af15$media,af16$media)
as<-c(as11$media,as11$media,as12$media,as13$media,as14$media,as15$media,as16$media)
oc<-c(oc11$media,oc11$media,oc12$media,oc13$media,oc14$media,oc15$media,oc16$media)
eu<-c(eu11$media,eu11$media,eu12$media,eu13$media,eu14$media,eu15$media,eu16$media)


ame<-data.frame(seq(2010,2016,1),am)
afr<-data.frame(seq(2010,2016,1),af)
asi<-data.frame(seq(2010,2016,1),as)
oce<-data.frame(seq(2010,2016,1),oc)
eur<-data.frame(seq(2010,2016,1),eu)

colnames(ame)<-c("anio","value")
colnames(afr)<-c("anio","value")
colnames(asi)<-c("anio","value")
colnames(oce)<-c("anio","value")
colnames(eur)<-c("anio","value")


ggplot(size=1) + 
  geom_line(ame,mapping= aes(x = anio, y = value), color = "chocolate2",size=1.5,face="bold") + 
  geom_line(afr,mapping= aes(x = anio, y = value), color = "red",size=1.5) +
  geom_line(asi,mapping= aes(x = anio, y = value), color = "yellow",size=1.5) +
  geom_line(oce,mapping= aes(x = anio, y = value), color = "salmon",size=1.5) +
  geom_line(eur,mapping= aes(x = anio, y = value), color = "seagreen",size=1.5) + xlim(2010,2016) + ylim(0,10)+
  geom_text(position="identity", size = 3, col = "white", family = "Teen Regular")+
  
  annotate("text", x = 2011, y =5.7 , label = "Africa",col="red") +
  annotate("text", x = 2013, y = 7.05 , label = "Asia",col="yellow") +
  annotate("text", x = 2014, y = 7.05 , label = "America",col="chocolate2") +
  annotate("text", x = 2015, y = 8.05 , label = "Oceania",col="salmon") +
  annotate("text", x = 2012, y = 9 , label = "Europa",col="seagreen") +
 
  
   ylab('Indice')  +
  labs(title = "Indice de Libertad Humana",col="black",
       caption = "Fuente: https://www.cato.org/human-freedom-index-new",
       x = "Año") + scale_fill_brewer(palette='rainbow', name='Background:',
                                      breaks=c('blue','red','yellow','black','pink'),
                                      labels=c('America','Africa','Asia','Oceania','Europa'))+
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






















































