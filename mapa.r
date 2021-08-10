library(tidyverse)
library(ggspatial)
library(viridis)
library(viridisLite)
library(ggplot2)
library(sf)
library(tmap)
library(plotly) 
library(ggiraph)
library(sp)
library(rgdal)
library(leaflet)
library(RColorBrewer)
library("htmltools")

setwd("C:/Users/vvillegas/Desktop/MAPAS")
pac<-read.csv("D:/DI/Projects/BI/cDAT/hos/pac2.csv")

## HOSPITALIZACION###########
#############################
#############################

pac_hos<-pac[which(pac$are2=="HOS" & pac$Cantidad.de.Pacientes==1 & pac$Elegible=="Elegible"),]

#######################################
#######################################

edo<-st_read("C:/Users/vvillegas/Desktop/MAPAS/15_mexico/conjunto de datos/15mun.shp")
edo<-st_transform(edo,crs = 6372)
edo$CVE_MUN<-as.numeric(edo$CVE_MUN)

mex<-st_read("C:/Users/vvillegas/Desktop/MAPAS/09_ciudaddemexico/conjunto_de_datos/09mun.shp")
mex<-st_transform(mex,crs = 6372)
mex$CVE_MUN<-as.numeric(mex$CVE_MUN)


### Estado de méxico
pac_edo<-pac_hos[pac_hos$CVE_EDO%in%15,]
pacientes_edo<-pac_edo %>% group_by(CVE_MUN)%>%
  summarise(Pacientes=n())
pac_edo1<-pac_edo %>% group_by(CVE_MUN)%>%
  summarise(Pacientes=n())
pac_edo1$Pacientes2<-log(pac_edo1$Pacientes)
dt_edo<- edo %>% left_join(pac_edo1)



### CIUDAD DE MÉXICO
pac_mex<-pac_hos[pac_hos$CVE_EDO%in%09,]
pacientes_mex<-pac_mex %>% group_by(CVE_MUN)%>%
  summarise(Pacientes=n())
pac_mex1<-pac_mex %>% group_by(CVE_MUN)%>%
  summarise(Pacientes=n())
pac_mex1$Pacientes2<-log(pac_mex1$Pacientes)
dt_mex<- mex %>% left_join(pac_mex1)
dt_mex$NOMGEO<-c("Azcapotzalco","Coyoacan","La Magdalena Contreras","Tlalpan",
                 "Xochimilco", "Alvaro Obregon","Cuauhtemoc","Iztapalapa",
                 "Tlahuac","Cuajimalpa de Morelos","Miguel Hidalgo","Benito Juarez",   
                 "Gustavo A. Madero","Milpa Alta","Iztacalco","Venustiano Carranza")


### MAPA INTERACTIVO  CIUDAD DE MEXICO Y ESTADO DE MEXICO
d<-max(pacientes_edo$Pacientes,pacientes_mex$Pacientes)
my_breaks <-c(0,round(d/3,0),round(2*d/3,0),d)

dt3<-rbind(dt_edo,dt_mex)
dt3<-cbind(dt3$NOMGEO,dt3)


dt3[which(is.na(dt3$Pacientes2)),7]<-0
dt3[which(is.na(dt3$Pacientes)),6]<-0

dt3$NOMCOM<-paste(dt3$NOMGEO,dt3$Pacientes) # Etiquetas 

colnames(dt3)[7]="Px" #Columna del filtro

gg <- ggplot(dt3) +
  geom_sf(colour = "white") +
  geom_sf_interactive(aes(
    fill = Px , tooltip = NOMCOM, data_id = NOMCOM),
    show.legend = "point")+
  theme(axis.line = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.title = element_blank())+
  scale_fill_viridis(option="mako",direction=-1,labels=my_breaks)


x <- girafe( ggobj = gg)
x 


HTMLOut <- "C:/DI/Solution/Tomcat/Tomcat 9.0/webapps/ROOT/Mapa.html"

save_html(list(x), HTMLOut)

################################################################################
################################################################################
###############################################################################


####URGENCIAS #####

pac_urg<-pac[which(pac$are2=="URG" & pac$Cantidad.de.Pacientes==1 & pac$Elegible=="Elegible"),]


### Estado de méxico
pac_edo<-pac_urg[pac_urg$CVE_EDO%in%15,]
pacientes_edo<-pac_edo %>% group_by(CVE_MUN)%>%
  summarise(Pacientes=n())
pac_edo1<-pac_edo %>% group_by(CVE_MUN)%>%
  summarise(Pacientes=n())
pac_edo1$Pacientes2<-log(pac_edo1$Pacientes)
dt_edo<- edo %>% left_join(pac_edo1)



### CIUDAD DE MÉXICO
pac_mex<-pac_urg[pac_urg$CVE_EDO%in%09,]
pacientes_mex<-pac_mex %>% group_by(CVE_MUN)%>%
  summarise(Pacientes=n())
pac_mex1<-pac_mex %>% group_by(CVE_MUN)%>%
  summarise(Pacientes=n())
pac_mex1$Pacientes2<-log(pac_mex1$Pacientes)
dt_mex<- mex %>% left_join(pac_mex1)
dt_mex$NOMGEO<-c("Azcapotzalco","Coyoacan","La Magdalena Contreras","Tlalpan",
                 "Xochimilco", "Alvaro Obregon","Cuauhtemoc","Iztapalapa",
                 "Tlahuac","Cuajimalpa de Morelos","Miguel Hidalgo","Benito Juarez",   
                 "Gustavo A. Madero","Milpa Alta","Iztacalco","Venustiano Carranza")


### MAPA INTERACTIVO  CIUDAD DE MEXICO Y ESTADO DE MEXICO
d1<-max(pacientes_edo$Pacientes,pacientes_mex$Pacientes)
my_breaks1 <-c(0,round(d1/4,0),round(2*d1/4,0),round(3*d1/4,0),d1)

dt4<-rbind(dt_edo,dt_mex)
dt4<-cbind(dt4$NOMGEO,dt4)


dt4[which(is.na(dt4$Pacientes2)),7]<-0
dt4[which(is.na(dt4$Pacientes)),6]<-0

dt4$NOMCOM<-paste(dt4$NOMGEO,dt4$Pacientes) # Etiquetas 

colnames(dt4)[7]="Px" #Columna del filtro

gg <- ggplot(dt4) +
  geom_sf(colour = "white") +
  geom_sf_interactive(aes(
    fill = Px , tooltip = NOMCOM, data_id = NOMCOM),
    show.legend = "point")+
  theme(axis.line = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.title = element_blank())+
  scale_fill_viridis(option="mako",direction=-1,labels=my_breaks1)


x_urg <- girafe( ggobj = gg)
x_urg


HTMLOut <- "C:/DI/Solution/Tomcat/Tomcat 9.0/webapps/ROOT/Mapa_urg.html"

save_html(list(x_urg), HTMLOut)
