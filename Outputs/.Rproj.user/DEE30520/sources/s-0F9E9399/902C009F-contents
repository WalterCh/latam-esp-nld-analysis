#Seleccionar paises
library(readr)
VariosPaises_Covid19 <- read.csv("VariosPaises_Covid19.csv", sep=";")
(paises<-unique(VariosPaises_Covid19$COUNTRY))
library(dplyr)

library(ggplot2)
VariosPaises_Covid19$FECHA<-as.Date(VariosPaises_Covid19$FECHA, "%d/%m/%y")
#VariosPaises_Covid19<-VariosPaises_Covid19[,1:7]
#write.csv(VariosPaises_Covid19, "VariosPaises_Covid19.csv")
#ploteamos los casos por fecha usando facetas por pais
ggplot(data=VariosPaises_Covid19)+
  geom_point(mapping=aes (x=FECHA, y=FALLECIDOS, color=COUNTRY))+
  facet_wrap(~COUNTRY, nrow = 3)
#CALCULO DEL INDICADOR DE PELIGROSIDAD:
attach(VariosPaises_Covid19)
IP<-(CONTAGIADOS+FALLECIDOS)-RECUPERADOS
VariosPaises_Covid19$RECUPERADOS<-as.double(VariosPaises_Covid19$RECUPERADOS)
VariosPaises_Covid19<-mutate(VariosPaises_Covid19, IP=CONTAGIADOS+FALLECIDOS-RECUPERADOS)



#Indice de peligrosidad de un solo pais: ejemplo Peru
Peru<-filter(VariosPaises_Covid19, COUNTRY=="PERU")
Spain<-filter(VariosPaises_Covid19, COUNTRY=="SPAIN")

#REPRESENTACIÓN DEL INDICE DE PELIGROSIDAD POR PAISES:

ggplot(data=Spain)+
  geom_point(mapping=aes (x=FECHA, y=IP, color="red"))+
  geom_smooth(mapping=aes (x=FECHA, y=IP))


ggplot(data=VariosPaises_Covid19)+
  geom_point(mapping=aes (x=FECHA, y=CONTAGIADOS, color=COUNTRY))+
  geom_smooth(mapping=aes (x=FECHA, y=CONTAGIADOS))
SN<-filter(VariosPaises_Covid19, (COUNTRY=="SPAIN")|(COUNTRY=="NETHERLAND"))
ggplot(data=SN)+
  geom_point(mapping=aes (x=FECHA, y=CONTAGIADOS, color=COUNTRY))+
  geom_smooth(mapping=aes (x=FECHA, y=CONTAGIADOS, linetype=COUNTRY))


attach(VariosPaises_Covid19)
FECHA<-as.Date(FECHA, "%d/%m/%y")
MARZO2020<-filter(VariosPaises_Covid19,months(FECHA)=="marzo")

MARZO2020<-filter(MARZO2020,(COUNTRY!="SPAIN"),(COUNTRY!="NETHERLAND"))
MARZO2020<-filter(MARZO2020,(COUNTRY!="BRAZIL"),(COUNTRY!="GUATEMALA"))
ggplot(data=MARZO2020)+
  geom_point(mapping=aes (x=FECHA, y=CONTAGIADOS, color=COUNTRY))
  

ggplot(data=MARZO2020)+
  geom_point(mapping=aes (x=FECHA, y=CONTAGIADOS, color=COUNTRY))+
  geom_smooth(mapping=aes (x=FECHA, y=CONTAGIADOS, linetype=COUNTRY)) 
