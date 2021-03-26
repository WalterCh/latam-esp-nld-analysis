datos<-read.csv("covid_analysis.csv")
datos$FECHA<-as.character(datos$FECHA)
library(dplyr)

colnames(datos)[1]<-"COUNTRY" #lo he corregido porque se cargaba mal
unique(datos$COUNTRY)

#--------------SPAIN------COMPARACION VISUAL----------------
DatosSpain<-filter(datos, COUNTRY=="SPAIN") #FILTRAR DATOS SPAIN

plot(DatosSpain$DIA, DatosSpain$IP_mov_avg, main= "SPAIN") # PINTAR TODOS IP AVER
plot(DatosSpain$DIA[10:35], DatosSpain$IP_mov_avg[10:35], main= "SPAIN_WAVE1") # PINTAR OLA1

#--------------NETHERLAND------COMPARACION VISUAL----------------
DatosNetherland<-filter(datos, COUNTRY=="NETHERLAND") #FILTRAR DATOS NEDERLAND

plot(DatosNetherland$DIA, DatosNetherland$IP_mov_avg, main= "NETHERLAND") # PINTAR TODOS IP AVER
plot(DatosNetherland$DIA[15:40], DatosNetherland$IP_mov_avg[15:40], main= "NETHERLAND_WAVE1") # PINTAR OLA1
 

#--------------CHILE------COMPARACION VISUAL----------------

DatosChile<-filter(datos, COUNTRY=="CHILE")#FILTRAR DATOS CHILE

plot(DatosChile$DIA, DatosChile$IP_mov_avg, main= "CHILE") # PINTAR TODOS IP AVER
plot(DatosChile$DIA[75:100], DatosChile$IP_mov_avg[75:100], main= "CHILE_WAVE1") # PINTAR OLA1

#--------------PERU------COMPARACION VISUAL----------------
DatosPeru<-filter(datos, COUNTRY=="PERU") #FILTRAR DATOS PERU

plot(DatosPeru$DIA, DatosPeru$IP_mov_avg, main= "IP-PERU") # PINTAR TODOS IP AVER
plot(DatosPeru$DIA[50:75], DatosPeru$IP_mov_avg[50:75], main= "PERU_WAVE1") # PINTAR OLA1

#--------------COLOMBIA------COMPARACION VISUAL----------------
DatosColombia<-filter(datos, COUNTRY=="COLOMBIA") #FILTRAR DATOS COLOMBIA

plot(DatosColombia$DIA, DatosColombia$IP_mov_avg, main= "IP-COLOMBIA") # PINTAR TODOS IP AVER
plot(DatosColombia$DIA[140:165], DatosColombia$IP_mov_avg[140:165], main= "COLOMBIA_WAVE1") # PINTAR OLA1


#----------------AJUSTE DE DATOS y visualizacion

#estudio 1: ver que paises tienen un IP parecido respecto a una variable de escala
# por ejemplo Chile y Espania se parecen con escala de 2.5 y Colombia con escala de 1.5
boxplot(DatosSpain$IP_mov_avg[10:35],DatosChile$IP_mov_avg[75:100]*2.5,DatosColombia$IP_mov_avg[140:165]/1.5)
#tambien se pueden mostrar estos datos:
summary(DatosSpain$IP_mov_avg[10:35])
summary(DatosChile$IP_mov_avg[75:100]*2.5)
summary(DatosColombia$IP_mov_avg[140:165]/1.5)

#Estudio 2: clasificar los paises por VECTOR IP parecido en la primera ola
# aqui hago la comparacion para 5 paises y clasifico con caras de Chernoff

Spain_IP<-DatosSpain$IP_mov_avg[10:35]
Netherland_IP<-DatosNetherland$DIA[15:40]
Colombia_IP<-DatosColombia$IP_mov_avg[140:165]
Chile_IP<-DatosChile$IP_mov_avg[75:100]
Peru_IP<-DatosPeru$IP_mov_avg[50:75]
# para ello tengo que pasar a matriz y usar la funcion faces de TeachingDemos.
# hay otra libreria mejor para faces, con colores
library(TeachingDemos)
paises<-data.frame(Spain_IP, Netherland_IP,Colombia_IP,Chile_IP,Peru_IP)
paises<-as.matrix(paises)
paises<-t(paises)
faces(paises)
#--------------------CONCLUSIONES--------------------------

#CONCLUSION 1. La primera ola es similar en Spain, Chile y Colombia mediante un factor de escala
#CONCLUSION 2. El clasificador por faces situa en la misma clase a Spain y Colombia, ademas
#se visualizan 3 clases: (Spain, Colombia) - (Chile,  Peru) - Netherland sola


