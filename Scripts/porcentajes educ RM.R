## extración de ponderación de base de datos CASEN 2017

#LLamada de base

library(haven)
library(dplyr)
library(summarytools)
casen17 <-as.data.frame(read_spss("/home/debsuri/Descargas/Casen 2017.sav"))
casen17 <-select(casen17, expc, provincia, educ, edad)

save(casen17, file= "/home/debsuri/Descargas/Casen-2017.Rdata")

load("/home/debsuri/Descargas/Casen-2017.Rdata")

#ordenamiento y codificación acorde a la CECH - region y edad
casen17$provincia <- as.numeric(casen17$provincia)
casen17 <-filter(casen17, provincia == 131)
casen17 <-filter(casen17, edad > 17)

#recodificación variable "educ"
casen17 $educ<- as.numeric(casen17$educ)
casen17 <- mutate(casen17, educ = car::recode(casen17$educ,
                                              "0:1 = 1; 2:4 = 2; 5:7 = 3; 9 = 3; 8 = 4; 10:11 = 4;
                                              12 = 5; else = NA"))
casen17 <- mutate(casen17, educ = dplyr::recode(casen17$educ, "1" = "Sin estudios",
                                                "2" = "Básica completa","3" = "Media completa",
                                                "4" = "Educación superior", "5" = "Postgrados"))
tablaeduc <-table(casen17$educ)
prop.table(tablaeduc)*100
round((prop.table(tablaeduc)*100),2)

# recodificación variable "edad"
casen17$edad <- as.numeric(casen17$edad)
class(casen17$edad)
casen17 <- mutate(casen17, Edadrec = car::recode(casen17$edad, "18:21 = 1; 22:35 = 2;
                                                           36:50 = 3; 50:90 =4; else = NA"))
casen17 <- mutate(casen17, Edadrec = recode(casen17$Edadrec,"1" = "Centennials","2" = "Millenials",
                                            "3" = "Gen X", "4" = "Boomers"))

tablaedad <-table(casen17$Edadrec)
round((prop.table(tablaedad)*100),2)


#Qué fala hacer?
#Comprobar que la variable educ es la que neesito
# ver si necesito aplicar algún factor de expansión


## PORCENTAJES PARA LA PONDERACIÓN ##

#Qué fala hacer?
#Comprobar que la variable educ es la que neesito
# ver si necesito aplicar algún factor de expansión



