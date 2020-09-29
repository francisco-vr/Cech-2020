#Trabajo de base de datos de la encuesta "Conducta Electoral, Región metropolitana"

# Carga de paquetes y base de datos -------------------------------------------------------

library("haven")
library("dplyr")
library("readxl")
library("tidyverse")

#cargamos base de datos
Cech <- read_excel("Base de trabajo/Conducta Electoral en Chile.xlsx")

#vemos algunos datos para comprobar cómo se está leyendo
names(Cech)
summary(Cech)


# Ajuste, reagrupación y recodificación de variables ----------------------

#Cambio de nombre "Nacionalidad"
Cech <- rename(Cech,"Nacionalidad" = "¿Cual es su nacionalidad?")

#recodificación de edades
Cech$Edad <- as.numeric(Cech$Edad)
class(Cech$Edad)
Cech <- mutate(Cech, Edadrec = car::recode(Cech$Edad, "17:21 = 1; 22:35 = 2;
                                                           36:50 = 3; 50:90 =4; else = NA"))
Cech <- mutate(Cech, Edadrec = recode(Cech$Edadrec,"1" = "Centennials","2" = "Millennials",
                                      "3" = "Gen X", "4" = "Boomers"))

#Recordar: 1 = Centennials; 2 = Millenials; 3 = Gen X; 4 = Baby Boomers
#cambio de nombre de los tramos en la variable "Edad"
table(Cech$Edadrec)

#Cambio nombre "Estado_civil", Unión de columnas y recodificación de nombres de Estado civil
Cech <- rename(Cech,"estado_civil" = "¿Cual es su estado civil?")
Cech$Estado_civil <-paste(Cech$estado_civil, Cech$...7,Cech$...8,Cech$...9)
table(Cech$Estado_civil)
Cech <- mutate(Cech, Estado_civil = recode(Cech$Estado_civil,
                                           "NA Casado/a NA NA" = "casado",
                                           "NA NA NA NA" = "NA",
                                           "NA NA NA Viudo/a" = "Viudo",
                                           "NA NA Separado/a NA" = "Separado",
                                           "Soltero/a NA NA NA" = "Soltero"))
table(Cech$Estado_civil)

#Cambio nombre "Hijos/as", Unión de columnas y recodificación de nombres de "Hijos"
Cech <- rename(Cech, "Hijos/as" = "¿Tiene usted hijos/as?")
Cech$Hijos <-paste(Cech$`Hijos/as`, Cech$...11)
table(Cech$Hijos)
Cech <- mutate(Cech, Hijos = recode(Cech$Hijos, "NA NA" = "NA",
                                    "NA No, no tengo hijos/as" = "No",
                                    "Si, tengo hijos/as NA"  = "Si"))
table(Cech$Hijos)
  
#recodificación "trabajo", Unión de columnas y recodificación de nombres de "Hijos"
Cech <- rename(Cech,"Trabajo" = "¿tiene usted algún tipo de trabajo remunerado? (independiente o dependiente)")
Cech$TrabajoR <- paste(Cech$Trabajo, Cech$...13)
table(Cech$TrabajoR)
Cech <-mutate(Cech, TrabajoR = recode(Cech$TrabajoR, "NA NA" = "NA",
                                      "NA No, no poseo un trabajo remunerado" = "No",
                                      "Si, poseo un trabajo remunerado NA" = "Si"))
table(Cech$TrabajoR)

#Recodificacón "Educación"
Cech <- rename(Cech,"Educación" = "Nivel de educación ")
Cech$niv_educ <-paste(Cech$Educación, Cech$...15, Cech$...16, Cech$...17, Cech$...18, Cech$...19, Cech$...20)
table(Cech$niv_educ)
Cech <-mutate(Cech, niv_educ = recode(Cech$niv_educ,
                                      "Básica incompleta NA NA NA NA NA NA" = "1",
                                      "NA Básica completa NA NA NA NA NA" = "2",
                                      "NA NA Media incompleta NA NA NA NA" = "3",
                                      "NA NA NA Media completa NA NA NA" = "4",
                                      "NA NA NA NA NA NA NA" = "NA",
                                      "NA NA NA NA NA Superior/técnica incompleta NA" = "5",
                                      "NA NA NA NA Superior/técnica completa NA NA" = "6",
                                      "NA NA NA NA NA NA Postgtados (Magister, doctorado, etc)" = "7"))

# Agrupación de nivel eucativo por niveles
Cech$niv_educ<- as.numeric(Cech$niv_educ)
table(Cech$niv_educ)
Cech <- mutate(Cech, NivEduc = car::recode(Cech$niv_educ,"1 = 1; 2:3 = 2; 4:5 = 3; 6 = 4; 7 = 5; else = NA"))
Cech <- mutate(Cech, NivEduc = recode(Cech$NivEduc,"1" = "Sin estudios", "2" = "básica completa",
                                      "3" = "Media Completa", "4" = "Ed superior", "5" = "Postgrado"))
Cech$niv_educ <- NULL
table(Cech$NivEduc) #chequeamos y listo

#recodificación género - los "otros" se colocan como NA'S para facilitar ponderación
Cech$Sexo[Cech$Sexo== "Otro"] <- NA

#cambio nombre unión de columnas y recodificación de nombre voto municipal 2016
Cech <- rename(Cech,"Vot_Mun_2016" = "¿Fue a votar en las elecciones municipales del 2016? (Alcaldes)")
Cech$VotMun2016 <-paste(Cech$Vot_Mun_2016, Cech$...28)
table(Cech$VotMun2016)
Cech <-mutate(Cech, VotMun2016 = recode(Cech$VotMun2016,"NA NA" = "NA",
                                        "NA No, no fui a votar" = "0",
                                        "Si, fui a votar NA" = "1"))
Cech$VotMun2016 <- as.numeric(Cech$VotMun2016)
table(Cech$VotMun2016)

#Cambio nombrem, unión de columnas y recodificación voto presidencial 2017
Cech <- rename(Cech, "Vot_pres_2017" = "¿Fue a votar en las elecciones presidenciales del 2017? (Presidente)")
Cech$VP_2017 <-paste(Cech$Vot_pres_2017, Cech$...30)
table (Cech$VP_2017)
Cech <-mutate(Cech, VP_2017 = recode(Cech$VP_2017, "NA NA" = "NA",
                                     "NA No, no fui a votar" = "0",
                                     "Si, fui a votar NA" = "1"))
Cech$VP_2017 <- as.numeric(Cech$VP_2017)
table(Cech$VP_2017)

#cambio nombre, unión de columnas y recodificación probabilidad de voto plebiscito
Cech <- rename(Cech, "Prob_vot_ple" = "¿Cree usted que ira a votar en el próximo plebiscito?")
Cech$Prob_Plebi <- paste(Cech$Prob_vot_ple, Cech$...32, Cech$...33)
table(Cech$Prob_Plebi)
Cech <-mutate(Cech, Prob_Plebi = recode(Cech$Prob_Plebi, "NA NA NA" = "NA",
                                        "NA NA No, no iré a votar al plebiscito" = "0",
                                        "NA No estoy seguro/a de ir a votar en el plebiscito NA" = "0.5",
                                        "Si, Iré a votar al plebiscito NA NA" = "1"))
Cech$Prob_Plebi <- as.numeric(Cech$Prob_Plebi)
table(Cech$Prob_Plebi)

#cambio nombre, unión de columnas y recodificación probabilidad de voto municipales
Cech <- rename(Cech, "Prob_vot_Mun" = "¿Cree usted que ira a votar en las próximas elecciones municipales? (Alcalde)")
Cech$PV_Mun <- paste(Cech$Prob_vot_Mun, Cech$...35, Cech$...36)
table(Cech$PV_Mun)
Cech <- mutate(Cech, PV_Mun = recode(Cech$PV_Mun, 
                                     "NA NA NA" = "NA",
                                     "NA NA No, no iré a votar en las próximas elecciones municipales" = "0",
                                     "NA No estoy seguro/a de ir a votar en las próximas elecciones municipales NA" = "0.5",
                                     "Si, iré a votar en las próximas elecciones municipales NA NA" = "1"))
Cech$PV_Mun <-as.numeric(Cech$PV_Mun)
table(Cech$PV_Mun)

#cambio nombre, unión de columnas y recodificación probabilidad voto derecha
Cech <- rename(Cech, "vot_derecha" = "Si el candidato(a) tiene buenas ideas, ¿Usted votaría por alguien de Derecha?")
Cech$voto_derecha <-paste(Cech$vot_derecha, Cech$...41, Cech$...42, Cech$...43)
table(Cech$voto_derecha)
Cech <- mutate(Cech, voto_derecha = recode(Cech$voto_derecha, "NA NA NA NA" = "NA", "NA NA NA No estoy seguro/a" = "No se",
                                           "NA NA No, su posición política es importante NA" = "No, su posicion politica es importante",
                                           "NA Si, porque su posición política es importante NA NA" = "Si, porque su posicion politica es importante",
                                           "Si, su posición política no es importante NA NA NA" = "Si, su posicion politica no es importante"))
table(Cech$voto_derecha)#ver si podemos recodificarla de otra forma

#cambio nombre probabilidad voto izquierda
Cech <- rename(Cech, "vot_izq" = "Si el candidato(a) tiene buenas ideas, ¿Usted votaría por alguien de Izquierda?")
Cech$voto_izquierda <-paste(Cech$vot_izq, Cech$...45, Cech$...46, Cech$...47)
table(Cech$voto_izquierda)
Cech <- mutate(Cech, voto_izquierda = recode(Cech$voto_izquierda,
                                             "NA NA NA NA" = "NA",
                                             "NA NA NA No estoy seguro/a" = "No se",
                                             "NA NA No, su posición política es importante NA" = "No, su posicion politica es importante", 
                                             "NA Si, porque su posición política es importante NA NA" = "Si, porque su posicion politica es importante",
                                             "Si, su posición política no es importante NA NA NA" = "Si, su posicion politica no es importante"))
table(Cech$voto_izquierda) 
           
#Cambio de nombre de afectar posibilidad de voto
Cech <- rename(Cech, "afect_vot" = "De las siguientes opciones; ¿Cuales pueden afectar que usted NO vaya a votar?")
Cech$afecta_votar <-paste(Cech$afect_vot, Cech$...49, Cech$...50, Cech$...51, Cech$...52, Cech$...53)
table(Cech$afecta_votar)
Cech <-mutate(Cech, afecta_votar = recode(Cech$afecta_votar,
                                          "NA Si estoy muy cansado, no iré a votar Los/as candidatos/as no me motivan a ir a votar NA NA NA" = "0",
                                          "NA NA NA No creo en las elecciones NA NA" = "0",
                                          "NA NA NA NA Si el clima no es favorable (lluvia o mucho frió), no iré a votar NA" = "0",
                                          "NA NA NA NA NA Ninguna, voy a votar igual" = "1", "NA NA Los/as candidatos/as no me motivan a ir a votar No creo en las elecciones NA NA" = "0",
                                          "NA NA Los/as candidatos/as no me motivan a ir a votar NA Si el clima no es favorable (lluvia o mucho frió), no iré a votar NA" = "0",
                                          "NA NA Los/as candidatos/as no me motivan a ir a votar NA NA Ninguna, voy a votar igual" = "0.5", 
                                          "NA NA Los/as candidatos/as no me motivan a ir a votar NA NA NA" = "0.5",
                                          "NA" = "0",
                                          "Locomoción, me queda lejos mi lugar de votación Si estoy muy cansado, no iré a votar Los/as candidatos/as no me motivan a ir a votar No creo en las elecciones Si el clima no es favorable (lluvia o mucho frió), no iré a votar Ninguna, voy a votar igual" = "0",
                                          "Locomoción, me queda lejos mi lugar de votación Si estoy muy cansado, no iré a votar Los/as candidatos/as no me motivan a ir a votar No creo en las elecciones Si el clima no es favorable (lluvia o mucho frió), no iré a votar NA" = "0",
                                          "Locomoción, me queda lejos mi lugar de votación NA NA No creo en las elecciones NA NA" = "0",
                                          "Locomoción, me queda lejos mi lugar de votación NA Los/as candidatos/as no me motivan a ir a votar NA Si el clima no es favorable (lluvia o mucho frió), no iré a votar NA" = "0",
                                          "Locomoción, me queda lejos mi lugar de votación NA Los/as candidatos/as no me motivan a ir a votar NA NA NA" = "0",
                                          "Locomoción, me queda lejos mi lugar de votación NA NA NA NA Ninguna, voy a votar igual" = "0"))
Cech$afecta_votar<- as.numeric(Cech$afecta_votar)
table(Cech$afecta_votar)
#cech <- mutate(Cech, afecta_voto = recode (Cech$afecta_voto, "NA NA NA NA NA NA" = "NA", "NA NA NA NA NA Ninguna, voy a votar igual" = "Ninguna", "NA Si estoy muy cansado, no iré a votar NA NA NA NA" = "Cansancio", "NA NA NA No creo en las elecciones NA NA" = "No creo en las elecciones", "NA NA NA NA Si el clima no es favorable (lluvia o mucho frió), no iré a votar NA" = "Clima", ""
#Está la soberana cagada con esta pregunta porque parece que no es de una sola elección. revisar en excel si es necesario

#Cambio nombre probabilidad de voto
Cech <- rename(Cech, "sect_vot" = "¿Por cual sector político usted votaría probablemente?")
Cech$voto_ideol <-paste(Cech$sect_vot, Cech$...55, Cech$...56, Cech$...57, Cech$...58, Cech$...59)
table(Cech$voto_ideol)
Cech <-mutate(Cech,
              voto_ideol = recode(Cech$voto_ideol,
                                  "Derecha NA NA NA NA NA" = "Derecha",
                                  "NA Centro Derecha NA NA NA NA" = "Centro Derecha",
                                  "NA NA Centro NA NA NA" = "Centro",
                                  "NA NA NA Centro Izquierda NA NA" = "Centro Izquierda",
                                  "NA NA NA NA Izquierda NA" = "Izquierda",
                                  "NA NA NA NA NA NA" = "NA",
                                  "NA NA NA NA NA Ninguno" = "Ninguno"))
table(Cech$voto_ideol)

#cambio nombre de sector por el cual nunca votará
Cech <- rename(Cech, "nunca_vot" = "¿Por cual sector político usted nunca votaría?")
Cech$Nunca_vot <-paste(Cech$nunca_vot,Cech$...61,Cech$...62,Cech$...63,Cech$...64,Cech$...65)
table(Cech$Nunca_vot)
Cech <-mutate(Cech,
              Nunca_vot = recode(Cech$Nunca_vot,
                                 "Derecha NA NA NA NA NA" = "Derecha",
                                 "NA Centro Derecha NA NA NA NA" = "Centro Derecha",
                                 "NA NA Centro NA NA NA" = "Centro",
                                 "NA NA NA Centro Izquierda NA NA" = "Centro izquierda",
                                 "NA NA NA NA Izquierda NA" = "Izquierda",
                                 "NA NA NA NA NA NA" = "NA",
                                 "NA NA NA NA NA Ninguno" = "Ninguno"))
table(CECh_semi$Nunca_vot)

#cambio nombre probabilidad de votar por gente del mismo sexo
Cech <- rename(Cech, "vot_same_sex" = "¿Votaría usted por una persona de su mismo sexo (hombre o mujer) a pesar de ser de un sector político diferente al suyo?")
Cech$Voto_mismo_sexo <-paste(Cech$vot_same_sex, Cech$...67, Cech$...68)
table(Cech$Voto_mismo_sexo)
Cech <-mutate(Cech,
              Voto_mismo_sexo = recode(Cech$Voto_mismo_sexo,
                                       "NA NA NA" = "NA",
                                       "NA NA No estoy seguro/a" = "No sé",
                                       "NA No, no votaría por una persona de mi mismo sexo de un sector político diferente al mio. NA" = "No",
                                       "Si, si votaría por una persona de mi mismo sexo a pesar de ser de un sector político diferente al mio. NA NA" = "Si"))
table(Cech$Voto_mismo_sexo)

#cambio nombre probabilidad de votar por alguien homosexual
Cech <- rename(Cech,
               "homosex_vot" = "¿Votaría usted por un/a candidato/a abiertamente homosexual?")
Cech$voto_homosex <-paste(Cech$homosex_vot, Cech$...70)
table(Cech$voto_homosex)
Cech <-mutate(Cech,
              voto_homosex = recode(Cech$voto_homosex,
                                    "NA NA" = "NA",
                                    "NA No, no votaría por un/a candidato/a homosexual" = "No",
                                    "Si, no me es importante su sexualidad NA" = "Si"))
table(Cech$voto_homosex)

#Cambio nombre importancia pasado
Cech <- rename(Cech, "importancia_pasado" = "¿Es importante conocer el pasado personal y político de un candidato?")
Cech$import_pas <-paste(Cech$importancia_pasado, Cech$...72, Cech$...73, Cech$...74)
table(Cech$import_pas)
Cech <-mutate(Cech,
              import_pas = recode(Cech$import_pas,
                                  "NA NA NA NA" = "NA",
                                  "NA NA NA No es importante" = "No es importante",
                                  "NA NA No, no es tan importante, importante es lo que puede hacer NA" = "No es tan importnte",
                                  "NA Si, es importante, pero solo en el ámbito político NA NA" = "Si, en lo político",
                                  "Si, es importante, para conocer sus valores personales NA NA NA" = "Si, por valores"))
table(Cech$import_pas)

#cambio nombre influencia familia
Cech <- rename(Cech, "infl_familia" = "¿Cree usted que el/la candidato/a tenga familia influye en su voto?")
Cech$influencia_familia <-paste(Cech$infl_familia, Cech$...76, Cech$...77)
table(Cech$influencia_familia)
Cech <-mutate(Cech,
              influencia_familia = recode(Cech$influencia_familia,
                                          "NA NA NA" = "NA",
                                          "NA NA No estoy seguro/a" = "No estoy seguro",
                                          "NA No, no es importante que tenga una familia (Pareja, Hijos/as) NA" = "No",
                                          "Si, es importante que tenga una familia (Pareja, hijos/as) NA NA" = "Si"))
table(Cech$influencia_familia)

#cambio nombre influencia de cercanos -> revisar si se elimina
Cech <- rename(Cech,
               "relac_vot" = "¿Cree usted que la relación entre sus familiares, amigos/as, pareja o compañeros/as de trabajo influye en su voto?")
Cech <- rename(Cech, "influencia_cercanos" = "relac_vot")
Cech$infl_cercanos <-paste(Cech$influencia_cercanos, Cech$...79, Cech$...80)
table(Cech$infl_cercanos)
Cech <- mutate(Cech,
               infl_cercanos = recode(Cech$infl_cercanos,
                                      "NA NA NA" = "NA",
                                      "NA NA No, sus opiniones no influyen en mi elección" = "No, no influyen",
                                      "NA No, en su mayoría voto de forma distinta a ellos/as NA" = "No, voto distinto",
                                      "Si, en su mayoría voto de forma similar a ellos/as NA NA" = "Si, voto similar"))
table(Cech$infl_cercanos)

#cambio nombre influencia entorno
Cech <- rename(Cech, "infl_entorn" = "¿Cree usted que en el entorno donde usted reside puede influir en su voto?")
Cech$infl_entorno <-paste(Cech$infl_entorn, Cech$...82)
table(Cech$infl_entorno)
Cech <-mutate(Cech,infl_entorno = recode(Cech$infl_entorno, "NA NA" = "NA",
                                         "NA No, el lugar donde vivo no influye en mi voto" = "No",
                                         "Si, el lugar donde vivo puede influir en mi voto NA" = "Si"))
table(Cech$infl_entorno)

#cambio nombre información de candidatos
Cech <- rename(Cech, "info_candit" = "¿Por donde normalmente se informa sobre los candidatos/as a elección?")
Cech$InfoCandidato <-paste(Cech$info_candit, Cech$...84, Cech$...85, Cech$...86, Cech$...87)
table(Cech$InfoCandidato)
Cech <-mutate(Cech,
              InfoCandidato = recode(Cech$InfoCandidato,
                                           "NA NA Diarios (en formato físico) NA NA" = "Diarios",
                                           "NA NA NA Internet (diarios digitales, Facebook, Whatsapp, Instagram, etc) NA" = "Internet",
                                     "NA NA NA NA Familiares, amigos/as, pareja o compañeros/as de trabajo" = "Cercanos",
                                     "NA NA NA NA NA" = "NA",
                                     "NA Radio NA NA NA" = "Radio",
                                     "Televisión NA NA NA NA" = "Television"))
table(Cech$InfoCandidato)

#cambio nombre consumo de redes sociales
Cech <-rename(Cech,
              "redes_soc" = "Ademas de Facebook, ¿cual de estas plataformas utiliza para distraerse, informarse o divertirse en Internet?")
Cech$uso_RRSS <-paste(Cech$redes_soc, Cech$...89, Cech$...90, Cech$...91, Cech$...92, Cech$...93)
table(Cech$uso_RRSS)
Cech <-mutate(Cech,
              uso_RRSS = recode(Cech$uso_RRSS,
                                "NA Instagram NA NA NA NA" = "Instagram",
                                "NA NA NA NA NA NA" = "NA", 
                                "NA NA NA NA NA Otro" = "Otro",
                                "NA NA NA NA Solo uso Facebook NA" = "Facebook",
                                "NA NA NA Tik Tok NA NA" = "Tik Tok",
                                "NA NA Whatsapp NA NA NA" = "Whatsapp",
                                "twitter NA NA NA NA NA" = "Twitter"))
table(Cech$uso_RRSS)
   

#[CAMBIADAS VÍA EXCEL]

#posición ideológica
#Cech <-rename(Cech,"Pos_ideo" = "En las opciones del 1 al 5, siendo 1 de Izquierda y 5 de Derecha ¿Donde se posicionaría políticamente usted?")
Cech$PosIdeo <-paste(Cech$Pos_ideo, Cech$...22, Cech$...23, Cech$...24, Cech$...25, Cech$...26)
table(Cech$PosIdeo)
Cech <-mutate(Cech,
              PosIdeo = recode(Cech$PosIdeo,
                               "1.- De Izquierda NA NA NA NA NA" = "1",
                               "NA 2.- De Centro Izquierda NA NA NA NA" = "2",
                               "NA NA 3.- De Centro NA NA NA" = "3",
                               "NA NA NA 4.- De Centro Derecha NA NA" = "4",
                               "NA NA NA NA 5.- De Derecha NA" = "5",
                               "NA NA NA NA NA 0.- De Ninguno/a" = "0",
                               "NA NA NA NA NA NA" = "NA"))
table(Cech$PosIdeo)

#Cambio nombre importancia voto
Cech$valor_voto <-paste(Cech$Importancia_voto, Cech$...38, Cech$...39)
table(Cech$valor_voto)
Cech <-mutate(Cech,
              valor_voto = recode(Cech$valor_voto,
                                  "NA NA NA" = "NA",
                                  "NA NA No estoy seguro/a" = "0.5",
                                  "NA No, no es importante NA" = "0",
                                  "Si, es muy importante NA NA" = "1"))
Cech$valor_voto <-as.numeric(Cech$valor_voto)
table(Cech$valor_voto)

#Luego de estar casi lista (aún falta recodificar sexo y si afecta_voto)
names(Cech)
CECh_semi <-select(Cech, Nacionalidad, Sexo, Edad, Comuna,Edadrec:NivEduc, PosIdeo,VotMun2016:import_pas,
                   influencia_familia:valor_voto)

#Reordenar columnas

CECh_semi <- select(CECh_semi,
                    Nacionalidad:PosIdeo, voto_derecha, voto_izquierda,voto_ideol:uso_RRSS,
                    VotMun2016:PV_Mun, afecta_votar,valor_voto)

#Creación de nueva variable "Probabilidad de voto"

ProbVoto <- rowSums(CECh_semi[23:28], na.rm = T)
CECh_semi <-data.frame(CECh_semi, ProbVoto)

#Creació de ponderador por Nivel Educaciona
CECh_semi<- mutate(CECh_semi, EduPonde = recode(CECh_semi$NivEduc, "Sin estudios" = "3.7", "básica completa" = "1.5",
                                             "Media Completa" = "0.96", "Ed superior" = "0.81", "Postgrado" = "0.44"))
class(CECh_semi$EduPonde)
CECh_semi$EduPonde <-as.numeric(CECh_semi$EduPonde)
table(CECh_semi$EduPond)

#Creación de Ponderador por Generación

CECh_semi <- mutate(CECh_semi, GenPonde = recode(CECh_semi$Edadrec,"Centennials" = "1.72","Millenials" = "0.97",
                                                 "Gen X" = "0.85", "Boomers" = "1.04"))
CECh_semi$GenPonde <- as.numeric(CECh_semi$GenPonde)
class(CECh_semi$GenPonde)

#Creación de Ponderador por Género
CECh_semi <- mutate(CECh_semi, SexPonde = recode(CECh_semi$Sexo, "Masculino" = "1.14", "Femenino" = "0.90"))
CECh_semi$SexPonde <-as.numeric(CECh_semi$SexPonde)
class(CECh_semi$SexPonde)

#promedio de todos los ponderadores
CECh_semi$Pond <-round(rowMeans(CECh_semi[30:32]),2)

#Generación de grupos según probablidad de voto: Voto probable, voto poco probable y voto Nulo

CECh_semi$ProbVoto <- as.numeric(CECh_semi$ProbVoto)
class(CECh_semi$ProbVoto)
CECh_semi <- mutate(CECh_semi, VotoProb = car::recode(CECh_semi$ProbVoto, "0:3 = 1; 3.5:4 = 2;
                                                           4.5:6 = 3"))
CECh_semi <- mutate(CECh_semi, VotoProb = recode(CECh_semi$VotoProb, "1" = "No Vota",
                                                      "2" = "Votante improbable","3" = "Votante Probable"))
table(CECh_semi$VotoProb)
save(CECh_semi, file = "Conducta_electoral.RData")

# division de bases por probabilidad de voto ------------------------------

CECH_filtrada <-filter(CECh_semi, VotoProb == "Votante Probable" | VotoProb == "Votante improbable")
CECH_filtrada <-save(CECH_filtrada, file = "Prob_voto_medio_y_alto.RData")


## Tareas por hacer ##

#Revisar las respuestas sobre qué cosas afecta la posibilidad de votar porque tienen la cagá
#Ponderar la base de datos.


