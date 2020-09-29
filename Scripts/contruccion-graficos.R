## FASE 2 CECH  - CONSTRUCCIÓN DE GRÁFICOS  ##

library(summarytools)
library(psych)
library(ggplot2)
library(dplyr)
library(tidyverse)

load("~/Documentos/Git/Cech/Prob_voto_medio_y_alto.RData")


# Contrucción de tablas (Opcional) ----------------------------------------
#Exposición de la muestra: quienes son los votantes probables, por edad, género y nivel educaci
ctable(CECh_semi$VotoProb, CECh_semi$NivEduc, report.nas = FALSE, headings = FALSE, prop = "r")
ctable(CECh_semi$VotoProb, CECh_semi$Sexo,report.nas = FALSE, headings = FALSE, prop = "r")
ctable(CECh_semi$VotoProb, CECh_semi$Edadrec, report.nas = FALSE, headings = FALSE, prop = "c")
ctable(CECH_filtrada$VotoProb,CECH_filtrada$uso_RRSS, reṕort.nas = FALSE, headings = FALSE, prop = "c")


#Tablas cruzadas de conducta poítica según votante probable.
ctable(CECh_semi$Edadrec, CECh_semi$uso_RRSS, prop = "t")
ctable(CECh_semi$Edadrec, CECh_semi$VotoProb, prop = "c")
ctable(CECh_semi$VotoProb, CECh_semi$Nunca_vot, prop = "r")

TablaSexo <-table(CECh_semi$Sexo)
round((prop.table(TablaSexo)*100),2)

TablaEdad <-table(CECH_filtrada$Edadrec)
round((prop.table(TablaEdad)*100),2)

TablaNEduc <-table(CECh_semi$NivEduc)
round((prop.table(TablaNEduc)*100),2)
table(CECH_filtrada$NivEduc)


# contrucción de gráficos (Alta probabilidad de voto y probabilidad media-----------------------------

# Género de votantes probables

GrafSex <-ggplot(data = subset(CECH_filtrada, !is.na(Sexo)),
                     aes(x = factor(Sexo),
                         y = prop.table(stat(count)),
                         weight = Pond,
                         fill = factor(VotoProb),
                         label = scales::percent(prop.table(stat(count))))) +
  geom_bar(position = "dodge") + 
  labs(title = "Distribución por género, \n según probabilidad de voto",
       x = "Género", y = "Porcentaje",
       caption = "Fuente: Elaboración propia, basada en Encuesta de Conducta Electoral 2020") +
  xlab("Género") + ylab("Porcentaje") +
  scale_y_continuous(labels=scales::percent) + 
  scale_fill_manual("Probabilidad de ir a votar",
                    values = c("#FF6666", "#00CC66"),
                    labels = c("Probabilidad media", "Probabilidad alta")) +
  geom_text(stat = 'count',
            position = position_dodge(.9), 
            vjust = -0.5, 
            size = 4) + 
  scale_y_continuous(labels = scales::percent) + 
  labs(x = 'Género', fill = 'Probabilidad de Voto') +
  theme(plot.title = element_text(hjust = .5, size = 20, face = "bold"),
        plot.caption = element_text(face = "italic")) +
  facet_wrap(~VotoProb)
plot(GrafSex)

ggsave(GrafSex, filename = "resultados/GrafSex.png",
       dpi = 400, width = 8, height = 7)

# Grupos generacional de los votantes
  
GrafGen <-ggplot(data = subset(CECH_filtrada, !is.na(Edadrec)),
         mapping = aes(x = factor(Edadrec),
                       y = prop.table(stat(count)),
                       fill = factor(VotoProb),
                       label = scales::percent(prop.table(stat(count))))) +
  geom_bar(position = "dodge") + 
  labs(title = "Distribución por género, según probabilidad de voto",
       x = "Género", y = "Porcentaje",
       caption = "Fuente: Elaboración propia, basada en Encuesta de Conducta Electoral 2020") +
  xlab("Grupo generacional") + ylab("Porcentajes") +
  scale_y_continuous(labels=scales::percent) + 
  scale_fill_manual("Probabilidad de ir a votar",
                    values = c("#FF6666", "#00CC66"),
                    labels = c("Probabilidad media", "Probabilidad alta")) +
  geom_text(stat = 'count',
            position = position_dodge(.9), 
            vjust = -0.5, 
            size = 4) + 
  ggtitle("Distribución de grupos generacionales, según probabilidad de voto") + 
  theme(plot.title = element_text(hjust = .5, size = 20, face = "bold"),
        plot.caption = element_text(face = "italic"))
plot(GrafGen)

ggsave(GrafGen, filename = "resultados/GrafGen.png",
       dpi = 400, width = 12, height = 7)

# Uso de RRSS de votantes probables

graficoRRSS <- ggplot(data = subset(CECH_filtrada, !is.na(uso_RRSS)), aes(x = factor(uso_RRSS), 
                          y = prop.table(stat(count)), weight = Pond, 
                          fill = factor(VotoProb), 
                          label = scales::percent(prop.table(stat(count))))) +
  geom_bar(position = "dodge") + 
  labs(title = "Redes sociales usadas por personas con alta y media posibilidade ir a votar",
       x = "Redes sociales usadas", y = "Proporción",
       caption = "Fuente: Elaboración propia, basada en Encuesta de Conducta Electoral 2020") +
  xlab("Redes sociales") + ylab("Porcentaje") +
  scale_y_continuous(labels=scales::percent) + 
  scale_fill_manual("Probabilidad de ir a votar",
                    values = c("#FF6666", "#00CC66"),
                    labels = c("Votante de probabilidad media", "Votante de probabilidad alta")) +
  geom_text(stat = 'count',
            position = position_dodge(.9), 
            vjust = -0.5, 
            size = 4) + 
  scale_y_continuous(labels = scales::percent) + 
  labs(x = 'Redes sociales usadas', fill = 'Probabilidad de Voto') +
  ggtitle("Redes sociales usadas, según probabilidad de ir a votar") + 
  theme(plot.title = element_text(hjust = .5, size = 20, face = "bold"),
        plot.caption = element_text(face = "italic"))
plot(graficoRRSS)

  ggsave(graficoRRSS, filename = "resultados/GrafRRSS.png",
       dpi = 400, width = 12, height = 7)

# Por quien nunca votarían

GrafNoVot <-ggplot(data = subset(CECH_filtrada, !is.na(Nunca_vot)), aes(x = factor(Nunca_vot),
                       y = prop.table(stat(count)),
                       fill = factor(VotoProb),
                       label = scales::percent(prop.table(stat(count))))) +
  geom_bar(position = "dodge") + 
  labs(title = "Redes sociales usadas por personas con alta y media posibilidade ir a votar",
       x = "Redes sociales usadas", y = "Proporción",
       caption = "Fuente: Elaboración propia, basada en Encuesta de Conducta Electoral 2020") +
  xlab("Posiciones políticas") + ylab("Porcentajes") +
  scale_y_continuous(labels=scales::percent) + 
  scale_fill_manual("Probabilidad de ir a votar",
                    values = c("#FF6666", "#00CC66"),
                    labels = c("Probabilidad media", "Probabilidad alta")) +
  geom_text(stat = 'count',
            position = position_dodge(.9), 
            vjust = -0.5, 
            size = 4) + 
  ggtitle("Ideologías por las cuales nunca votarían, según probabilidad de voto") + 
  theme(plot.title = element_text(hjust = .5, size = 20, face = "bold"),
        plot.caption = element_text(face = "italic"))
plot(GrafNoVot)

ggsave(GrafNoVot, filename = "resultados/GrafNovot.png",
       dpi = 400, width = 12, height = 7)

# afecta el pasado de la gente?
GrafPasado <- ggplot(data = subset(CECH_filtrada, !is.na(import_pas)),
                     aes(x = factor(import_pas),
                         y = prop.table(stat(count)),
                         fill = factor(VotoProb),weight = Pond,
                         label = scales::percent(prop.table(stat(count))))) +
  geom_bar(position = "dodge") + 
  labs(title = "¿Importa el pasado del Candidato?, según probabilidad de voto",
       x = "Redes sociales usadas", y = "Proporción",
       caption = "Fuente: Elaboración propia, basada en Encuesta de Conducta Electoral 2020") +
  xlab("Razones de importancia") + ylab("Porcentaje") +
  scale_y_continuous(labels=scales::percent) + 
  scale_fill_manual("Probabilidad de ir a votar",
                    values = c("#FF6666", "#00CC66"),
                    labels = c("Probabilidad media", "Probabilidad alta")) +
  geom_text(stat = 'count',
            position = position_dodge(.9), 
            vjust = -0.5, 
            size = 4) + 
  scale_y_continuous(labels = scales::percent) + 
  labs(x = 'Razones usadas', fill = 'Probabilidad de Voto') +
  ggtitle("¿Importa el pasado del Candidato?, según probabilidad de voto") + 
  theme(plot.title = element_text(hjust = .5, size = 20, face = "bold"),
        plot.caption = element_text(face = "italic"))
plot(GrafPasado)

ggsave(GrafPasado, filename = "resultados/GrafPasado.png",
       dpi = 400, width = 15, height = 7)

# Por donde se informa de los candidatos

GrafInfoCandit <- ggplot(data = subset(CECH_filtrada, !is.na(InfoCandidato)),
                         aes(x = factor(InfoCandidato),
                             y = prop.table(stat(count)),
                             weight = Pond,
                             fill = factor(VotoProb),
                             label = scales::percent(prop.table(stat(count))))) +
  geom_bar(position = "dodge") + 
  labs(title = "Por donde se informan de los Candidatos, según probabilidad de votar",
       x = "Medios de información", y = "Porcentaje",
       caption = "Fuente: Elaboración propia, basada en Encuesta de Conducta Electoral 2020") +
  xlab("Medios donde se informan") + ylab("Porcentaje") +
  scale_y_continuous(labels=scales::percent) + 
  scale_fill_manual("Probabilidad de ir a votar",
                    values = c("#FF6666", "#00CC66"),
                    labels = c("Probabilidad media", "Probabilidad alta")) +
  geom_text(stat = 'count',
            position = position_dodge(.9), 
            vjust = -0.5, 
            size = 4) + 
  scale_y_continuous(labels = scales::percent) + 
  labs(x = 'Medios de información', fill = 'Probabilidad de Voto') +
  theme(plot.title = element_text(hjust = .5, size = 20, face = "bold"),
        plot.caption = element_text(face = "italic"))
plot(GrafInfoCandit)

ggsave(GrafInfoCandit, filename = "resultados/GrafInfoCandidat.png",
       dpi = 400, width = 12, height = 7)

# Posibilidad de votar por alguien de derecha

GrafDere <- ggplot(data = subset(CECH_filtrada, !is.na(voto_derecha)),
                                 aes(x = factor(voto_derecha),
                                     y = prop.table(stat(count)),
                                     weight = Pond,
                                     fill = factor(VotoProb),
                                     label = scales::percent(prop.table(stat(count))))) +
  geom_bar(position = "dodge") + 
  labs(title = "Independiente de su postura política, ¿votaría por alguien de derecha?",
       x = "Razones de su postura", y = "Porcentaje",
       caption = "Fuente: Elaboración propia, basada en Encuesta de Conducta Electoral 2020") +
  xlab("Razones de su postura") + ylab("Porcentaje") +
  scale_y_continuous(labels=scales::percent) + 
  scale_fill_manual("Probabilidad de ir a votar",
                    values = c("#FF6666", "#00CC66"),
                    labels = c("Probabilidad media", "Probabilidad alta")) +
  geom_text(stat = 'count',
            position = position_dodge(.5), 
            vjust = -0.5, 
            size = 4) + 
  scale_y_continuous(labels = scales::percent) + 
  labs(x = 'Razones de su postura', fill = 'Probabilidad de Voto') +
  theme(plot.title = element_text(hjust = .5, size = 20, face = "bold"),
        plot.caption = element_text(face = "italic"))
plot(GrafDere)

  ggsave(GrafDere, filename = "resultados/GrafDere.png",
       dpi = 400, width = 15, height = 7)

# Posibilidad de votar por alguien de izquierda
GrafIzq <- ggplot(data = subset(CECH_filtrada, !is.na(voto_izquierda)),
                  aes(x = factor(voto_izquierda),
                      y = prop.table(stat(count)),
                      weight = Pond,
                      fill = factor(VotoProb),
                      label = scales::percent(prop.table(stat(count))))) +
  geom_bar(position = "dodge") + 
  labs(title = "Independiente de su postura política, ¿votaría por alguien de Izquierda?",
       x = "Razones de su postura", y = "Porcentaje",
       caption = "Fuente: Elaboración propia, basada en Encuesta de Conducta Electoral 2020") +
  xlab("Razones de su postura") + ylab("Porcentaje") +
  scale_y_continuous(labels=scales::percent) + 
  scale_fill_manual("Probabilidad de ir a votar",
                    values = c("#FF6666", "#00CC66"),
                    labels = c("Probabilidad media", "Probabilidad alta")) +
  geom_text(stat = 'count',
            position = position_dodge(.9), 
            vjust = -0.5, 
            size = 4) + 
  scale_y_continuous(labels = scales::percent) + 
  labs(x = 'Razones de su postura', fill = 'Probabilidad de Voto') +
  theme(plot.title = element_text(hjust = .5, size = 20, face = "bold"),
        plot.caption = element_text(face = "italic"))
plot(GrafIzq)

ggsave(GrafIzq, filename = "resultados/GrafIzq.png",
       dpi = 400, width = 15, height = 7)

# Otros comportamiientos electorales - Voto Homosexual

GrafHomosex <-ggplot(data = subset(CECH_filtrada, !is.na(voto_homosex)),
                                   aes(x = factor(voto_homosex),
                                       y = prop.table(stat(count)),
                                       weight = Pond,
                                       fill = factor(VotoProb),
                                       label = scales::percent(prop.table(stat(count))))) +
  geom_bar(position = "dodge") + 
  labs(title = "¿Votarías por alguien abiertamente homosexual? según probabilidad de ir a votar",
       x = "Respuesta", y = "Porcentaje",
       caption = "Fuente: Elaboración propia, basada en Encuesta de Conducta Electoral 2020") +
  xlab("Razones de su postura") + ylab("Porcentaje") +
  scale_y_continuous(labels=scales::percent) + 
  geom_text(stat = 'count',
            position = position_dodge(.9), 
            vjust = -0.5, 
            size = 4) + 
  scale_y_continuous(labels = scales::percent) + 
  labs(x = 'Razones de su postura', fill = 'Probabilidad de Voto') +
  theme(plot.title = element_text(hjust = .5, size = 20, face = "bold"),
        plot.caption = element_text(face = "italic")) +
  facet_wrap(~VotoProb)
plot(GrafHomosex)

ggsave(GrafHomosex, filename = "resultados/GrafHomosex.png",
       dpi = 400, width = 10, height = 7)
