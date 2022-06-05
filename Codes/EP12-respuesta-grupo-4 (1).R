#Integrantes
#Ariel Aarón Argomedo Madrid
#Matías Alejandro Barolo Tobar
#Ramón Alejandro Parra Castillo

#Se cargan las librerías a utilizar
library(tidyverse)
library(ggplot2)
library(ggpubr)
library ( rcompanion )
library(WRS2)

#Se cargan los datos a utilizar en un data frame
datosEP11 <- read.csv2("./EP11 Datos.csv", stringsAsFactors = FALSE, check.names = FALSE)

#Hipótesis en lenguaje natural
#H0: En promedio los algoritmos son igual de rápidos.
#Ha: En promedio los algoritmos no son igual de rápidos.

#Hipótesis en lenguaje matemático
#H0: u1-u2 = 0
#Ha: u1-u2 != 0

#1.- En el trabajo de título de un estudiante del DIINF se reportan los siguientes tiempos de ejecución (en
#milisegundos) medidos para dos versiones de un algoritmo genético para resolver instancias del problema
#del vendedor viajero disponibles en repositorios públicos. ¿Es uno de los algoritmos más rápido que el otro?

instancia_a <- c(129,109,28,178,74,16,87,108,149,78)
instancia_b <- c(134,193,10,88,142,86,36,190,163,33)
tiempo_a <- c(1510394,402929,885722,4428151,48667,834565,70599,783108,210041,37449)
tiempo_b <- c(1252837,2196277,120276,4629726,5743260,6701654,6568968,180141,6684497,35974)

datos <- data.frame(instancia_a,tiempo_a,instancia_b,tiempo_b)
datosCombinados <- c(tiempo_a,tiempo_b)
#Como se puede observar los datos para los tiempos del algoritmo A
#se distribuyen con una asimetría negativa.
g1 <- gghistogram ( datos , x = "tiempo_a", bins = 10,
                    xlab = "Tiempo", ylab = "Frecuencia",
                    color = " blue ", fill = " blue ")

plot(g1)

#Como se puede observar los datos para los tiempos del algoritmo B
#se distribuyen de forma simétrica.
g2 <- gghistogram ( datos , x = "tiempo_b", bins = 10 ,
                    xlab = "Tiempo", ylab = "Frecuencia",
                    color = "yellow", fill = "yellow")

plot(g2)

#Dado que estamos haciendo una transformación a los datos de los tiempos del algoritmo A, debemos transformar también
#los datos de los tiempos del algoritmo B, dado que, para el primer algoritmo A se cambia la escala.

#Transformacion de los datos del algoritmo A utilizando Escalera de Potencias de Tukey.

datosTransformados<- transformTukey (datosCombinados, start = -4 , end = 4 ,
                                     int = 0.0001 , returnLambda = FALSE)

datosTransformados_a <- datosTransformados[1:10]
datosTransformados_b <- datosTransformados[11:20]

datosTransformadosCombinados <- data.frame(datosTransformados_a,datosTransformados_b)

#Una vez se ha realizado la transformación a los datos, se procede a realizar la prueba t de student para dos
#muestras pareadas. Se considera t de student la más adecuada, ya que ambas muestras tienen un tamaño menor a 30.
#En primer lugar, debemos verificar las condiciones:

# Condiciones
# 1- Las observaciones son independientes entre sí, ya que son instancias distintas de la medición del problema.
# 2- Dado que anteriormente se realizó una transformación a los datos para que siguieran una distribución aproximada a la normal
# se verifica la segunda condición.

# Procedimiento: Se realiza la prueba t de student para dos muestras pareadas, se considera la más adecuada ya que en ambas
# muestras el tamaño es menor a 30.
prueba <- t.test(x=datosTransformados_a,y=datosTransformados_b,paired=TRUE,alternative="two.sided", mu=0,sig.level=alfa)
print(prueba)

#Dado que, el valor p de la prueba t de student para dos muestras pareadas es mayor a alfa, entonces podemos concluir que
#se falla al rechazar la hipótesis nula, esto quiere decir que con un 95% de confiabilidad no existe diferencia entre los tiempos
#de los algoritmos.

# 2- Analice la primera pregunta abordada en el ejercicio práctico 11, con los mismos datos, utilizando un
# método robusto adecuado.

B <- 1000
set.seed(666)

#Pregunta de investigación EP11: En promedio, el ingreso total de los hombres y mujeres solteros(as) entre 18 y 35 años es igual.

#Hipotesis en lenguaje natural
#H0: En promedio, el ingreso total de los hombres y mujeres solteros(as) entre 18 y 35 años es igual.
#Ha: En promedio, el ingreso total de los hombres y mujeres solteros(as) entre 18 y 35 años es distinto.

#En teoría, sabemos que se debería aplicar una prueba t de student para dos muestras independientes,
#sin embargo, como los datos son problemáticos y no siguen una distribución normal, se escoge la alternativa no paramétrica.

datosHombre <- filter(datosEP11, sexo=="Hombre" & (edad>=18 & edad<=35) & ecivil=="Soltero(a)")$ytotcorh
datosMujer <- filter(datosEP11, sexo=="Mujer" & (edad>=18 & edad<=35) & ecivil=="Soltero(a)")$ytotcorh

#Comprobacion de normalidad
ggqqplot(datosHombre)
ggqqplot(datosMujer)

#De acuerdo a lo observado en los graficos qq si quisiéramos aplicar la prueba de Yuen, tendríamos que podar en un punto
#muy cercano a la mediana y dado que la cantidad de datos atípicos es enorme, estaríamos eliminando gran parte de los datos.
#Por lo que, utilizaremos como alternativa no paramétrica la Prueab de Yuen con bootstrapping usando la mediana como medida de
#tendencia central.

ingresos <- c(datosHombre,datosMujer)
sexo <- c(rep("Hombre", length (datosHombre) ) , rep("Mujer", length (datosMujer) ) )

datosNormales <- data.frame(ingresos,sexo)

pruebaYuenMediana <-  pb2gen (ingresos ~ sexo, data = datosNormales , est = "median", nboot = B)

print(pruebaYuenMediana)

#Como el p valor igual a 0.002 es menor al nivel de significancia 0.05 se rechaza la hipótesis nula en favor de la
#hipotesis alternativa. Podemos decir con un 95% de confiabilidad que si existe diferencia entre las medianas de los ingresos de hombres y mujeres
#solteros de entre 18 y 35 años.

# 3.- Analice la segunda pregunta abordada en el ejercicio práctico 11, con los mismos datos, utilizando un
#     método robusto adecuado 

#Se plantea la siguiente pregunta de investigación: ¿La mediana del ingreso total del hogar corregido es igual para todos los niveles de educación?

#Hipotesis en lenguaje natural
#H0: La mediana del ingreso total del hogar corregido es igual para todos los niveles de educación
#Ha: La mediana del ingreso total del hogar corregido es distinta para todos los niveles de educación.

set.seed(777)

datosp2 <- select(datosEP11,ytotcorh,educ)%>%filter(!is.na(ytotcorh) & !is.na(educ))

datosp2 <- sample_n(datosp2,400)

grupo_1 <- filter(datosp2, educ == "Sin Educ. Formal")$ytotcorh
grupo_2 <- filter(datosp2, educ == "Básica Incom.")$ytotcorh
grupo_3 <- filter(datosp2, educ == "Básica Compl.")$ytotcorh
grupo_4 <- filter(datosp2, educ == "M. Hum. Incompleta")$ytotcorh
grupo_5 <- filter(datosp2, educ == "M. Téc. Prof. Incompleta")$ytotcorh
grupo_6 <- filter(datosp2, educ == "M. Hum. Completa")$ytotcorh
grupo_7 <- filter(datosp2, educ == "M. Téc Completa")$ytotcorh
grupo_8 <- filter(datosp2, educ == "Técnico Nivel Superior Incompleta")$ytotcorh
grupo_9 <- filter(datosp2, educ == "Técnico Nivel Superior Completo")$ytotcorh
grupo_10 <- filter(datosp2, educ == "Profesional Incompleto")$ytotcorh
grupo_11 <- filter(datosp2, educ == "Postgrado Incompleto")$ytotcorh
grupo_12 <- filter(datosp2, educ == "Profesional Completo")$ytotcorh
grupo_13 <- filter(datosp2, educ == "Postgrado Completo")$ytotcorh
grupo_14 <- filter(datosp2, educ == "NS/NR")$ytotcorh

datosAnovaDependiente <- c(grupo_1,grupo_2,grupo_3,grupo_4,grupo_5,grupo_6,grupo_7,grupo_8,grupo_9,grupo_10,grupo_11,grupo_12,grupo_13,grupo_14)


datosAnovaIndependiente <- c(rep("Sin Educ. Formal", length (grupo_1) ) ,
                             rep("Básica Incom.", length (grupo_2 ) ) ,
                             rep ("Básica Compl.", length (grupo_3) ),
                             rep("M. Hum. Incompleta", length(grupo_4)),
                             rep("M. Téc. Prof. Incompleta" , length(grupo_5)),
                             rep("M. Hum. Completa", length(grupo_6)),
                             rep("M. Téc Completa", length(grupo_7)),
                             rep("Técnico Nivel Superior Incompleta", length(grupo_8)),
                             rep("Técnico Nivel Superior Completo", length(grupo_9)),
                             rep("Profesional Incompleto", length(grupo_10)),
                             rep("Postgrado Incompleto", length(grupo_11)),
                             rep("Profesional Completo", length(grupo_12)),
                             rep("Postgrado Completo", length(grupo_13)),
                             rep("NS/NR", length(grupo_14)))

dataPrueba <- data.frame(datosAnovaIndependiente,datosAnovaDependiente)
dataPrueba[["datosAnovaIndependiente"]] <- factor(dataPrueba[["datosAnovaIndependiente"]])

#Comprobación de normalidad
data <- datosp2$ytotcorh
ggqqplot(data)

#Como se necesita una poda muy cercana a la mediana usaremos la prueba de ANOVA de una vía para múltiples grupos
# independientes, sin embargo, como se logra observar en el gráfico qq de los datos, estos no siguen una distribución normal
# y si se quisiera aplicar una poda se tendría que descartar una gran cantidad de datos, es por esto, que se usa la mediana
# como medida de tendencia central.
#Debido a un problema presentado con la prueba med1way es que se utiliza la prueba Qanova para aplicar la prueba de hipótesis a los datos.
#La prueba Qanova compara las medianas de cada grupo independiente, opcionalmente se utiliza parámetro q que corresponde a los cuartiles que se quiere
#comparar, sin embargo, cuando este valor no es ingresado toma por defecto 0.5 que corresponde a la mediana o al 50% de los datos.

pruebaAnova <- Qanova(datosAnovaDependiente ~ datosAnovaIndependiente, dataPrueba, nboot = 2000)
print(pruebaAnova)

#Conclusion
#Luego de realizar la prueba Qanova para ver si existe diferencia entre la mediana de los diferentes grupos independientes, el valor p obtenido es de 0.012, al
#ser comparado con un nivel de significancia 0.05 podemos concluir de que no existe evidencia suficiente para rechazar la hipótesis nula. Esto quiere decir
#que podemos afirmar con un 95% de confianza que la mediana del ingreso total del hogar corregido es igual para todos los niveles de educación.
