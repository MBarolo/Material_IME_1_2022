#Integrantes Grupo 4
# Ariel Aaron Argomedo Madrid
# Matias Alejandro Barolo Tobar
# Ramon Alejandro Parra Castillo

#Importar librerias, se esperan que esten instaladas en el computador de prueba
library ( TeachingDemos )
library ( ggpubr )
library ( ggplot2 )
library ( dplyr )

#Leer datos del archivo csv
data <- read.csv2("./EP04 datos.csv",stringsAsFactors = FALSE)


#ENUNCIADO:

# El Comité Olímpico de una gran potencia del atletismo está estudiando el programa de entrenamiento de varones
# para la competencia de 100 metros planos, por lo que ha recopilado datos de diversos atletas:
# ??? Id: identificador único para cada atleta.
# ??? Raza: raza del atleta (categórica: Blanca, Negra, Oriental).
# ??? Previo: mejor tiempo registrado por el atleta antes de ingresar al programa de entrenamiento (numérica, en
#                                                                                                 segundos).
# ??? Posterior: mejor tiempo registrado por el atleta durante los primeros 6 meses del programa de entrenamiento
# (numérica, en segundos).

#Pregunta Grupo 4 - 1
# 1. El Comité Olímpico cree que el mejor tiempo medio de los atletas orientales antes de ingresar al programa de
# entrenamiento es superior a 18,83 segundos. ¿Soportan los datos esta afirmación?


#Obtener set de datos de los atletas orientales
data_oriental <- filter(data,Raza=="Oriental")%>%select(Previo)
data_oriental <- unlist(data_oriental)

################################ HIPOTESIS #####################################
#Para la pregunta se han definido las siguientes hipotesis:                    #
                                                                               #
#Hipotesis nula                                                                #
                                                                               #
#El mejor tiempo medio de los atletas orientales antes de ingresar al programa #
#de entrenamiento es igual a 18,83.                                            #
                                                                               #
#Hipotesis alternativa                                                         #
                                                                               #
#El mejor tiempo medio de los atletas orientales antes de ingresar             #
#al programa de entrenamiento es superior a 18,83                              #
                                                                               #
# Matemáticamente las hipótesis formuladas quedan como:                        #
# H0: µ = 18.83                                                                #
# HA: µ > 18.83                                                                #
                                                                               #
################################################################################


################################ DATOS #########################################
                                                                               #
#Se define el valor nulo                                                       # 
valor_nulo = 18.83                                                             #
                                                                               #
#Tamaño de la muestra                                                          #
n <- length(data_oriental)                                                     #
                                                                               #
#Grados de libertad                                                            #
grados_libertad <- n-1                                                         #
                                                                               #
#Media de la muestra                                                           #
media_orientales <- mean(data_oriental)                                        #
                                                                               #
#Desviacion estandar                                                           #
sd_orientales <- sd(data_oriental)                                             #
                                                                               #
#Error estandar                                                                #
error_orientales <- sd_orientales/sqrt(n)                                      #
                                                                               #
#Definimos el nivel de significacion alfa = 0,01                               #
alfa = 0.01                                                                    #
                                                                               #
################################################################################


#Como el tamaño de la muestra es menor a 30 y no conocemos la varianza poblacional,
#para probar las hipotesis hacemos uso de la prueba T

#En primer lugar, debemos corroborar que se cumplan las 2 condiciones asociadas a este test:

# 1 - Podemos asumir que los valores de la muestra son independientes ya que fueron escogidos 
# de forma aleatoria o azar.

# 2- El segundo punto, el cual corresponde a verificar que los datos sigan una distribucion normal, lo podemos
#realizar utilizando un grafico q-q.
grafico_orientales <- ggqqplot(data_oriental,title = "Grafico Q-Q Raza oriental")
print(grafico_orientales)

#Como muestra en la grafica q-q podemos decir que los datos siguen una distribucion normal, ya que, no se
#encuentran valores atipicos.


#Calcular estimador
t <- (media_orientales-valor_nulo)/error_orientales

#Luego, debemos encontrar el valor p
p <- pt(t, df = grados_libertad,lower.tail = FALSE)

#Encontrar valor t critico
t_critico <- qt(alfa,df=grados_libertad)

#Calcular intervalo de confianza
sup_orientales <- Inf
inf_orientales <- media_orientales + abs(t_critico*error_orientales)
cat("\n-------------------------------------------------------------------------\n")
cat("Intervalo de confianza Orientales\n","[",inf_orientales,",",sup_orientales,"]\n\n")

#Concluir
cat("Valor p: ",p,"\nNivel de significacion: ",alfa,"\n\n")
cat("\n-------------------------------------------------------------------------\n")
#Dado que, p< alfa se rechaza la hipotesis nula en favor de la hipotesis alternativa con un 99% de confianza.


# Datos obtenidos a través de estadístico T y el p valor:
# p-value = 0.009118263
# Intervalo de confianza = [20.14627, Inf[

# Conclusión:

# Como p = 0.009118263 es menor a nuestro ?? = 0.01, se falla al rechazar
# la hipótesis nula, afirmando con un 99% de confianza; El mejor tiempo medio 
# de los atletas orientales antes de ingresar al programa de entrenamiento es superior a 18.83. 

#------------------------------------------------------------------------------------------------------------------------------------------------




#Pregunta Grupo 4 - 2:

#¿Sugieren los datos que la mejor marca de los atletas negros se reduce en 5,82 segundos tras el entrenamiento?

################################## HIPOTESIS ###################################
#Para la pregunta se han definido las siguientes hipotesis:                    #
#Hipotesis Nula                                                                #
                                                                               #
#La mejor marca de los atletas negros reduce a 5,82 (Diferencia de las medias) #
valor_nulo_d0 <- 5.82                                                          #
                                                                               #
#Hipotesis Alternativa                                                         #
                                                                               #
#La mejor marca de los atletas negros es distinta a 5,82                       #
                                                                               #
# Matemáticamente las hipótesis formuladas quedan como:                        #
# H0: µ1 - µ2 = 5.82                                                           #
# HA: µ1 - µ2 != 5.82                                                          #
################################################################################

#Filtrar por atetlas negros
data_negros_previo <- filter(data,Raza=="Negra")%>%select(Previo)
data_negros_previo <- unlist(data_negros_previo)

data_negros_posterior <- filter(data,Raza=="Negra")%>%select(Posterior)
data_negros_posterior <- unlist(data_negros_posterior)

#Para este caso debemos aplicar una prueba T para dos muestras pareadas
#Verificar si se cumplen las condiciones para ambas muestras.

# 1 - Podemos asumir que las muestras se disitribuyen de forma independiente, ya que, corresponde a la medicion de tiempos
# en instantes y condiciones diferentes.

# 2 - Para verificar que ambas muestras se distribuyen de forma normal, hacemos uso del test Shapiro-Wilk
cat("\n-------------------------------------------------------------------------\n")
normalidad_previo <-  shapiro.test(data_negros_previo)
cat("Test Shapiro-Wilk Tiempo previo: ")
print(normalidad_previo)
normalidad_posterior <- shapiro.test(data_negros_posterior)
cat("Test Shapiro-Wilk Tiempo posterior: ")
print(normalidad_posterior)

#Como en ambos casos el valor de p es lo suficientemente alto con respecto 
#al nivel de significación podemos concluir ambas muestra provienen de poblaciones que se distribuyen
#de forma aproximada a la normal. Por lo tanto, se cumple esta segunda condicion.

#Se calcula la media de los datos pareados
media_A = mean(data_negros_previo)
media_B = mean(data_negros_posterior)
diferenciaMedia = media_A - media_B

#Nivel de significación                                                        
alfa_b <- 0.01                                                                 
                                                                               

#Se realiza la prueba t para dos muestras pareadas
t_b <- t.test(data_negros_previo, data_negros_posterior, paired = TRUE, alternative = "two.sided", mu = valor_nulo_d0, conf.level = 1 - alfa_b)

cat("Valores de la prueba:\n")
print(t_b)
cat("Nivel de significacion: ",alfa_b)
cat("\n-------------------------------------------------------------------------\n")

# Datos obtenidos a través de estadístico T y la prueba de Shapiro:
# t = -3.7319
# p-value = 0.001659
# Intervalo de confianza = [2.407212, 5.390887]
# media_A = 13.68
# media_B = 9.78 
# diferencia = 3.89
# p-value(Shapiro-test / data_negros_previo) = 0.897
# p-value(Shapiro-test / data_negros_posterior) = 0.1537

#Conclusión:

#A modo de conclusion, como el valor p = 0.001659 es mayor al nivel de significación podemos concluir que la hipotesis nula es verdadera. Esto es,
#con un 99% de confianza podemos asegurar que la mejor marca de los atletas negros se reduce en 5,82 segundos tras el entrenamiento.

#---------------------------------------------------------------------------------------------------------------------------------------

#Pregunta Grupo 4 - 3
#¿Es posible afirmar que, en promedio, los atletas negros superan a los orientales por menos de 3,64 segundos
#después del entrenamiento?
  
#Para este caso debemos aplicar una prueba T para dos muestras independientes.

#Se plantean las siguientes hipotesis:
#Hipotesis Nula

#Los atletas negros superan a los orientales por 3.64 segundos despues del entrenamiento
valor_nulo_d0_c <- 3.64

#Hipotesis Alternativa

#Los atletas negros superan a los orientales por menos de 3,64 segundos después del entrenamiento

# Matemáticamente las hipótesis formuladas quedan como:                        
# H0: µ1 - µ2 = 5.82                                                           
# HA: µ1 - µ2 < 5.82

#Filtrar por atetlas negros
data_orientales_posterior <- filter(data,Raza=="Oriental")%>%select(Posterior)
data_orientales_posterior <- unlist(data_orientales_posterior)

data_negros_posterior_c <- filter(data,Raza=="Negra")%>%select(Posterior)
data_negros_posterior_c <- unlist(data_negros_posterior_c)

#Media data_orientales_posterior
media_orientales_posterior <- mean(data_orientales_posterior)

#Media data_negros_posterior
media_negros_posterior_c <- mean(data_negros_posterior_c)

#Tamaño data_negros_previo
n_1_c <- length(data_orientales_posterior)

#Tamaño data_negros_posterior
n_2_c <- length(data_negros_posterior_c)

#Grados del libertad
grados_libertad_c <- min(n_1_c,n_2_c)

#Desviacion estandar data_negros_previo
sd_orientales_posterior <- sd(data_orientales_posterior)

#Desviacion estandar data_negros_posterior
sd_negros_posterior_c <- sd(data_negros_posterior_c)

#Error estandar data_negros_previo
error_orientales_posterior <- sd_orientales_posterior/sqrt(n_1_c)

#Error estandar data_negros_previo
error_negros_posterior_c <- sd_negros_posterior_c/sqrt(n_2_c)

#Error estandar
error_estandar_c <- sqrt(error_orientales_posterior^2 + error_negros_posterior_c^2)

#Nivel de significación 
alfa_c <- 0.01

#Verificar si se cumplen las condiciones para ambas muestras.

# 1 - Podemos asumir que las muestras se disitribuyen de forma independiente, ya que, corresponde a la medicion de tiempos
# en instantes y condiciones diferentes y de razas diferentes.

# 2 - Para verificar que ambas muestras se distribuyen de forma normal, hacemos uso del test Shapiro-Wilk
cat("\n-------------------------------------------------------------------------\n")
normalidad_orientales_posterior <-  shapiro.test(data_orientales_posterior)
cat("Test Shapiro-Wilk Tiempo Orientales posterior: ")
print(normalidad_orientales_posterior)
normalidad_negros_posterior_c <- shapiro.test(data_negros_posterior_c)
cat("Test Shapiro-Wilk Tiempo Negros posterior: ")
print(normalidad_negros_posterior_c)

#Como en ambos casos el valor de p es lo suficientemente alto con respecto 
#al nivel de significación podemos concluir ambas muestra provienen de poblaciones que se distribuyen
#de forma aproximada a la normal. Por lo tanto, se cumple esta segunda condicion.
#Calculamos el estadistico

t_c <- (abs(media_orientales_posterior-media_negros_posterior_c)-valor_nulo_d0_c)/(error_estandar_c)

#Grados libertad
grados_libertad_c <- (sd_orientales_posterior^2/n_1_c + sd_negros_posterior_c^2/n_2_c)^2 / ((sd_orientales_posterior)^4 / (n_1_c*(n_1_c-1)) + (sd_negros_posterior_c)^4 / (n_2_c*(n_2_c-1))) 

#Calculamos el valor P
p_c <- pt(t_c, df = grados_libertad_c,lower.tail = FALSE)

cat("Valor p para dos muestras independientes: ",p_c,"\n")
cat("Nivel de significacion: ",alfa_c)
cat("\n-------------------------------------------------------------------------\n")
 
# Datos obtenidos a través de estadístico T y la prueba de Shapiro:
# p-value = 0.4168847
# p-value(Shapiro-test / data_orientales_posterior) = 0.8099
# p-value(Shapiro-test / data_negros_posterior) = 0.1537

#Conclusión:

#A modo de conclusion, como el valor p = 0.4168847 es mayor al nivel de significación podemos concluir que la hipotesis nula es verdadera. Esto es,
#con un 99% de confianza podemos asegurar que los atletas negros superan a los orientales por 3,64 segundos despues del entrenamiento.


