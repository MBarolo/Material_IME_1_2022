#Integrantes
#Ariel Aarón Argomedo Madrid
#Matías Alejandro Barolo Tobar
#Ramón Alejandro Parra Castillo

#Se cargan las librerías a utilizar
library(dplyr)
library(ggpubr)
library ( bootES )
library ( boot )

#Se cargan los datos a utilizar en un data frame
datos <- read.csv2("./EP11 Datos.csv", stringsAsFactors = FALSE, check.names = FALSE)

set.seed(666)

R <- 333

alfa <- 0.05

#Hipotesis en lenguaje natural
#H0: En promedio, el ingreso total de los hombres y mujeres solteros(as) entre 18 y 35 años es igual.
#Ha: En promedio, el ingreso total de los hombres y mujeres solteros(as) entre 18 y 35 años es distinto.

#Enunciado
#En promedio, el ingreso total de los hombres y mujeres solteros(as) entre 18 y 35 años es igual.

datosHombre <- filter(datos, sexo=="Hombre" & (edad>=18 & edad<=35) & ecivil=="Soltero(a)")$ytotcorh
datosMujer <- filter(datos, sexo=="Mujer" & (edad>=18 & edad<=35) & ecivil=="Soltero(a)")$ytotcorh

#Grafico q-q de cada muestra, que señalan que los datos no siguen una distribucion normal

g1<-ggqqplot(datosHombre)
g2<-ggqqplot(datosMujer)

print(g1)
print(g2)

#Como se puede observar en los graficos qq, los datos no siguen una distribucion cercana a la normal.

# Función para hacer la prueba de permutaciones.

# Argumentos:
# - muestra_1, muestra_2: vectores numÃ©ricos con las muestras a comparar.
# - repeticiones: cantidad de permutaciones a realizar.
# - FUN: función del estadístico E para el que se calcula la diferencia.
# - alternative: tipo de hipótesis alternativa. "two.sided" para
#   hipótesis bilateral, "greater" o "less" para hipótesis unilaterales.
# - plot: si es TRUE, construye el grÃ¡fico de la distribución generada.
# - ...: otros argumentos a ser entregados a graficar_distribucion.

obtiene_permutacion <- function (i , muestra_1 , muestra_2) {
  n_1 <- length ( muestra_1)
  combinada <- c( muestra_1 , muestra_2)
  n <- length ( combinada )
  permutacion <- sample( combinada , n , replace = FALSE )
  nueva_1 <- permutacion [1: n_1]
  nueva_2 <- permutacion [( n_1+1) : n ]
  return ( list ( nueva_1 , nueva_2) )
}

calcular_diferencia <- function ( muestras , FUN ) {
  muestra_1 <- muestras [[1]]
  muestra_2 <- muestras [[2]]
  diferencia <- FUN ( muestra_1) - FUN ( muestra_2)
  return ( diferencia )
}

calcular_valor_p <- function ( distribucion , valor_observado ,
                                   repeticiones , alternative ) {
  if( alternative == "two.sided") {
    numerador <- sum(abs( distribucion ) > abs ( valor_observado ) ) + 1
    denominador <- repeticiones + 1
    valor_p <- numerador / denominador
    }
  else if( alternative == "greater") {
    numerador <- sum( distribucion > valor_observado ) + 1
    denominador <- repeticiones + 1
    valor_p <- numerador / denominador
    }
  else {
    numerador <- sum( distribucion < valor_observado ) + 1
    denominador <- repeticiones + 1
    valor_p <- numerador / denominador
    }
  
return ( valor_p )
}

graficar_distribucion <- function ( distribucion , ...) {
  observaciones <- data.frame ( distribucion )
  
  histograma <- gghistogram ( observaciones , x = "distribucion",
                                 xlab = "Estadístico de interés",
                                 ylab = "Frecuencia", bins = 30 , ...)
  
  qq <- ggqqplot ( observaciones , x = "distribucion", ...)
  
  # Crear una ú nica figura con todos los grá ficos de dispersi ón.
  figura <- ggarrange ( histograma , qq , ncol = 2 , nrow = 1)
  print ( figura )
}

contrastar_hipotesis_permutaciones <- function(muestra_1, muestra_2, repeticiones, FUN, alternative, plot, ...) {
  cat("Prueba de permutaciones\n\n")
  cat("Hipótesis alternativa:", alternative, "\n")
  observado <- calcular_diferencia(list(muestra_1, muestra_2), FUN)
  cat("Valor observado:", observado, "\n")
  
  n_1 <- length(muestra_1)
  
  # Generar permutaciones.
  permutaciones <- lapply(1:repeticiones, obtiene_permutacion, muestra_1,
                          muestra_2)
  
  # Generar la distribución.
  distribucion <- sapply(permutaciones, calcular_diferencia, FUN)
  
  # Graficar la distribución.
  if(plot) {
    graficar_distribucion(distribucion, ...)
  }
  
  # Calcular el valor p.
  valor_p <- calcular_valor_p(distribucion, observado, repeticiones,
                              alternative)
  
  cat("Valor p:", valor_p, "\n\n")
  
  if (valor_p < alfa )
  {
    cat("Se rechaza la hipotesis nula en favor de la hipotesis alternativa. Podemos concluir con un 95% de confianza que existe diferencia entre las medias de los ingresos totales de hombres y mujeres solteros(as) entre 18 y 35.")
  }else 
  {
    cat("Se falla al rechazar la hipotesis nula. Por lo que podemos concluir con un 95% de confianza que no existe diferencia entre las diferencia de los ingresos totales de hombres y muejeres solteros(as) entre 18 y 35.")
  }
}

# Hacer pruebas de permutaciones para la media y la varianza.

contrastar_hipotesis_permutaciones(datosHombre, 
                                   datosMujer, 
                                   repeticiones = R, 
                                   FUN = mean,
                                   alternative = "two.sided", 
                                   plot = TRUE,
                                   color = "blue", 
                                   fill = "blue")

# 2 - Propongan una pregunta de investigación original, que involucre la comparación de las medias de más de
# dos grupos independientes (más abajo se dan unos ejemplos). Fijando una semilla distinta a la anterior,
# seleccionen una muestra aleatoria de hogares (400 < n < 600) y respondan la pregunta propuesta utilizando
# bootstrapping. Solo por ejercicio académico, aplique un análisis post-hoc con bootstrapping aunque este no
# sea necesario.


#Se plantea la siguiente pregunta de investigacion: ¿En promedio las horas trabajadas por tipo de trabajo 
#(Patron o empleador, Trabajo por cuenta propia, entre otras) son iguales?


set.seed(777)

B <- 444

#Se seleccionan los datos a utilizar
datosP2 <- select(datos,o10,o15)%>%filter(!is.na(o10) & !is.na(o15))

distribucion_bootstrapES <- bootES (datosP2, R = B , ci.type = "bca",
                                     ci.conf = 1 - alfa , plot = TRUE )

print ( distribucion_bootstrapES )

#Basarse en el codigo de montecarlo, modificarlo y realizar bootstrapping, usandi anova en cada myestra











