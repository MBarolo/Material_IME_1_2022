#Integrantes
# -Ariel Aaron Argomedo Madrid
# -Matias Alejandro Barolo Tobar
# -Ramon Alejandro Parra Castillo

library(dplyr)
library(ggpubr)
library(purrr)

#Parte 1 - Definir semilla propia

#Codigo extraido del enunciado con la semilla cambiada.
población <- read.csv2("./EP03 Datos Casen 2017.csv")
tamaño <- nrow(población)
ingreso <- as.numeric(población[["ytot"]])
poda <- 0.2
q20 <- quantile(ingreso, poda)
q80 <- quantile(ingreso, 1 - poda)
ingreso.podado <- ingreso[ingreso > q20 & ingreso < q80]
tamaño.podado <- length(ingreso.podado)
media.ingreso <- mean(ingreso.podado)
sd.ingreso <- sqrt (sum((ingreso.podado - media.ingreso)^2) / tamaño.podado )
set.seed(666)
ingreso.normal <- rnorm(5000, mean = media.ingreso, sd = sd.ingreso)

#Parte 2 - Generar distribucion Z
ingreso.estandar <- as.numeric(map(ingreso.normal,function(x) (x-media.ingreso)/sd.ingreso))

#Parte 3 - Construir dos distribuciones chi cuadrado, con entre 3 y 15 grados de libertad.

#Primera distribucion chi cuadrado
ingreso.cuadrado_1 <- c()

#Grados de libertad
lambda_1 <- 13

var <- 0
#Se extraen 5000 muestras de tamaño lambda_1 de forma aleatoria de la distribucion Z
#para generar Chi cuadrado con lambda_1 grados de libertad
while (var < 5000)
{
  indices <- sample(1:5000,lambda_1,replace=FALSE)
  
  acum <- 0
  for (x in indices)
  {
    acum <- acum + ingreso.estandar[x]*ingreso.estandar[x]
  }
  
  ingreso.cuadrado_1 <- append(ingreso.cuadrado_1,acum)
  
  var <- var + 1 
}

#Segunda distribucion chi cuadrado
ingreso.cuadrado_2 <- c()

#Grados de libertad
lambda_2 <- 8

var <- 0
#Se extraen 5000 muestras de tamaño lambda_2 de forma aleatoria de la distribucion Z
#para generar Chi cuadrado con lambda_2 grados de libertad
while (var < 5000)
{
  indices <- sample(1:5000,lambda_2,replace=FALSE)
  
  acum <- 0
  for (x in indices)
  {
    acum <- acum + ingreso.estandar[x]*ingreso.estandar[x]
  }
  
  ingreso.cuadrado_2 <- append(ingreso.cuadrado_2,acum)
  
  var <- var + 1 
}

#Parte 4 - Construir distribucion F
ingreso.f <- (ingreso.cuadrado_1/lambda_1)/(ingreso.cuadrado_2/lambda_2)

#Parte 5 - Graficar

#Se calculan las densidad de cada una de las distribuciones
densidad.normal <- density(ingreso.normal)
densidad.estandar <- density(ingreso.estandar)
densidad.chi_1 <- density(ingreso.cuadrado_1)
densidad.f <- density(ingreso.f)
densidad.chi_2 <- density(ingreso.cuadrado_2)

#Se crean los graficos
par(mfcol = c(2, 3))

plot(densidad.normal,main="Distribucion normal",xlab="Ingreso")

plot(densidad.estandar,main="Distribucion estándar",xlab="Ingreso")

plot(densidad.chi_1,main="Distribucion Chi-cuadrado 1",xlab="Ingreso")

plot(densidad.f,main="Distribucion F",xlab="Ingreso")

plot(densidad.chi_2,main="Distribucion Chi-cuadrado 2",xlab="Ingreso")

par(mfcol = c(1, 1))

#-------------------------------------------------------------------------------

#Parte 1 - Cambiar seed a: 666

set.seed(666)
n.repeticiones <- 30
ensayo <- function(x)
  ifelse(sample(población[["sexo"]], 1) == "Mujer", 1, 0)
treinta.repeticiones <- sapply(1:n.repeticiones, ensayo)

#Probabilidad de exito
p <- 0.5 #Existe la misma probabilidad de escoger a un hombre o a una mujer.

#Parte 2 - Distribucion binomial
distribucion.binomial <- c()

#Se genera la distribucion binomial, se calcula la probabilidad de obtener 0 exitos en n.repeticiones
#1 exito en n.repeticiones, 2 exitos en n.repeticiones, etc.
for (k in 0:n.repeticiones)
{
  prob <- (factorial(n.repeticiones)/(factorial(k)*factorial(n.repeticiones-k)))*(p^k)*((1-p)^(n.repeticiones-k))
  distribucion.binomial <- append(distribucion.binomial,prob)
}

#Parte 3 - Distribucion Geometrica
distribucion.geometrica <- c()
n <- n.repeticiones

#Se genera la distribucion geometrica, se calcula la probabilidad de obtener 1 exito al n-esimo intento.
while (n > 0)
{
  prob <- ((1-p)^(n-1))*p
  distribucion.geometrica <- append(distribucion.geometrica,prob)
  
  n <- n-1
}

#Parte 4 - Distribucion Binomial Negativa

# Corresponde a una "ampliacion" de la Distribucion Geometrica.
# Se calcula la probabilidad de obtener k exitos al n-esimo intento.
distribucion.binomial.negativa <- c()

for (k in 0:n.repeticiones)

{
  prob <- ((factorial(n.repeticiones)-1)/((factorial(k)-1)*factorial(n.repeticiones-1-k+1)))*(p^k)*((1-p)^(n.repeticiones-k))
  distribucion.binomial.negativa <- append(distribucion.binomial.negativa,prob)
}

#Parte 5- Graficar 

#Se calculan las densidad de cada una de las distribuciones
densidad.binomial <- density(distribucion.binomial)
densidad.geometrica <- density(distribucion.geometrica)
densidad.binomial.negativa <- density(distribucion.binomial.negativa)

#Se grafican cada una de las distribuciones.
par(mfcol = c(2, 2))

plot(densidad.binomial,main="Distribucion Binomial")
plot(densidad.geometrica,main="Distribucion Geometrica")
plot(densidad.binomial.negativa,main="Distribucion Binomial Negativa")

par(mfcol = c(1, 1))