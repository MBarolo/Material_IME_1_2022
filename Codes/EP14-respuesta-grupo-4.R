#Integrantes
#1- Ariel Argomedo Madrid
#2- Matias Barolo Tobar
#3- Ramon Parra Castillo

#Importacion de librerias
library(dplyr)
library(ggpubr)
library(ggplot2)
library(tidyverse)
library (pROC)
library(car)

#Leer Datos
data <- read.csv2('./EP13 Datos.csv')

#Sobrepeso: IMC >= 25.0
#No sobrepeos: IMC < 25.0

#Se crea la columna IMC
data$IMC <- data$Weight / (data$Height/100)^2

#Se crea la columna EN -> Estado Nutricional
data$EN <- ifelse(data$IMC >= 25.0, "Sobrepeso", "No sobrepeso")

#1.- Definir la semilla a utilizar, que corresponde a los últimos cuatro dígitos del 
# RUN (sin considerar el dígito verificador) del integrante de mayor edad del equipo.

#Se define la semilla
set.seed(4975)

#2.- Seleccionar una muestra de 120 mujeres (si la semilla es un número par) o 120 hombres (si la semilla es impar),
# asegurando que la mitad tenga estado nutricional “sobrepeso” y la otra mitad “no sobrepeso”. Dividir
# esta muestra en dos conjuntos: los datos de 80 personas (40 con EN “sobrepeso”) para utilizar en la
# construcción de los modelos y 40 personas (20 con EN “sobrepeso”) para poder evaluarlos.

#Muestra de datos
hombreSobrepeso <- filter(data,Gender==1 & EN=="Sobrepeso")
hombreNoSobrepeso <- filter(data, Gender==1 & EN=="No sobrepeso")

hombreSobrepesoMuestra <- sample_n(hombreSobrepeso,60)
hombreNoSobrepesoMuestra <- sample_n(hombreNoSobrepeso,60)

muestraHombres <- rbind(hombreSobrepesoMuestra,hombreNoSobrepesoMuestra)

#Datos de entrenamiento
divHombreSobrepesoTrain <- filter(muestraHombres, EN=="Sobrepeso")
divHombreNoSobrepesoTrain <- filter(muestraHombres, EN=="No sobrepeso")

n <- 40
muestraSobrepeso <- sample.int(n = nrow(divHombreSobrepesoTrain), size = n , replace = FALSE)
entrenamiento_1 <- divHombreSobrepesoTrain[muestraSobrepeso,]

muestraNoSobrepeso <- sample.int(n = nrow(divHombreNoSobrepesoTrain), size = n , replace = FALSE)
entrenamiento_2 <- divHombreNoSobrepesoTrain[muestraNoSobrepeso,]

entrenamiento <- rbind(entrenamiento_1,entrenamiento_2)

muestraIndex <- rbind(muestraSobrepeso,muestraNoSobrepeso)

#Datos de prueba
prueba_1<- divHombreSobrepesoTrain[-muestraSobrepeso, ]
prueba_2<- divHombreNoSobrepesoTrain[-muestraNoSobrepeso, ]

prueba <- rbind(prueba_1,prueba_2)

divHombreSobrepesoTest <- filter(muestraHombres,EN=="Sobrepeso")
divHombreNoSobrepesoTest <- filter(muestraHombres,EN=="No sobrepeso")

# 3.- Variables predictoras

#predictors <- c(Ankles.diameter,Age,Knees.diameter,Shoulder.Girth,Wrists.diameter,Chest.depth,Bicep.Girth,Knee.Girth)

# 4.- Variable predictora de EN: Waist.Girth -> Corresponde a la circunferencia de la cintura. Como equipo nos parece que es un excelente
# predictor, ya que, una persona con sobrepeso tiende a tener una mayor circunferencia que una persona que no tiene sobrepeso.

#predictors <- c(predictors,"Waist.Girth")

# 5.- Usando el entorno R y paquetes estándares, construir un modelo de regresión logística con el predictor
# seleccionado en el paso anterior y utilizando de la muestra obtenida.

# Se ajusta el modelo con la variable predictora Waist.Girth.

#Se convierte a formato factor la columna EN del dataframe entrenamiento
entrenamiento$EN <- factor(entrenamiento$EN)

#Se crea el modelo.
modelo <- glm(EN~Waist.Girth , family = binomial(link = "logit"), data = entrenamiento )
print (summary(modelo))

# Evaluar el modelo con el conjunto de entrenamiento.

cat ("Evaluación del modelo a partir del conjunto de entrenamiento :\n")
probs_e <- predict(modelo,entrenamiento, type="response")

muestraHombres$EN <- factor(muestraHombres$EN)
umbral <- 0.5
preds_e <- sapply ( probs_e , function ( p ) ifelse ( p >= umbral , "1", "0"))
preds_e <- factor(preds_e)
entrenamiento$EN <- factor(ifelse(entrenamiento$EN == "Sobrepeso","1","0"))

#Se muestra la curva ROC, la cual se aleja bastante de la diagonal, por lo que podemos decir que se trata de un buen modelo.
ROC_e <- roc(entrenamiento[["EN"]], probs_e)
plot(ROC_e)

#Se genera la matriz de confusion del modelo, esta matriz nos entrega una exactitud del 77.5%, una sensibilidad de 90% y 
#una especifidad del 65%. Lo anterior nos indica que el modelo se desempeña mejor identificando elementos de la clase positiva.
matriz_e <- confusionMatrix (preds_e , entrenamiento [["EN"]])
print (matriz_e)

# Evaluar el modelo con el conjunto de prueba .
cat (" Evaluaci ón del modelo a partir del conjunto de prueba :\n")
probs_p <- predict ( modelo , prueba , type = "response")

#Se convierte a formato factor la columna EN de el dataframe de datos de prueba.
prueba$EN <- factor(ifelse(prueba$EN == "Sobrepeso",1,0))

preds_p <- sapply ( probs_p , function ( p ) ifelse ( p >= umbral , "1", "0") )
preds_p <- factor(preds_p)

#Se genera la curva ROC para el modelo evaluado con los datos de prueba. Como podemos ver, la curva se aleja bastante de la diagonal
#por lo que podemos decir que es un buen modelo.
ROC_p <- roc ( prueba [["EN"]] , probs_p )
plot(ROC_p)

matriz_p <- confusionMatrix ( preds_p , prueba[["EN"]])
#Verificación de las condiciones para el uso del modelo.

# 1 - Debe existir una relación lineal entre los predictores y la respuesta transformada.
corRLogS1 <- cor(as.matrix(probs_p),prueba$Waist.Girth)

#Se cumple la condición de relacion lineal. Dado que la correlacion es igual 1.

# 2 - Los residuos deben ser independientes entre sí. -> Se obtiene un valor p igual a 0, menor a un alfa 0.05.
# Por lo tanto, no podemos asegurar que los residuos sean independientes entre sí.

cat (" Verificaci ón de independencia de los residuos \n")
cat (" - - - - - - - - - - -- - - - - - - - - - - - - - - - - - - - -- - - - - -\n")
print( durbinWatsonTest (modelo, max.lag = 1) )

#Al no cumplirse la segunda condición podemos asegurar que el modelo no puede ser generalizado.

#------------------------------------------------------------------------------------------------------------------
# Usando herramientas estándares para la exploración de modelos del entorno R, buscar entre dos y cinco
# predictores de entre las variables seleccionadas al azar, recordadas en el punto 3, para agregar al modelo
# obtenido en el paso 5.

#Ajustar modelo nulo
EN <- factor(entrenamiento$EN)
entrenamiento$EN <- NULL
entrenamiento <- cbind(EN,entrenamiento)

#Los datos de entrenamiento solo contienen las variables predictoras del ejercicio práctico anterior.
entrenamiento <- data.frame(entrenamiento$Ankles.diameter,entrenamiento$Age,entrenamiento$Knees.diameter,
                            entrenamiento$Shoulder.Girth,entrenamiento$Wrists.diameter,entrenamiento$Chest.depth,
                            entrenamiento$Bicep.Girth,entrenamiento$Knee.Girth,entrenamiento$Waist.Girth)

nulo <- glm( EN ~ entrenamiento.Waist.Girth , family = binomial ( link = "logit") , data = entrenamiento )

#Ajustar modelo completo
completo <- glm( EN ~ . , family = binomial ( link = "logit") , data = entrenamiento )

#Ajustar modelo con regresión escalonada
mejor <- step ( nulo , scope = list ( lower = nulo , upper = completo ) ,direction = "both", trace = 0)

print(summary(mejor))

#Se realiza una verificación de la multicolinealidad. Dado que, ninguna variable presenta un VIF superior a 10 y el promedio 
#es ligeramente superior a 1, podemos decir que es poco probable que el modelo presente problemas. En conclusión, no es necesario
#eliminar ninguna de las variables predictoras, ya que, los predictores seleccionados por el ajuste con regresión escalonada del modelo
#son los mejores que pueden predecir la variable IMC.
vifs <- vif ( mejor )
print(vifs)

cat ("\ nPromedio VIF: ")
print ( mean ( vifs ) )

#Se crea un nuevo modelo con las 3 variables que son las mejores predictoras para el Estado Nutricional y generar el nuevo modelo.
modelo_final <- glm(formula = EN ~ entrenamiento.Bicep.Girth + entrenamiento.Knee.Girth + 
                      entrenamiento.Ankles.diameter + entrenamiento.Waist.Girth, family = binomial(link = "logit"), data = entrenamiento)

print(modelo_final)

# Evaluar el modelo con el conjunto de prueba .
cat (" Evaluación del modelo a partir del conjunto de prueba :\n")
colnames(prueba)[which(names(prueba) == "Waist.Girth")] <- "entrenamiento.Waist.Girth"
colnames(prueba)[which(names(prueba) == "Bicep.Girth")] <- "entrenamiento.Bicep.Girth"
colnames(prueba)[which(names(prueba) == "Knee.Girth")] <- "entrenamiento.Knee.Girth"
colnames(prueba)[which(names(prueba) == "Ankles.diameter")] <- "entrenamiento.Ankles.diameter"

probs_final <- predict (modelo_final , prueba , type = "response")

preds_final <- sapply ( probs_final , function ( p ) ifelse ( p >= umbral , "1", "0"))
preds_final <- factor(preds_final)

#Gráfica de la curva ROC para el modelo con 3 variables predictoras
ROC_final <- roc ( prueba [["EN"]] , probs_final )
plot ( ROC_final )

#Matriz de confusión para el modelo con 3 variables predictoras.
matriz_final <- confusionMatrix ( preds_final , prueba [["EN"]])

cat("Matriz de confusion para el modelo con 1 variable predictora:\n")
print(matriz_p)
cat("Matriz de confusion para el modelo con 3 variables predictoras:\n")
print ( matriz_final )

#Verificación de las condiciones para el uso del modelo.

# 1 - Debe existir una relación lineal entre los predictores y la respuesta transformada.

corRLogM1 <- cor(as.matrix(probs_final),prueba$Waist.Girth)
corRLogM2 <- cor(as.matrix(probs_final),prueba$Bicep.Girth)
corRLogM3 <- cor(as.matrix(probs_final),prueba$Knee.Girth)
corRLogM4 <- cor(as.matrix(probs_final),prueba$Ankles.diameter)

#Se cumple la condición de relación lineal. Dado que la correlación es igual 1 en todos los casos.

# 2 - Los residuos deben ser independientes entre sí. -> Se obtiene un valor p igual a 0.004, menor a un alfa 0.05.
# Por lo tanto, no podemos asegurar que los residuos sean independientes entre sí.
cat (" Verificación de independencia de los residuos \n")
cat (" - - - - - - - - - - -- - - - - - - - - - - - - - - - - - - - -- - - - - -\n")
print( durbinWatsonTest (modelo_final, max.lag = 5) )

#Al no cumplirse la segunda condición podemos asegurar que el modelo no puede ser generalizado.

#Se puede comparar el modelo de RLogS con el de RLogM utilizando ANOVA
cat (" Likelihood Ratio Test para los modelos \n")
cat (" - - - - - - - - - - -- - - - - - - - - - - - - - - - - - - - -- - - - - -\n")
print (anova(modelo , modelo_final , test = "LRT"))

#Dado que el valor p entregado por el procedimiento ANOVA es de 0.006 bastante inferior a un alfa de 0.05, es conveniente utilizar el modelo con 
#un solo predictor.

#De los resultados obtenidos, como podemos observar al comparar los valores de Accuracy de ambos modelos, obtenemos que
#el modelo con una variable predictora tiene un Accuracy igual a 77.5% y el modelo con 3 variables predictoras tiene un Accuracy igual a 67.5%.
#De lo anterior, podemos deducir que el primer modelo evaluado es más exacto y, por lo tanto, tiene un poder predictivo mas elevado que el segundo.
#A modo de conclusion, Waist.Girth corresponde a la variable que le otorga mayor predictivo al modelo y, por lo tanto, es capaz de describir de mejor
#manera el Estado Nutricional de los hombres.