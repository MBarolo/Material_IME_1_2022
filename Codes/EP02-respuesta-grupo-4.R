library(dplyr)
library(ggpubr)

#Se lee el csv y se almacena como dataframe en la variable casen
casen <- read.csv2("./EP02 Datos Casen 2017.csv",stringsAsFactors = FALSE)

#Se filtra el dataframe y se dejan solo aquellas filas que en la columna sexo tengan el valor "Hombre"
casen <- filter(casen,sexo=="Hombre")

#Crear tabla de contingencia con las variables categoricas sexo, ch1 y zona
tabla_casen <- data.frame(table(casen[["sexo"]],casen[["ch1"]],casen[["zona"]]))
tabla_casen <- rename(tabla_casen,Sexo=Var1,Ocupacion=Var2,Zona=Var3)

#Crear grá fico de barras agrupadas para comparar la situacion ocupacional de los hombres por zona
barrasAgrupadas <- ggplot (tabla_casen , aes ( fill = Zona , y = Freq , x = Ocupacion ) )
barrasAgrupadas <-barrasAgrupadas + geom_bar ( position = "dodge", stat = "identity")
barrasAgrupadas <-barrasAgrupadas + labs ( y = "Frecuencia") + ggtitle ("Distribucion de la Situacion Ocupacional")
barrasAgrupadas <-barrasAgrupadas + theme_pubr ()

#Se imprime el grafico
print(barrasAgrupadas)