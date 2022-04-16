#Integrantes Grupo 4
# Ariel Aaron Argomedo Madrid
# Matias Alejandro Barolo Tobar
# Ramon Alejandro Parra Castillo

#Respuestas preguntas:

# ¿Qué variables se han cargado?
# Variable: Region
# Variable: fechas en formato dd.mm.yyyy

# ¿Qué tipo tiene cada una de estas variables?
# Tipo variable Region:
# Variable Region: Categorico nominal
# Variable fecha: Numerico discreta
# covid_data <- read.csv2("./EP01 Datos Covid.csv", stringsAsFactors = FALSE)
# tipo <- data.frame(sapply(covid_data,class))

# ¿Qué escala parecen tener estas variables?
# Escala Region: Escala Nominal.
# Escala fecha: Escala de Razón.

library(dplyr)

#Pregunta 1

#Se importa el csv con los datos desde la carpeta actual
covid_data <- read.csv2("./EP01 Datos Covid.csv", stringsAsFactors = FALSE)

#Se filtran las filas que cumplan con la condicion Region=="Coquimbo" para realizar el estudio posterior.
coquimbo_covid <- covid_data %>% filter(Region == "Coquimbo")

#Se seleccionan aquellas columnas que se encuentren dentro del periodo de tiempo 01-05-2020 a 31-10-2020
coquimbo_Acotado <- select(coquimbo_covid,(X01.05.2020:X31.10.2020))

#Se transpone el dataframe coquimbo_Acotado
transpuesta <-data.frame(t(coquimbo_Acotado))

#Se renombra la columna t.coquimbo_Acotado. a Casos para un mejor manejo de esta.
transpuesta <- rename(transpuesta, Casos=t.coquimbo_Acotado.)

#Se transforman los datos de la columna Casos a valores numericos enteros.
numeric_data <- transform(transpuesta, Casos =as.numeric(Casos))

#Se obtiene la fila que contiene el maximo numero de casos.
maximo <- filter(numeric_data, Casos == max(numeric_data$Casos))


#Pregunta 2

#Se obtiene el total de casos en el mes de Mayo
total_coquimbo_mayo <- rowSums(select(coquimbo_Acotado,(X01.05.2020:X31.05.2020)))

#Se obtiene el total de casos en el mes de Junio
total_coquimbo_junio <- as.numeric(rowSums(data.frame(t(data.frame(as.numeric(select(coquimbo_Acotado,(X01.06.2020:X30.06.2020))))))))

#Se obtiene el total de casos en el mes de Julio
total_coquimbo_julio <- rowSums(select(coquimbo_Acotado,(X01.07.2020:X31.07.2020)))

#Se obtiene el total de casos en el mes de Agosto
total_coquimbo_agosto <- rowSums(select(coquimbo_Acotado,(X01.08.2020:X31.08.2020)))

#Se obtiene el total de casos en el mes de Septiembre
total_coquimbo_septiembre <- rowSums(select(coquimbo_Acotado,(X01.09.2020:X30.09.2020)))

#Se obtiene el total de casos en el mes de Octubre
total_coquimbo_octubre <- rowSums(select(coquimbo_Acotado,(X01.10.2020:X31.10.2020)))

#Se crea un dataframe para mostrar la cantidad de casos totales por mes en una sola fila.
casos_totales <- data.frame(t(data.frame(c(total_coquimbo_mayo,total_coquimbo_junio,total_coquimbo_julio,total_coquimbo_agosto,total_coquimbo_septiembre,total_coquimbo_octubre))),row.names="Casos Totales")

#Se renombran las columnas para que se identifiquen con los meses de Mayo a Octubre.
casos_totales <- rename(casos_totales, Mayo=X1,Junio=X2,Julio=X3,Agosto=X4,Septiembre=X5,Octubre=X6)

#Se imprimen los resultados obtenidos para las preguntas 1 y 2.
cat("Respuesta pregunta 1")

print(maximo)

cat("\n")

cat("Respuesta pregunta 2")

print(casos_totales)

cat("\n")

