# Integrantes

#Ariel Aaron Argomedo Madrid
#Matias Alejandro Barolo Tobar
#Ramon Alejandro Parra Castillo

#Se importan librerias
library(dplyr)
library(ggpubr)

#Se setea la semilla
set.seed(592)

#Se lee el archivo csv
datos <- read.csv2("./EP07 Datos.csv")

alfa <- 0.05

# 1- Estudios científicos han descubierto que la inteligencia musical está altamente relacionada con la inteligencia
# matemática. Pensando en mejorar la oferta de actividades culturales y recreativas, una Universidad ha
# examinado la preferencia de talleres de un grupo de 8 estudiantes de carreras científicas y 11 de carreras
# humanistas, encontrando que 6 de los primeros y 5 de los segundos participaron de talleres musicales. ¿Existe
# relación entre el tipo de carrera que cursan los estudiantes y su participación en talleres musicales?

#Hipotesis nula: Las variables carrera y taller musical son independientes.
#Hipotesis alternativa: Las variables carrera y taller se encuentran relacionadas.

#8 de carreras cientifica: 6->musica
#11 de carreras humanistas: 5->musica

Cientifico <- c(6,2)
Humanista <- c(5,6)

dataframe_1 <- data.frame(Cientifico,Humanista)

tabla_1 <- as.table(as.matrix(dataframe_1))

print(fisher.test(tabla_1,1-alfa))
cat("------------------------- 1 --------------------------\n")
# Dado que p > alfa podemos decir que no existe evidencia suficiente para rechazar la hipotesis nula
# por lo tanto, no podemos asegurar que las variables estén relacionadas.

# 2- Siempre tenaz en su lucha para erradicar a los vampiros de la faz de la tierra, Van Helsing desea probar una
# vacuna que, según él, causará una grave enfermedad en estos seres una vez que beban la sangre de sus
# víctimas. Para ello, ha almacenado una gran cantidad de dosis de su propia sangre, separadas en dos grupos:
# uno de ellos contiene el químico de la vacuna, mientras el otro está completamente limpio. Adicionalmente,
# Van Helsing cuenta con 13 vampiros cautivos, a los que alimentó con sangre limpia por una semana. Luego de
# un periodo de limpieza (durante el cual los vampiros fueron alimentados con su dieta normal, por lo que
# eliminaron todo rastro de la sangre de Van Helsing), repitió el experimento con la sangre que contiene la
# vacuna. Para ambos casos, registró cuántos vampiros enfermaron, con los siguientes resultados:
# * 2 vampiros no presentaron enfermedad alguna con ninguna de las dietas de Van Helsing.
# * 3 vampiros enfermaron tras ambas dietas de Van Helsing.
# * 1 vampiro enfermó con la sangre limpia de Van Helsing, pero no con la sangre que contiene la vacuna.
# * 9 vampiros enfermaron con la sangre que contiene la vacuna, pero no con la sangre limpia de Van Helsing.
# ¿Es posible decir que la vacuna de Van Helsing causa una enfermedad en los vampiros?

#Hipotesis nula: No hay cambios significativos en los vampiros
#Hipotesis alternativa: Si hay cambios significativos en los vampiros

#Aplicar prueba de mcnemmar

Enfermo <- c(3,9)
No_Enfermo <- c(1,2)

dataEstudioVacunas <- data.frame(Enfermo,No_Enfermo)

rownames(dataEstudioVacunas) <- c("Enfermo","No_Enfermo")

tablaEstudioVacunas <- as.table(as.matrix(dataEstudioVacunas))

print(mcnemar.test(tablaEstudioVacunas))
cat("------------------------- 2 --------------------------\n")

#En conclusion, ya que el valor p es mayor al nivel de significancia, se falla al rechazar la hipotesis nula. 

# 3 - El 21 de marzo de 2022 se realizó un estudio acerca de la aprobación al presidente Gabriel Boric en una
#comunidad universitaria, obteniéndose los resultados que se muestran en la tabla. ¿Existe relación entre el
#estamento de la comunidad y la aprobación del presidente?

#Hipotesis nula: Las variables comunidad y aprobacion son independientes.
#Hipotesis alternativa: Las variables comunidad y aprobacion se encuentran relaciones.

aprueba <- c(96,103,21)
desaprueba <- c(119,132,34)

tabla_3 <- as.table ( rbind ( aprueba,desaprueba))

dimnames (tabla_3) <- list ( Opinion = c("Aprueba","Desaprueba") ,
                             Estamento = c("Estudiante","Profesor","Funcionario") )

# Hacer prueba chi - cuadrado de independencia.
prueba_3 <- chisq.test (tabla_3)

cat ("\nLa prueba internamente calcula los valores esperados :\n")
print(esperados_3 <- round (prueba_3[["expected"]] , 3))

#Ya que la prueba nos entrega los valores esperados, y todos son mayor a 5 podemos hacer uso de la prueba para 
#concluir sobre las hipotesis.

prueba_3 <- chisq.test (tabla_3)
print(prueba_3)
cat("------------------------- 3 --------------------------\n")
#En conclusion, como el valor entregado por la prueba es mayor al nivel de significancia entonces, se falla al rechazar
#la hipotesis nula.


# 4 - La Facultad de Ingeniería desea saber si existe diferencia significativa en el desempeño de los estudiantes en
#asignaturas críticas de primer semestre. Para ello, le ha entregado un archivo de datos que, para 3 asignaturas,
#indica si una muestra de 50 estudiantes aprobó o reprobó. ¿Qué puede concluir la Facultad? Indicación:
#obtenga la muestra a partir del archivo EP07 Datos.csv, usando la semilla 592. Considere un nivel de
#significación alfa = 0,05.

#Hipotesis nula: Existe la proporcion de aprobados y reprobados en Calculo, Algebra y Fisica.
#Hipotesis alternativa: La proporcion de aprobados y reprobados es distinta en cada asignatura.

muestra <- datos[sample(nrow(datos), size = 50,replace=FALSE),]

# calculo <- table(muestra["Calculo"])
# algebra <- table(muestra["Algebra"])
# fisica <- table(muestra["Fisica"])

Aprobados <- c(23,17,31)
Reprobados <- c(27,33,19)

muestra <- as.table(rbind(Aprobados,Reprobados))

dimnames (muestra) <- list(Estado = c("Aprueba","Reprueba") ,
                           Asignatura = c("Calculo","Algebra","Fisica"))

prueba_4 <- chisq.test(muestra)

print(esperados <- round (prueba_4[["expected"]] , 3))

#Como podemos observar en la tabla de frecuencias esperadas, se verifica la segunda condicion para aplicar el test
#por lo tanto podemos continuar y concluir sobre las hipotesis a partir de el.

print(prueba_4)
cat("------------------------- 4 --------------------------\n")
#En conclusion, se rechaza la hipotesis nula en favor de la hipotesis alternativa, por lo que no podemos asegurar que las 3
#asignaturas tienen la misma proporcion de aprobados y reprobados.




