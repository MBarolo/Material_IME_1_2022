# 6. Un estudio clínico reclutó a 32 pacientes con fatiga crónica para determinar si un tratamiento basado
# en inyecciones intramusculares de magnesio es efectivo para esta condición. De los 15 pacientes que
# recibieron estas inyecciones, seleccionados de manera aleatoria, 12 reportaron sentirse mejor (80 %),
# mientras que solo 3 pacientes de los 17 que recibieron inyecciones placebo (18 %) reportaron mejorías.
#    a) ¿Se cumplen las condiciones para aplicar una prueba exacta de Fisher al problema enunciado?
#   b) ¿Cuáles serían las hipótesis nula y alternativa para esta prueba?
#   c) Independientemente de la respuesta anterior, aplica la prueba usando R y luego de forma manual
#     (Ayuda: hay 16 tablas que mantienen los totales marginales en el enunciado).
#   d) ¿A qué conclusión lleva este procedimiento?
#   

#a)
#Condiciones para poder aplicar la prueba f exacta:
  #- Observaciones deben ser independientes
  #- Debe haber 5 observaciones esperadas en cada grupo

#Como los pacientes fueron escogidos de manera aleatoria  podemos decir que las observaciones son independientes entre si.

#Calculamos las observaciones esperadas para los pacientes
#32 pacientes, 15 pacientes inyectados, 15 tratamiento real->12 mejoran, 17 placebo-> 3 mejoran, 
            #a  #b
placebo <- c(14, 3)
          #c #d
real <- c(3, 12)

total_col <- c(17,15)
total_fil <- c(17,15)
total <- 32
esperados <- c()
for(i in 1:2){
  for (j in 1:2){
    esperados <- append(esperados, total_col[i]*total_fil[j]/total)
  }
}
print(esperados)

#Como se puede observar, todos los esperados son mayores a 5, por lo que se satisfacen las condiciones para la prueba f exacta.

#b)
#Primero, definamos cuales serían nuestras variables
#tratamiento: El tipo de tratamiento (Placebo, o tratamiento real)
#mejora: Si el paciente mejora con el tratamiento

#Las hipotesis nula y alternativa serian:
  #H0: Las variables tratamiento y mejora son independientes
  #Ha: Las variables tratamiento y mejora estan relacionadas

#c)
#Definimos un alfa de 0.05
alfa <- 0.05
#Creamos la tabla de contingencia
tabla <- as.table(rbind(placebo, real))
dimnames(tabla) <- list(tratamiento = c("Placebo", "Real"), mejora = c("No", "Si"))
print(tabla)

prueba <- fisher.test(tabla, 1-alfa)
print(prueba)
#Al realizar la prueba obtenemos un p = 0.001033, este valor es bastante menor al 0.05 que tomamos para alfa, por lo cual se rechaza H0 en favor de Ha
#Por lo tanto se concluye con un 95% de confianza que las variables tratamiento y mejora no son independientes (están relacionadas).
