#Se importan las librearias 
library(dplyr)
library(ggpubr)

#Se define la significacion
alfa <- 0.05

#Se crea el set de datos
Especialidad <- c("Pediatria","Obstreticia","Dermatologia","Psiquiatria",
                  "Medicina interna","Oncologia","Neurologia","Anestesiologia","Radiologia")
Mujeres <- c(54,71,35,30,45,44,56,21,17)
Hombres <- c(52,66,41,42,65,62,88,40,35)

data <- data.frame(Especialidad,Mujeres,Hombres)

#----------------------------------------------- 1 ----------------------------------------------------------
#Estudios previos habían determinado que la proporción de autoras en la especialidad de pediatría era de 35%.
cat("-------------------------------------- 1 -------------------------------------------\n\n")
#¿Respaldan estos datos tal estimación?

#¿Que piden?

#¿La proporcion de autoras en la especialidad de pediatria es de 0.35

#¿Como lo hice?

#La probabiladidad de exito la podemos definir como:
p = 0.35
totalPediatria <- filter(data,Especialidad == "Pediatria")$Mujeres+filter(data,Especialidad == "Pediatria")$Hombres
#Se distribuye de forma cercana a la normal?
# 1 - Como los datos fueron escogidos de forma aleatoria y que las especialidades no tienen relacion entre si
#     podemos asumir que los datos de la muestra son independientes entre si.

# 2 - ¿Se cumple la condicion de exito fracaso?
probMayor_1 <- totalPediatria*p
probMayor_2 <- totalPediatria*(1-p)

cat(" Probabilidad exito-fracaso mujeres Anestesiologia: ","np: ",probMayor_1,"  ","n(1-p): ",probMayor_2)
# Como los valores probMayor_1 y probMayor_2 cumplen la condicion de exito fracaso entonces podemos asumir
# que el estimador p sigue una distribucion aproximada a la normal.

#Error estandar
SE <- sqrt((p*(1-p))/totalPediatria)

z <- qnorm ( alfa / 2 , lower.tail = FALSE )

inferior <- p - z*SE
superior <- p + z*SE

cat ("\n Intervalo de confianza = [", inferior*100 , ", ", superior*100 , "]\n", sep = "")
#Resultado
cat("-------------------------------------- 2 -------------------------------------------\n\n")

#Como podemos observar en el intervalo de confianza obtenido la probabilidad de exito si se encuentra contenida
#por lo que, podemos concluir que la proporcion de autoras de especialidad Pediatria es de 0.35 o 35%.

#----------------------------------------------- 2----------------------------------------------------------
#Según estos datos, ¿es igual la proporción de autoras en las áreas de anestesiología y pediatría?

#¿Que piden?

#Debemos probar que la proporcion de autoras en las areas de anestesiologia y pediatria es igual.

#¿Como lo hice?

#En primer lugar, debemos corroborar que cada proporcion por separado siga una distribucion cercana a la normal. Sin embargo,
#solo debemos corroborar para la proporcion de anestesiologia, ya que, la proporcion de pediatria como se vio en
#el ejercicio anterior si sigue una distribucion cercana a la normal.
#Por lo tanto, vamos a corroborar que la proporcion de anestesiolgia se distribuye de forma cercana a la normal.
totalAnestesiologia <- filter(data,Especialidad == "Anestesiologia")$Mujeres+filter(data,Especialidad == "Anestesiologia")$Hombres

#La probabiladidad de exito la podemos definir como:
p_anestesiologia <- filter(data,Especialidad == "Anestesiologia")$Mujeres/totalAnestesiologia
p_pediatria <- filter(data,Especialidad == "Pediatria")$Mujeres/totalAnestesiologia

# 1 - Como los datos fueron escogidos de forma aleatoria y que las especialidades no tienen relacion entre si
#     podemos asumir que los datos de la muestra son independientes entre si.

# 2 - ¿Se cumple la condicion de exito fracaso?
probMayor_3 <- totalAnestesiologia*p_anestesiologia
probMayor_4 <- totalAnestesiologia*(1-p_anestesiologia)

cat(" Probabilidad exito-fracaso mujeres Anestesiologia: ","np: ",probMayor_3,"  ","n(1-p): ",probMayor_4)

#Como podemos observar si se cumple la condicion de exito fracaso para la proporcion de anestesiologia, por lo tanto
#como se cumplen las dos condiciones podemos concluir que la proporcion se distribuye de forma cercana a la normal.

#Estimar diferencia
p_diferencia <- abs(p_pediatria - p_anestesiologia)

error_estandar_diferencia <- sqrt((p_pediatria*(1-p_pediatria))/totalPediatria + 
                                  (p_anestesiologia*(1-p_anestesiologia))/totalAnestesiologia)

z_diferencia <- qnorm ( alfa / 2 , lower.tail = FALSE )

inferior_diferencia <- p_diferencia - z_diferencia*error_estandar_diferencia
superior_diferencia <- p_diferencia + z_diferencia*error_estandar_diferencia

cat ("\n Intervalo de confianza = [", inferior_diferencia*100 , ", ", superior_diferencia*100 , "]\n", sep = "")
cat("-------------------------------------- 3 -------------------------------------------\n")

#Resultado

#Podemos concluir con un 95% de confianza que la diferencia de las proporciones de mujeres en pediatria y anestesiologia
#varia dentro del intervalo de confianza calculado anteriormente.

#----------------------------------------------- 3 ----------------------------------------------------------
#Suponiendo que la diferencia en la proporción de autoras en la especialidad de medicina interna y la de
#dermatología es de 0,15. ¿A cuántos autores deberíamos monitorear para obtener un intervalo de confianza
#del 97,5% y poder estadístico de 80%, si se intenta mantener aproximadamente la misma proporción de gente
#estudiada en cada caso?

totalMedicinaInterna <- filter(data,Especialidad == "Medicina interna")$Mujeres+filter(data,Especialidad == "Medicina interna")$Hombres
totalDermatologia <- filter(data,Especialidad == "Dermatologia")$Mujeres+filter(data,Especialidad == "Dermatologia")$Hombres

totalGlobal <- totalMedicinaInterna+totalDermatologia

p_5 <- (filter(data,Especialidad == "Medicina interna")$Mujeres+filter(data,Especialidad == "Medicina interna")$Hombres)/totalGlobal
p_6 <- (filter(data,Especialidad == "Dermatologia")$Mujeres+filter(data,Especialidad == "Medicina interna")$Hombres)/totalGlobal
#¿Que piden?

#¿A cuantos autores se deberian monitorear para obtener un intervalo de confianza del 97,5% y poder de 0.8, si se intenta 
#mantener aproximadamente la misma proporcion de gente estudiada en cada caso.

#¿Como lo hice?

#Se calcula el tamaño de la muestra que deberiamos monitorear utilizando el poder estadistico para la prueba de proporcion
n <- round(power.prop.test(n=NULL,p1 = p_5,p2 = p_6, sig.level = 0.025, power = 0.8, alternative="two.sided")$n,0)

#Resultado
cat ("\n Número de autores que deberian ser monitoreados: ",n)
#En conclusion podemos decir que el numero de autores que deberiamos monitorear para obtener un intervalo de confianza del 97,5%
#y poder estadistico de 80% si se intenta mantener aproximadamente la misma proporcion de gente estudiada en cada caso es igual a 1616.
