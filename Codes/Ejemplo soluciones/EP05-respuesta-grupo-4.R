#Integrantes:
#Ariel Aaron Argomedo Madrid
#Matias Alejandro Barolo Tobar
#Ramon Alejandro Parra Castillo

#Se cargan las librerias correspondientes
library(ggplot2)
library(dplyr)
library(ggpubr)

############################################################# DESARROLLO ####################################################################

#1 - Si el ingeniero piensa rechazar la hipótesis nula cuando la muestra presente una media menor a 9,5 litros o
#mayor a 10,5 litros, ¿cuál es la probabilidad de que cometa un error de tipo I?

#Datos
sd <- 1
mu <- 10
n <- 100

valor_nulo_1 <- 9.5
grados_libertad <- n-1
error_estandar <- sd/sqrt(n)

#Encontrar p-value P(X<9.5)
t <- (mu-valor_nulo_1)/error_estandar
p <- pt(t, df = grados_libertad,lower.tail = TRUE)

#Calculo de porcentaje
porcentaje <- 100*p
cat("\n---------------------------------- 1 ------------------------------------\n")
cat("Probabilidad de cometer error Tipo I: ",porcentaje,"%\n")

valor_nulo_2 <- 10.5

#Encontrar p-value para P(X>10.5) 
t <- (mu-valor_nulo_2)/error_estandar
p <- pt(t, df = grados_libertad,lower.tail = FALSE)

#Calculo de porcentaje
porcentaje2 <- 100*p

cat("Probabilidad de cometer error Tipo I: ",porcentaje2,"%")
cat("\n-----------------------------------------------------------------------\n")

#-------------------------------------------------------------------

#Se grafican las áreas de rechazo.
x <- seq(-3,3,length = n)*sd+mu
density <- dnorm(x,mu,sd)

lb <- min(x)
ub <- 9.5
x2 <- seq(min(x), ub, length = n)
density2 <- dnorm(x2,mu,sd)

lb2 <- 10.5
ub2 <- max(x)
x3 <- seq(lb2, ub2, length = n)
density3 <- dnorm(x3,mu,sd)

plot(x,density,type="l")
abline(v = mu)
polygon(c(lb, x2, ub), c(0, density2, 0), col = rgb(0, 0, 1, alpha = 0.5))
polygon(c(lb2, x3, ub2), c(0, density3, 0), col = rgb(0, 0, 1, alpha = 0.5))

################################################################################################################################################
#                                                                         Conclusión                                                           #
                                                                                                                                               #
#Como bien se puede apreciar el la grafica obtenida y en el valor de p (p = 0.999988) la probabilidad de cometer un error Tipo I es de 99.99%. #
                                                                                                                                               #
################################################################################################################################################





# 2- Si el verdadero volumen medio de los bidones fuera de 10,3 litros, ¿cuál sería la probabilidad de que el
#    ingeniero, que obviamente no conoce este dato, cometa un error de tipo II? 

#Se generan las curvas de densidad de los datos hipotetico y real.
x_hipotetico <- seq(-3,3,length = n)*sd+mu
density_hipotetico <- dnorm(x_hipotetico,mu,sd)

x_real <- seq(-3,3,length = n)*sd+10.3
density_real <- dnorm(x_real,mu,sd)

#Se grafican las curvas de densidad para los datos hipoteticos y real. Con el motivo de
#conocer el area bajo la curva asociada a la probabilidad de cometer error de Tipo II.
g <- ggplot(data = data.frame(x_hipotetico,density_hipotetico),aes(x_hipotetico))

g <- g + stat_function(
  fun = dnorm,
  args = list(mean=mu,sd=sd),
  colour = "red",size=1
)

g <- g+ylab("")
g <- g+scale_y_continuous(breaks=NULL)
g <- g+scale_x_continuous(name="Grafico",breaks = seq(min(x_hipotetico),max(x_hipotetico),1))
g <- g+theme_pubr()

g <- g+stat_function(
  fun = dnorm,
  args=list(mean=10.3,sd=sd),
  colour = "blue",size=1)

y1 <- dnorm(x_real,mean=10.3,sd=sd)
g <- g+geom_area(data=subset(data.frame(x_real,y1),x_real<9.5),
                 aes(x=x_real,y=y1),
                 colour = "blue",
                 fill="blue",
                 alpha=0.5)

g <- g+geom_area(data=subset(data.frame(x_real,y1),x_real>10.5),
                 aes(x=x_real,y=y1),
                 colour = "blue",
                 fill="blue",
                 alpha=0.5)

#Se imprime el  grafico
print(g)

#Calcular area asociada a la probabilidad de cometer error Tipo II.
area_1 <- pnorm(9.5,mean = 10.3,sd=sd,lower.tail=TRUE)
area_2 <- pnorm(10.5,mean = 10.3,sd=sd,lower.tail=FALSE)
resultado <- 1-(area_1+area_2)

cat("\n---------------------------------- 3 ------------------------------------\n")
cat("\nLa probabilidad de cometer un error Tipo II: ",resultado*100,"% aproximadamente")
cat("\n-----------------------------------------------------------------------\n")

################################################################################################################################################
#                                                                         Conclusión                                                           #
                                                                                                                                               #
#Con ayuda de los graficos de ambas distribuciones para la media antigua y la nueva media, logramos visualizar de mejor manera el area asociada#
#al error de Tipo II, el cual luego de unos calculos nos dio un valor de 36,74%.
                                                                                                                                               #
################################################################################################################################################




# 3 - Como no se conoce el verdadero volumen medio, genere un gráfico del poder estadístico con las condiciones
#     anteriores, pero suponiendo que el verdadero volumen medio podría variar de 9,3 a 10,7 litros

#Se genera una secuencia de datos correspondiente a diferentes tamaño de muestra.
arreglo <- seq(1,100,1)

#Se calcula el poder estadistico para los diferentes tamaños
poder <- power.t.test(n=arreglo,
                      delta=1.4,
                      sd=sd,
                      sig.level = 0.5,
                      type="two.sample",
                      alternative = "two.sided")$power
#Crear un data frame con los tamaños y su poder asociado
datos <- data.frame(arreglo,poder)

# Graficar la curva de poder.
g <- ggplot ( datos , aes (arreglo , poder ) )
g <- g + geom_line ( colour = "red")
g <- g + ylab (" Poder estadístico ")
g <- g + xlab (" Tamaño de la muestra ")
g <- g + theme_pubr ()
g <- g + ggtitle (" Relación entre el poder y el tamaño de la muestra ")

#Se imprime la curva de poder.
print(g)





# 4 - Considerando un volumen medio de 10 litros, ¿cuántos bidones deberían revisarse para conseguir un poder
#     estadístico de 0,8 y un nivel de significación de 0,05?

#Definiendo un tamaño de efecto igual a 0.3 se utiliza la funcion power.t.test para obtener el tamaño de muestra.

tamanho <- power.t.test(n=NULL,
                      delta=0.3,
                      sd=sd,
                      sig.level = 0.05,
                      power=0.8,
                      type="one.sample",
                      alternative = "two.sided")$n
cat("\n---------------------------------- 4 ------------------------------------\n")
cat("\nEl numero de bidones que se deberian revisar para conseguir un poder estadistico de 0,8 y un nivel de significacion de 0,05 es: ",round(tamanho,0)," aproximadamente")
cat("\n-----------------------------------------------------------------------\n")





# 5 - ¿Y si el ingeniero fuese muy exigente y quisiera reducir la probabilidad de cometer un error de tipo I a un 1% solamente?

#Definiendo un tamaño de efecto igual a 0.3 y una significancia de 0.01 se utiliza la funcion power.t.test para obtener el tamaño de muestra.

tamanho <- power.t.test(n=NULL,
                        delta=0.3,
                        sd=sd,
                        sig.level = 0.01,
                        power=0.8,
                        type="one.sample",
                        alternative = "two.sided")$n
cat("\n---------------------------------- 5 ------------------------------------\n")
cat("\nEl numero de bidones que se deberian revisar para conseguir un poder estadistico de 0,8 y un nivel de significacion de 0,01 es: ",round(tamanho,0)," aproximadamente")
cat("\n-----------------------------------------------------------------------")

















