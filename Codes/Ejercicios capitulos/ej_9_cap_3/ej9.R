#Definimos nuestros posibles resultados, exito y fracaso
res <- 1:0
#Definimos prob. de exito
exito <- 0.013
prob <- c(exito,1-exito)

#Definimos la variable aleatoria y calculamos los datos
X<- RV(outcomes = res, probs = prob)
de <- SD(X)
e <- E(X)
cat("Media (v. esperado): ")
print(e)
cat("\n")
cat("D. estandar: ")
print(de)
cat("\n")