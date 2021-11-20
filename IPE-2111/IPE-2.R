library(BSDA)
v = (1:10^4)
for(i in 1:10^4){
  z=rnorm(1, mean=-4, sd=sqrt(16)/50)
  v[i] <- z
  
}
# Presentamos en un histograma los valores obtenidos en el loop
s = seq(0, 5, 0.1)
hist(v,
     main="Promedio estandarizados y distribución normal estándar",
     breaks = 20,
     xaxt="n",
     ylab="Densidad",
     xlab="x",
     freq=FALSE)
# Sobre el histograma graficamos la densidad de la distribución normal estándar
lines(s, 
      dnorm(s, mean = -4, sd = sqrt(16)/50),
      lty = 1, 
      lwd = 2)

polygon(s, 
        dnorm(s, mean = -4, sd = sqrt(16)/50), 
        col = rgb(1, 0, 0, alpha = 0.5))
# Por último agregamos una leyenda para visualizar claramente los resultados  y cambiamos la escala del eje
axis(1, at=s,las=1)
legend("topleft", 
       legend = c("Promedio estandarizado",
                  "Distribución normal estándar"),
       lty = 1, col = c("grey","red"), lwd =1, box.lty =1)
#Falta arreglarr NORMAL ESTANDAR
#Parte c)
#10^4 intervalos de confianza con confianza 95%
#Para armar el intervalo de confianza
n <- 15   # El tamaño válido de la muestra
media <- 5 # la media 
nivelconfianza = 0.95
desv <- sqrt(16)  # utilizo la raíz cuadrada porque me dan como dato la varianza 
#y la fórmula del error estándar utiliza la desvío típico.
qt(0.005,14) # donde 14 son los grados de libertad. GL = n – 1  = 15 - 1 = 14
error.est <- desv/sqrt(n) # Calculamos el error estándar
margen.error <- 2.97 * error.est # nivel de confianza de 99% 
lim.inf <- media - margen.error # Límite inferior del intervalo
lim.inf
lim.sup <- media + margen.error # Límite superior del intervalo
lim.sup
tsum.test(mean.x=5,s.x=sqrt(2),n.x=15,conf.level=0.99)



