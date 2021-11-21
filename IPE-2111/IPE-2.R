v = (1:10^4)


for(i in 1:10^4){
  z=rnorm(50, mean=-4, sd=4)
  z2 <- mean(z)
  v[i] <- z2
  
}


# Presentamos en un histograma los valores obtenidos
h <- hist(v, breaks = 100, density = 10,
          ylab = "Densidad", xlab = "x", main = "Promedio estandarizados y distribución normal estándar") 
xfit <- seq(min(v), max(v), length = 40) 
yfit <- dnorm(xfit, mean = mean(v), sd =sd(v)) 
yfit <- yfit * diff(h$mids[1:2]) * length(v) 
lines(xfit, yfit, col = "orange", lwd = 2)
# Por último agregamos una leyenda para visualizar claramente los resultados  
# y cambiamos la escala del eje
axis(1, at=s,las=1)
legend("topleft", 
       legend = c("Promedio estandarizado",
                  "Distribución normal estándar"),
       lty = 1, col = c("grey","orange"), lwd =1, box.lty =1)


# Sobre el histograma graficamos la densidad de la distribución normal estándar
lines(s, 
      dnorm(s, mean = -4, sd = 16),
      lty = 1, 
      lwd = 2)

polygon(s, 
        dnorm(s, mean = -4, sd = 16), 
        col = rgb(1, 0, 0, alpha = 0.5))
# Por último agregamos una leyenda para visualizar claramente los resultados  
# y cambiamos la escala del eje
axis(1, at=s,las=1)
legend("topleft", 
       legend = c("Promedio estandarizado",
                  "Distribución normal estándar"),
       lty = 1, col = c("grey","red"), lwd =1, box.lty =1)
#Parte c)
#10^4 intervalos de confianza con confianza 95%
n <- 50    # El tamaño válido de la muestra
media <- 32 # la media 
desv <- 6  # La desviación estándar. Datos históricos
nivelconfianza = 0.90

zsum.test(mean.x=32,sigma.x=6, n.x=50,conf.level=0.90)

# EJ 2
# A)
alfa = 0.01
sigma = 16
mu0 = -4
sqrt_n = sqrt(50)
z = pnorm(alfa/2)
no_rc = 0
for(i in v){
  rc = abs(i - mu0) > ((sigma * z * (alfa / 2)) / sqrt_n)
  if(rc) {
    no_rc = no_rc + 1
  }
}
# Propoción de rechazo
no_rc_prop = (no_rc / 10^4) 

# B)
mu0 = -4
x = rnorm(50, mean=-4, sd=sqrt(16))
x_techo = mean(x)
prop = 0
for(Xn in v){
  add_to_prop = abs(Xn - mu0) > abs(x_techo - mu0)
  if(add_to_prop){
    prop = prop + 1
  }
}

#X = numero de exitos de la muestra
prop.test(no_rc_prop,n=50,p=0.9, alternative='two.sided', conf.level = 0.95, correct= FALSE)


#C)
no = (pnorm(abs(x_techo - mu0)*sqrt(50))/16)*2

no

qnorm(1-0.025)

