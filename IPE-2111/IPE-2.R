library(BSDA)
vPromedios = (1:10^4)
vExtSup = (1:10^4)
vExtInf = (1:10^4)

for(i in 1:10^4){
  #generamos la muestra tama駉 50
  z=rnorm(50, mean=-4, sd=4)
  #calculamos el promedio de la muestra anterior 
  promedio <- mean(z)
  #guardo en variables los valores que voy a usar en la qt.
  a = (1-(0.05/2))
  b = (50 -1)
  ErrorE = 4/sqrt(50)
  #calculamos extremos inferiores y superiores de cada muestra
  ExtremoInf <- promedio-(ErrorE*qt(a,b))
  ExtremoSup <- promedio+(ErrorE*qt(a,b))
  #guardamos promedios
  vPromedios[i] <- promedio
  #guardamos extremos de los intervalos de confianza
  vExtSup [i] <- ExtremoSup
  vExtInf [i] <- ExtremoInf
  
}

# Presentamos en un histograma los valores obtenidos
h <- hist(vPromedios, breaks = 100, density = 10,
          ylab = "Densidad", xlab = "x", main = "Promedio estandarizados y distribuci贸n normal est谩ndar") 
xfit <- seq(min(vPromedios), max(vPromedios), length = 40) 
yfit <- dnorm(xfit, mean = mean(vPromedios), sd =sd(vPromedios)) 
yfit <- yfit * diff(h$mids[1:2]) * length(vPromedios) 
lines(xfit, yfit, col = "orange", lwd = 2)
# Por 煤ltimo agregamos una leyenda para visualizar claramente los resultados  
# y cambiamos la escala del eje
axis(1, at=s,las=1)
legend("topleft", 
       legend = c("Promedio estandarizado",
                  "Distribuci贸n normal est谩ndar"),
       lty = 1, col = c("grey","orange"), lwd =1, box.lty =1)


#Parte c) intervalos obtenidos
vExtSup
vExtInf 

#Parte D)

noContieneEsperanza = 0
#recorremos los vectores de extremos de los intervalos de confianza
for(i in 1:10^4){
    if( -4 > vExtSup[i] | -4 < vExtInf[i]){
      #incrementamos el contador cuando el intervalo no contiene a la
      #esperanza teorica
      noContieneEsperanza = noContieneEsperanza +1
  }
}
noContieneEsperanza
#calculamos la proporcion que no posee a la esperanza teorica
proporcion = noContieneEsperanza/10^4
proporcion

# EJ 2
# A)
alfa = 0.01
sigma = 16
sqrt_n = sqrt(50)
z = qnorm(1-(alfa/2))
#calculamos el extremos de la region critica a partir de las variables definidas arriba
extremoRC = (sigma * z/ sqrt_n)
#creamos variable para contar la cantidad de rechazos
rechazo = 0
for(i in 1:10^4){
  #evaluamos si la region critica y contamos los rechazos de H0
  print(abs(vPromedios[i] - (-4)))
  if(abs(vPromedios[i] - (-4)) > extremoRC) {
    rechazo = rechazo + 1
  }
}
# Proporcion de rechazo
propRechazo = (rechazo / 10^4)
propRechazo


# B)
muestra = rnorm(50, mean=-4, sd=sqrt(16))

mu0 = -4
promedioX = mean(muestra)
estadisticoObservado = abs(promedioX - mu0)
cumpleDesigualdad = 0
for(i in 1:10^4){
  if((abs(vPromedios[i] - mu0)) > estadisticoObservado){
    cumpleDesigualdad = cumpleDesigualdad + 1
  }
}
estimacionP_valor = cumpleDesigualdad/10^4
estimacionP_valor


#C)
#iguala el extremo de la regi髇 cr韙ica al valor del estad韘tico y despejamos alfa
#el desarrollo se encuentra explicado en el documento final

valorAbs = abs(promedioX - mu0)
raiz = sqrt(50)
sigma = 16
pValor = (1-(pnorm(((valorAbs*raiz))/sigma)))*2
pValor









#10^4 intervalos de confianza con confianza 95%
me = mean(v) #La media
z = 1.96 #Cuando tenes 95% de confianza
media <- 32 # la media 
desv <- 6  # La desviaci贸n est谩ndar. Datos hist贸ricos
nivelconfianza = 0.90

zsum.test(mean.x=32,sigma.x=6, n.x=50,conf.level=0.90)
#X = numero de exitos de la muestra
prop.test(no_rc_prop,n=50,p=0.9, alternative='two.sided', conf.level = 0.95, correct= FALSE)
