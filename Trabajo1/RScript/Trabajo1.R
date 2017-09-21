# 6. AR(1) Binomial Negativo. Es un tipo de proceso autoregresivo que tiene distribuciones
# marginales distribu´ıas Binomial Negativa. Est´a definido en AlOsh and Aly (1992). 
# Es conveniente repasar la definicion de distribuci´on Binomial Negativa (de acuerdo con 
# la parametrizaci ´on que utiliza R).


# 1. Escriba la formula del modelo y con base en esta, escriba un programa en R que
# simule una trayectoria de longitud dada. Escoger valores de los par´ametros de
# acuerdo a la definici ´on del proceso. Reporte un grafico la serie. Muestra reversion
# en lamedia?, la varianza constante?, Repetir con otro conjunto de parametros. Como
# varıa el proceso?.



# 2. Estimar la autocovarianza (fac). Sobresalen algunas autocorrelaciones de las bandas
# de Bartlett (en azul)?. Por el contrario, es ruido blanco?. Estimar a fac del cuadrado
# para el caso en el cual la serie aparente ser un ruido blanco. Si se sale de las bandas
# entonces es ruido blanco tipo ARCH o GARCH.

# 3. Estimar la densidad espectral. Conclusiones posibles: ruido blanco (densidad constante).
# Unas frecuencias bajas dominantes visibles (caen en el intervalo de confianza,
# situado en azul a la derecha de la grafica), puede significar una dinamica auto regresiva.

# 4. Estimar lamedia y la varianza en unamuestra creciente. Si las graficas se estabilizan
# alrededor de un valor, puede ser evidencia de estacionariedad. Por el contrario, si no
# se estabilizan, es porque el proceso no es estacionario en covarianza. Use el codigo
# R siguiente

par(mfrow=c(2,1))
#media
acumulada
m <cumsum(
  Xn)/seq_along(Xn)
ts.plot(m)
#desviacion estandar acumulada
r = (Xn mean(Xn))ˆ2
t = seq(1,n)rep(1,n)
m.sd <sqrt(cumsum(r)/t)
ts.plot(m.sd)

# 5. Es Gaussiano?. Si el proceso es gaussiano las parejas (Xn,Xn−j), j = 1, 2, 3 deben
# mostrar una grafica con forma de elipsoide. No habrıa normalidad bivariada si
# aparece por ejemplo, un hueco en el centro de la “nube”, o se forma una recta
# o rectas. Los datos extremos que se salen de la “nube” tambien contradicen la
# normalidad. Use esta instrucci on.

require(tsDyn)
lag.plot(x, lags=3, layout=c(1,3))




k1 = kernel("daniell", 5)  # a long moving average
k2 = kernel("modified.daniell", 5)  # and a short one
k3 = kernel("daniell", 3)
k4 = kernel("modified.daniell", c(9,9))

#--------parametros del modelo
Xn <- NA
v = 100 
p = 0.6
alpha = 0.8

#--------simular el modelo
# N(x) ~ Bin(x,alpha*p) 
# Wj ~ iid Geo(alpha/(alpha+1))
# Xn = suma(Wj,j=1,...,N(Xn-1))
# Zn ~ iid BN(alpha/(alpha+1),v)
# v > 0, 0 < p < 1, x > 0 

n = 2500 # Cuantos Datos?
set.seed(123) #Fija la semilla: produce

Xn[1]=300
Xn[1]= rnbinom(1,v,(alpha*(1-p))/(1+alpha*(1-p)))

for(j in 2:n){
  
  Zn = rnbinom(1,v,alpha/(alpha+1))
  
  Wj = rgeom(rbinom(1,Xn[j-1],alpha*p),alpha/(alpha+1))
  
  Xn[j] = sum(Wj) + Zn
  
}




par(mfrow=c(2,2))
ts.plot(Xn,type="l")
rhok = acf(Xn,90,ci.type="ma")$acf
Vk = (1-rhok)/(1-rhok[2])
plot(Vk)
plot(density(Xn))
#Xn Tiene Reversion en la media que es equivalente a Estacionaria
#ACF o FACT se ven 6 resagos significativos
#Vk El variograma tiene a una constante
#La densidad ...


par(mfrow=c(2,2))
acf(Xn, main="Returns")
acf(Xn^2, main="Returns^2")
acf(abs(Xn), main="abs(Returns)")

#--------periodograma
# Muestra una tendencia parecida a un autoregresivo, pero aca es mas 
#picudo, dando la idea que no es un valor finito en cero a diferencia de las
#anteriores grafica.
par(mfrow=c(2,1))
spec.pgram(Xn, k1, ci = 0.8)
spec.pgram(Xn, k3, ci = 0.8)


#--------media acumulada
# es utilizar la funcion cumsum (Suma Acumulada) se divide por seq_along(xn) lo 
#que hace es generar un consecutivo que es lo mismo qeu colocar seq(n,length(xn))
#Osea que se esta dividiendo por J osea que se esta dividiendo la media cada ves
#por un valor mas grande, si la media es constante se debe de aproximar a una
#Constante
#Si no vemos esto hay una evidencia que el proceso no es estacionario, si Xn es estacionario
# la media es constante, si es constante las medias acumuladas debe de irse aprox
# a una constante. puede suceder que en los anteriores analisis se evidencia que es estacionario
# pero nen este no.
#Xn es estacionario en covarianza implica la media es constante, si la media es constante las medias 
#progresivas deben de tender a una media constante.

par(mfrow=c(1,1))

m <- cumsum(Xn)/seq_along(Xn) 
ts.plot(m)

#--------varianza con ventana m?vil

# ejemplo ewma de fTrading

install.packages("fTrading")

r = (Xn - mean(Xn))^2
t = seq(1,n)

lambda = c(0.5,0.2,0.05,0.025)
par(mfrow=c(2,2))
for(j in 1:4){
  sigma2 = emaTA(r, lambda = lambda[j], startup = 30)
  sigma = sqrt(sigma2)
  
  plot(t,sigma, xaxt="n", panel.first = grid()
       ,type='l',ylab='trm')
}

# La fluctacion cambia porque muevo la ventana, sin embargo se mantiene oscilando
# en la misma información

#--------chequeo Gaussiano
# Para chequear que sea Gausiano se grafican parejas. 
# (Xn,Xn-1) (Xn,Xn-2) (Xn,Xn-3) 
#Son normales bivariadas, y las graficas de una normal bicariana es similar a 
#una elipse, y se deberia de ver asi en las 3 parejas, cuando no es normal no 
#aparece ese patron, 

#-----------explorar si es Gaussiano
#-----------las graficas no deben mostrar "huecos" (nido, paredes,...)
#-----------debe mostrar una "nube"

require(tsDyn)
lag.plot(Xn, lags=3, layout=c(1,3))

#Lags 3 es las parejas Layout es que lo pongan en una fila las 3 graficas.
install.packages("tsDyn")
require(tsDyn)
#--------otras ventanas m?vil varianza
install.packages("TTR")
require(TTR)
par(mfrow=c(1,1))
sigma.m = runVar(Xn,n=30)
plot(t,sigma.m,type='l')

require(zoo)
roll.sd = rollapplyr(Xn, 30, FUN=sd) # Ventana movil con 30 datos.
roll.sd = rollapplyr(Xn, 90, FUN=sd) # Ventana movil con 30 datos.
plot(roll.sd,type='l')



