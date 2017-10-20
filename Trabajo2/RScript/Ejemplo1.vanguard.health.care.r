# Ejemplo: an?lisis de rendimientos de
# Vanguard Health Care Adm (VGHAX)
# Nasdaq - Nasdaq Delayed Price. Currency in USD


library(forecast)
library(TSA)
library(FitAR)
library(timsac)
library(tseries)
library(astsa)
library(fArma)
library(lmtest)

require(timeSeries)

source("GarchOxFit.r")

source("GarchOxInterface.r")

D = read.csv("/Users/anita/Downloads/Capitulo 7. Procesos GARCH/vanguard.health.care.csv", header = T,stringsAsFactors=FALSE)
attach(D)

y = diff(log(rev(Close)),1,1)
fecha = as.Date(rev(Date))

np = length(y)
t = seq(1,np,1)


ejex.mes = seq(fecha[1],fecha[np], "months")
ejex.a?o = seq(fecha[1],fecha[np],"years")

par(mfrow=c(2,1))
plot(fecha,rev(Close), xaxt="n", panel.first = grid()
,type='l',ylab='VFTSX: nivel',xlab='dia')
axis.Date(1, at=ejex.mes, format="%m/%y")
axis.Date(1, at=ejex.a?o, labels = FALSE, tcl = -0.2)


plot(fecha[-1],y, xaxt="n", panel.first = grid()
,type='l',ylab='VFTSX: rendimientos',xlab='dia')
axis.Date(1, at=ejex.mes, format="%m/%y")
axis.Date(1, at=ejex.a?o, labels = FALSE, tcl = -0.2)

require(TSA)
# Tiene la ventaja que no grafica el resago en 0
par(mfrow=c(2,1))
TSA::acf(y,60,ci.type="ma", drop.lag.0 = TRUE)
#Puede ser un MA(2) ess apenas un diagnostico no es concluyente se toma nota.
pacf(y,60)
# Analisis 0. Xn = Log(pt/pt-1)
# 1. Xn = mu +zn+theta1zn-1+theta2zn-2
# zn zn^2 ar(p)
# arma(max(p,q),q)

#Se cuentan los resagos, aguanta hasta 15? MA(15)

require(forecast)
auto.arima(y)
#Para ser concluyentes pasamos a autoarima para mirar si nos valida que es MA2
#ARIMA(0,0,2)
#------------------estimacion MA

m.ma15 = arima(y,c(0,0,15))

require(lmtest)
coeftest(m.ma15)
# ma15      -0.05036288  0.01642302 -3.0666 0.002165 ** 


m.ma2 = arima(y,c(0,0,2))
coeftest(m.ma2)
# ma2       -0.06158650  0.01659086 -3.7121 0.0002056 ***

theta = coef(m.ma15)

#------------------verificar
r = resid(m.ma15) 

# r =Zn estimado

TSA::acf(r,60,ci.type="ma", drop.lag.0 = TRUE)
pacf(r,60)

#-------------pruebas Ljung-Box
Box.test(r, lag = 15 , type =  "Ljung-Box")
Box.test(r, lag = 30 , type =  "Ljung-Box")
#  p-value = 0.9996 seguimos trabajando con 15

#---------------analisis GARCH

par(mfrow=c(2,1))

TSA::acf(r^2,60,ci.type="ma", drop.lag.0 = TRUE)
pacf(r^2,60)

TSA::acf(r,60,ci.type="ma", drop.lag.0 = TRUE)


#---------------prueba Engle de efectos ARCH
# Para chequear la heteroheasticidad
#------------https://www.mathworks.com/help/econ/engles-arch-test.html
#The test statistic for Engle?s ARCH test is 
#the usual F statistic for the regression 
#on the squared residuals. 

#Under the null hypothesis, the F statistic 
#follows a ?2 distribution with m degrees of freedom.

#A large critical value indicates rejection of the
#null hypothesis in favor of the alternative.


install.packages("FinTS", repos="http://R-Forge.R-project.org") 
require(FinTS)

FinTS.stats(r)
plot(density(r))
abline(v=0)

ArchTest(r, lags=15, demean = FALSE) 
auto.arima(r^2)
# Saca un 5 2 P=5 osea que puede ser un GARCH 5,2
#----------------------con fGarch
# https://www.rmetrics.org/files/Meielisalp2008
# /Presentations/Chalabi2.pdf
install.packages("fGarch")
require(fGarch)
#MA(2) GARCH(4)
ma.arch = garchFit(formula = ~arma(0,15)+garch(5,3),data=y)
# Zn=sigmanEn
(aics = ma.arch@fit$ics)
en = ma.arch@residuals
sigma.n = ma.arch@sigma.t #volatilidad

ma.arch2 = garchFit(formula = ~arma(0,2)+garch(5,3),data=y)
(aics = ma.arch2@fit$ics)
en2 = ma.arch2@residuals
sigma2.n = ma.arch2@sigma.t #volatilidad
#-6.535140 
# BIC -6.554125
#---------------------residuos

par(mfrow=c(2,1))
TSA::acf(en,160, drop.lag.0 = TRUE,ci.type="ma")
pacf(en,160)

Box.test(en,15)
Box.test(en,45)

#----------------------pronostico
# Se esta pronosticando Xn, se trata de hacer pronosticos de los rendimientos.
# Donde se estabiliza dice hasta aqui llegue no puedo hacer mas
FinTS.stats(y)

ypr = predict(ma.arch,n.ahead=30)
str(ypr)
plot.ts(ypr)

sigma.pr = ypr[30,3]
# sqrt(250)*0.010282705
# Vender a un mes a precio preferencial
# option put a k=98.3  hoy es 84.4 el contrato es entre dos partes, a y b, a quiere vender y b le va a comprar al precio preferencial 98
# a le paga una prima por anticipado, el objeto es calcular esa prima y es por black-soles, un parametro es la volatilidad
# 

#----------------------BlackScholesOption
install.packages("fOptions")
require(fOptions)

S= rev(Close)
tail(S)
# esta formula calcula la prima
BlackScholesOption(TypeFlag = "c", #tipo de opcion
S = 89.61, X = 85.3, # cambiamos x por el 98
Time = 1/12, r = 0.05,
b = 0.00, sigma = sigma.pr) # b tiene que ver con una tasa de oportunidad, una tasa que se cobra por tener el activo en su poder 
#y tener que guardarlo por ese tiempo, cobra una especie de arriendo por ese activo, se aplica en energia y en comodities, por ejemplo el maiz.
#sigma es el pronostico

BlackScholesOption(TypeFlag = "p", #tipo de opcion
                   S = 89.61, X = 98.3, # cambiamos x por el 98
                   Time = 1/12, r = 0.05,
                   b = 0.00, sigma = sigma.pr)

#BS es muy utilizada por ser facil pero es muy criticada por que cambia el RB por una MA(2)+GARCH(5,3)
#BS no protege contra caidas en el precio sino en volatilidad.
#EN los trabajos 1 muchos tuvimos series positivas una serie positiva es volatilidad. Cuando se trabaja en GARCH se descompone en dos
#volatilidad y serie, son modelos para calcular volatilidades por que son series positivas.

# Prueba eagle H0: no hay efecto arch
# Ha: si  hay
# zn= sigman En
# En = iid N(0,1)
# EAR(1) aprox sigma
#---------------------
install.packages("tseries")
install.packages("rugarch")
require(tseries)

#----------------------
require(knitr)
require(rugarch)

# sigma2(t) = w + alfa1.eps2(t-1) + beta1.sigma2(t-1) +...
p=1 # es el orden garch : beta1, beta2,...
q=1 # es el orden arch: alfa1, alfa2,...
arch2.spec = ugarchspec(variance.model = 
list(garchOrder=c(q,p)), 
mean.model = list(armaOrder=c(0,2)),
distribution.model = "nig")

rct.fit = ugarchfit(spec=arch2.spec, 
data=y,solver.control=list(trace = 1)) 

print(rct.fit)



require(hwwntest)
hwwn.test(as.vector(rt[1:2048]))
#---------------------ajustados

sigma = rct.fit@fit$sigma

par(mfrow=c(1,1))
plot(fecha[-1],sigma, xaxt="n", panel.first = grid()
,type='l',ylab='VFTSX: rendimientos',xlab='dia',
col='darkgray')
axis.Date(1, at=ejex.mes, format="%m/%y")
axis.Date(1, at=ejex.a?o, labels = FALSE, tcl = -0.2)
lines(fecha[-1],yhat,col='red')
#---------------------volatilidad


(volat.anual= mean(sigma)*sqrt(250))

media.anual = mean(y)*250

sharpe = media.anual/volat.anual

#------------ajustar MA(2)+FIGARCH(1,d,1)

# formula = ~arma(0,0) + ~garch(1,1))

rct.fi=garchOxFit(~arma(0,2)+ ~figarch.bbm(1,1),data=y,
cond.dist = 'ged',
include.mean=TRUE,trace = TRUE)

#  print.garchOx             S3 Print Method
#  summary.garchOx           S3 Summary Method
#  plot.garchOx              S3 Plot Method


print.garchOx(rct.fi)
summary.garchOx(rct.fi)
plot.garchOx(rct.fi)



M = matrix(rct.fi$coef,7,3)
(M)
colnames(M)=c("param.est","std.error","t")
rownames(M)=c("ma1","ma2","d","omega","alfa1","beta1","nu")
require(xtable)
print(xtable(M,digits=4))

sigma.ml = sqrt(rct.fi$condvars)

par(mfrow=c(2,1))
ts.plot(sigma)
ts.plot(sigma.ml)

(volat.anual= mean(sigma.ml)*sqrt(250))

#----------pruebas de memoria larga en volatilidad
source("vs.test.r")
## significance level: 0.01,   0.05,     0.1
## critical value:     0.2685, 0.1869,   0.1518
##  Q > Qa : rechaza nula memoria corta
vs.test.nivel <- vs.test(x=r, q=3, alpha=0.05)
vs.test.volat <- vs.test(x=sigma, q=3, alpha=0.05)




