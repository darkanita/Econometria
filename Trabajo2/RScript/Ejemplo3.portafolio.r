# ejemplo: generar rendimientos de un portafolio acciones + fiducia

 # para portfolio.optim
install.packages("timsac")
install.packages("FitAR")
install.packages("forecast")
install.packages("astsa")
library(timsac)
library(FitAR)
library(forecast)
library(astsa) # para arma.spec

Da = read.table("/Users/anita/Downloads/Capitulo 7. Procesos GARCH/acciones.fiducias.2000.2009.dat", header = TRUE, 
stringsAsFactors=FALSE)

attach(Da)

x1 = bogota
x2 = exito
x3 = bancol
x4 = trm
x5 = valle
x6 = occi
x7 = bog

x = na.omit(cbind(x1,x2,x3))

rx = apply(x,2,function(y){diff(log(y),1,1)})
colnames(rx)=c("bogota","exito","bancol")


mu = colMeans(rx)
S = cov(rx)
colnames(S) = colnames(rx)
(cov2cor(S))

require(tseries)
port.sol = portfolio.optim(x=rx, 
pm=mean(mu), covmat = S,
shorts = FALSE)

(w = port.sol$pw)
names(w) = colnames(rx)

rp = rx%*%w
#Forma el portafolio al final y se retrocede pero se realiza a si por el ejercicio

ts.plot(rp)

vp = c(igbc[1],igbc[1]*cumprod(exp(rp)))

ts.plot(vp,ylim=c(1000, 12000))
lines(igbc,col='red')

legend("topleft", 
c("portafolio", "igbc"),lty=c(1,1),
col = c("black","red"))

#-----------------analisis memoria corta ARMA

nll = matrix(nrow=5,ncol=5,dimnames=list(paste("p=",0:4,sep=""),paste("q=",0:4,sep="")))
aic = nll
lb = nll
for (p in (0:4)) {
for (q in (0:4)) {
    ARMAlabel = sprintf("ARMA(%d,%d)",p,q)
    armamodel = arima(rp,order=c(p, 0, q),include.mean = TRUE)
    lbtest = Box.test(na.omit(armamodel$resid), lag = 12,type="Ljung")
    nll[p+1,q+1] = armamodel$loglik;
    aic[p+1,q+1] = armamodel$aic;    
    lb[p+1,q+1] = lbtest$p.value
}
} 
cat("LogLik:\n")
print(nll)
cat("AIC:\n")
print(aic)
cat("Box-Ljung-Test:\n")
print(lb)

min(aic)


#-------------
require(forecast)
auto.arima(rp)
#-------------

armapq = arima(rp,order=c(3, 0, 3),include.mean = TRUE)
summary(armapq)

require(lmtest)
coeftest(armapq)

Zn = resid(armapq) 

require(TSA)

par(mfrow=c(2,2))
TSA::acf(Zn,60,ci.type="ma", drop.lag.0 = TRUE)
pacf(Zn,60)
TSA::acf(Zn^2,60,ci.type="ma", drop.lag.0 = TRUE)
pacf(Zn^2,60)

auto.arima(Zn^2)

require(FinTS)

ArchTest(Zn, lags=15, demean = FALSE) 
 

require(fGarch)

ma.arch = garchFit(formula = ~arma(3,3)+garch(1,3),data=rp)
print(ma.arch)

(aics = ma.arch@fit$ics)
en = ma.arch@residuals
sigma.n = ma.arch@sigma.t


