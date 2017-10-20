
#----------------
require(fGarch)

set.seed(123)
n = 500
spec = garchSpec(model = list(omega = 0.001,alpha = 0.1, 
beta = c(0.2,0.5)))
Zn = garchSim(spec, n = n) # Garch (1,2)

Pn = 1000*cumprod(exp(Zn))
par(mfrow=c(2,1))
ts.plot(Zn)
ts.plot(Pn)

#----------------
require(rugarch)

arch1.spec = ugarchspec(variance.model = 
list(garchOrder=c(1,2)),                        
mean.model = list(armaOrder=c(0,0)),
fixed.pars=list(mu=0,omega=0.0001, 
alpha1=0.1,
beta1=0.2, beta2=0.5))


sim.obj = ugarchpath(arch1.spec, n.sim=n)
sigma.n = sim.obj@path$sigmaSim
Zn = sim.obj@path$seriesSim
en = sim.obj@path$residSim

Pn = 1000*cumprod(exp(Zn))
par(mfrow=c(2,2))
ts.plot(sigma.n)
ts.plot(en)
ts.plot(Zn)
ts.plot(Pn)

#--------parametros del modelo EAR(1)

theta = 0.4
lambda = 0.8


#--------simular el modelo
Xn = integer(n)

Xn[1] = (1-theta)/lambda # El proceso
for(j in 2:n){
In = rbinom(1,1,1-theta)
En = rexp(1,rate=lambda)
Xn[j] = theta*Xn[j-1] + In*En
}
#--------cambiar escala para que 
#--------quede en la misma del GARCH

sigma.ear = Xn*max(sigma.n)/max(Xn) # Ponerlos en la misma escala es un truquito

Zn.ear = en*sigma.ear

Pn.ear = 1000*cumprod(exp(Zn.ear))

par(mfrow=c(3,2))
ts.plot(sigma.ear)
ts.plot(sigma.n)
ts.plot(Zn.ear)
ts.plot(Zn)
ts.plot(Pn.ear)
ts.plot(Pn)

#---------------prueba Engle de efectos ARCH
#------------https://www.mathworks.com/help/econ/engles-arch-test.html
The test statistic for Engle?s ARCH test is 
the usual F statistic for the regression 
on the squared residuals. 

Under the null hypothesis, the F statistic 
follows a ?2 distribution with m degrees of freedom.

 A large critical value indicates rejection of the
 null hypothesis in favor of the alternative.


install.packages("FinTS", repos="http://R-Forge.R-project.org") 
require(FinTS)

FinTS.stats(Zn)
FinTS.stats(Zn.ear)

plot(density(Zn))
abline(v=0)

plot(density(Zn.ear))
abline(v=0)

ArchTest(Zn, lags=15, demean = FALSE) 
# Se rechaza p-value = 0.02362

ArchTest(Zn.ear, lags=15, demean = FALSE) 
# No la rechaza no hay efecto ARCH.  

# La prueba es capaz de detectar correctamente un ARCH


