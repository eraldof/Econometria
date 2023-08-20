require(faraway)
library(stats)
data(gala)
fitgala=lm(Species~Area+Elevation+Scruz+Nearest+Adjacent,
           gala)
fitgalat=lm(sqrt(Species)~Area+Elevation+Scruz+Nearest+
              Adjacent, gala)
par(mfrow=c (1, 2))
plot(fitgala$fitted.values,residuals(fitgala),xlab="Fitted",
     ylab="Residuals",main="Usual")
plot(fitgalat$fitted.values,residuals(fitgalat),xlab="Fitted",
     ylab="Residuals",main="Transformed")


#Gráfico 1 apresenta um funil maior, gráfico 2 menor.

set.seed(3)
par(mfrow=c (2, 2))
plot (1:50, rnorm (50)) #Var constante
plot (1:50, sqrt ((1:50))*rnorm(50)) # Var não constante
plot (1:50, (1:50)*rnorm(50)) #Variancia não constante forte
plot(1:50, cos ((1:50)*pi/25)+rnorm(50)) #Não linear


#29 gl 
