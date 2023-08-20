require(wooldridge)
require(lmtest)
require(car)
library(dynlm)
library(foreign)
library(nortest)
library(orcutt)

#EXEMPLO 6.1
set.seed(333)
et<-rnorm(10,0,1) # Gerando erros normais N(0,1)

u=rep(0,10)

u[1]<-0.9*5+et[1]
for(i in 2:10){
  
 u[i]=0.9*u[i-1]+et[i]
  
}
u


## um residuo aparenta influenciar o próximo.
## a observação parece manter um padrao antes de "seguir o rumo"

t<-c(1,2,3,4,5,6,7,8,9,10)
X<-c(1,2,3,4,5,6,7,8,9,10)
plot(t,u)

Y<-1+0.8*X+u #Verdadeira Equação

summary(fit<-lm(Y~X))

plot(X,Y, ylim = c(2,12))
abline(fit,col="blue")
abline(a=1,b=0.8,col="red")


# Sem autocorrelação

Y1<-1+0.8*X+et
summary(fit1<-lm(Y1~X))
plot(X,Y1)
abline(fit1,col="blue")
abline(a=1,b=0.8,col="red")


rm(list = ls())

#Exemplo 6.3
data(phillips)
tsdata<-ts(phillips,start=1948)

attach(phillips)

#Ajuste
ajuste<-dynlm(inf~unem,data=tsdata)
summary(ajuste)


res<-residuals(ajuste)
ard = ls.diag(ajuste)
respadron = ard$std.res


#Verificando Normalidade
lillie.test(residuals(ajuste))

shapiro.test(residuals(ajuste))
qqnorm(residuals(ajuste))
qqline(residuals(ajuste), col=2)
##não tem normalidade



#Verificando Heterocedasticidade
bptest(ajuste, studentize=FALSE)
gqtest(ajuste)
##Não rejeita a homocedasticidade



#Gráfico resíduos x tempo
plot(year,res)

plot(respadron[-length(respadron)], respadron[-1]) #linearidade que indica correlação

acf(respadron)

# Outros Gráficos




#Testes autocorrelação


bgtest(ajuste, order = 3)


#Ajuste da primeira diferença

ajuste<-dynlm(d(inf)~unem,data=tsdata)
summary(ajuste)


res<-residuals(ajuste)
ard = ls.diag(ajuste)
respadron = ard$std.res






#Exemplo 6.4

data(prminwge)
tsdata<-ts(prminwge,start=1950)

attach(prminwge)

#MQO




#Verificando Autocorrelação




