########################Exemplo 1#################################







###############################Exemplo 2###################################
y<-c(70,65,90,95,110,115,120,140,155,150)
x2<-c(80,100,120,140,160,180,200,220,240,260)
x3<-c(810,1009,1273,1425,1633,1876,2052,2201,2435,2686)
library(foreign)
library(car)
fit1=lm(y~x2+x3)
summary(fit1)

confint(fit1)

#Regressão X3 e X2
fitx=lm(x3~x2)
summary(fitx)

#Regressões Individuais
fiti1=lm(y~x2)
summary(fiti1)

fiti2=lm(y~x3)
summary(fiti2)

# Correlação
cor(x2,x3)

#Gráfico dispersão ###

X<-cbind(y,x2,x3)
cor(X)

#Gráfico Dispersão
pairs(X)


#### Fator Inflação de Variância
vif(fit1)

#####################Exemplo 3###################

dados=read.table("dados_Multicolinearidade_Consumo.R", header=T)
attach(dados)
names(dados)


