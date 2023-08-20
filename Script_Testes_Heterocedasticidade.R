require(wooldridge)
require(lmtest)
library(nortest)

data(hprice1)
attach(hprice1)

fithprice1<-lm(price~assess + bdrms+ lotsize+ sqrft+factor(colonial))
summary(fithprice1)

fithpricef<-lm(price~lotsize + sqrft)

#Gráfico dos Residuos padronizados versus valores Ajustados

respadron = fithpricef$std.res
plot(fitted(fithpricef), respadron, xlab="Fitted",
     ylab="Residuals", main="Usual")

#Teste de Normalidade

shapiro.test(residuals(fithpricef))
lillie.test(residuals(fithpricef))
qqnorm(residuals(fithpricef))
qqline(residuals(fithpricef), col=2)

#Testes Homocedasticidade

#Breusch-Pagan
bptest(fithpricef, studentize=FALSE)

#Koenker
bptest(fithpricef, studentize=TRUE)

#Goldfeld-Quandt
gqtest(fithpricef)

#White

u2 <- fithpricef$residuals^2
reg.auxiliar <- lm(u2 ~ lotsize + I(lotsize^2)+sqrft+I(sqrft^2)+(lotsize*sqrft))  #Com termos cruzados, cross-terms
summary(reg.auxiliar)

Ru2 <- summary(reg.auxiliar)$r.squared
LM <- nrow(data) * Ru2
# obtendo o numero de regressores menos o intercepto
k <- length(coefficients(reg.auxiliar)) - 1
k

p.value <- 1 - pchisq(LM, k)  # O TESTE TEM k TERMOS REGRESSORES EM reg.auxiliar
# c('LM','p.value')
c(LM, p.value)

#Usando bptest para realizar o teste de White
bptest(fithpricef, ~ lotsize + I(lotsize^2)+sqrft+I(sqrft^2)+(lotsize*sqrft))

############Transformação Log
fithpricel1<-lm(lprice~llotsize + lsqrft+lassess)
summary(fithpricel1)

fithpricelf<-lm(lprice~llotsize + lsqrft)
summary(fithpricelf)

respadronl = fithpricelf$std.res

par(mfrow=c(1,2))
plot(fitted(fithpricef), residuals(fithpricelf), xlab="Fitted",
     ylab="Residuals", main="Usual")
plot(fitted(fithpricelf), residuals(fithpricelf), xlab="Fitted",
     ylab="Residuals", main="Log")


