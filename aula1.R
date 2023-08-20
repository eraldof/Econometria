rm(list=ls())
DadosExemplo1Aula2 <- read.csv("C:/Users/erald/Desktop/Faculdade/Econometria/DadosExemplo1Aula2.txt", row.names=1, sep="")

plot(DadosExemplo1Aula2)
fit <- lm(DadosExemplo1Aula2$desp_alim ~ DadosExemplo1Aula2$rsem)
abline(fit)
abline(a = 47.55265, b= 0.11942)
summary(fit)
