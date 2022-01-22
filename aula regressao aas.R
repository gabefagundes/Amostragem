##################### Exemplo Lucy

library(TeachingSampling)
data(Lucy)

renda = Lucy$Income
empreg = Lucy$Employees

N = length(renda)

TauY = sum(renda)
TauX = sum(empreg)

MiY = mean(renda)
MiX = mean(empreg)

cor(renda, empreg)

reg = lm(renda ~empreg)
summary(reg)

plot(empreg, renda)
abline(reg)

# Amostra
n = 100

set.seed(2025)
bd.sort = Lucy[sample(1:N,n,replace=T), ]

####### Regressão
##### Análise com o survey
library(survey)
daasc = svydesign(id=~1, probs=rep(n/N, n), data=bd.sort)

##### precisa conhecer o N, só calcula os coeficientes
svyglm(Income~Employees, daasc)

# estimativa de regressão da média
mean(bd.sort$Income) + 4.935*(MiX - mean(bd.sort$Employees))

# estimativa tradicional da média
svymean(~Income, daasc)




####### Exemplo 2 da seção 2.10

xpop = c(1,3,2)
ypop = c(12,30,18)

taux = sum(xpop)
tauy = sum(ypop)

mix = mean(xpop)
miy = mean(ypop)

# Regressão populacional
reg = lm(ypop ~ xpop)
summary(reg)
reg$coefficients

# Distribuição amostral da regressão - amostras com reposição
N = 3
n = 2
namostras = N^n
namostras

library(TeachingSampling)
amostrasx = OrderWR(N,n,ID=xpop)
amostrasy = OrderWR(N,n,ID=ypop)

bancocompl = data.frame(amostra=rep(1:namostras,2), x=c(amostrasx[,1],amostrasx[,2]), y=c(amostrasy[,1],amostrasy[,2]))
bancocompl = bancocompl[order(bancocompl$amostra), ]

alfa.ch = numeric()
beta.ch = numeric()
mi.chreg = numeric()
tau.chreg = numeric()

for(i in 1:namostras){
  bancoaux = subset(bancocompl, amostra == i) 
  reg = lm(bancoaux$y ~ bancoaux$x)
  alfa.ch[i] = reg$coefficients[1]
  beta.ch[i] = reg$coefficients[2]
  mi.chreg[i] = mean(bancoaux$y) + beta.ch[i]*(mix - mean(bancoaux$x))
  tau.chreg[i] = N*mi.chreg[i]
}

mi.chreg
tau.chreg

mean(mi.chreg,na.rm=T)
mean(tau.chreg,na.rm=T)

#### Comparação com estimador razão e estimador tradicional média amostral
mediasy = apply(amostrasy, 1, mean)
mediasx = apply(amostrasx, 1, mean)
razoes = mediasy/mediasx

tauRch = razoes*taux
miRch = razoes*mix

tauYch = N*mediasy

par(mfrow=c(1,3))
boxplot(tauYch,ylim=c(30,90))
boxplot(tauRch,ylim=c(30,90))
boxplot(tau.chreg,ylim=c(30,90))
boxplot(mediasy,ylim=c(10,30))
boxplot(miRch,ylim=c(10,30))
boxplot(mi.chreg,ylim=c(10,30))

var(mi.chreg,na.rm=T)
var(miRch)
var(mediasy)


