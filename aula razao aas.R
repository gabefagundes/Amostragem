########################### RAZ�O ###############################################

# Exemplo 3

xpop = c(1,3,2)
ypop = c(12,30,18)

taux = sum(xpop)
tauy = sum(ypop)

R = tauy/taux # raz�o verdadeira

# Distribui��o amostral da raz�o - amostras com reposi��o
N = 3
n = 2
namostras = N^n
namostras

library(TeachingSampling)
amostrasx = OrderWR(N,n,ID=xpop)
mediasx = apply(amostrasx,1,mean)
amostrasx
mediasx

amostrasy = OrderWR(N,n,ID=ypop)
mediasy = apply(amostrasy,1,mean)
amostrasy
mediasy

razoes = mediasy/mediasx

mean(razoes) # estimador viesado para a raz�o

# Estimador do total de Y pela raz�o

tauRch = razoes*taux
mean(tauRch) # estimador viesado

tauYch = N*mediasy
mean(tauYch) # estimador n�o viciado

8*var(tauRch)/9
8*var(tauYch)/9

cor(xpop,ypop)

### Ilustra��o

library(animation)
?sample.ratio
sample.ratio(size = 30, m.col=c("black", "green"))

# Exerc�cio: trocar os dados para y = c(30,12,18)


##################### Exemplo Lucy

library(TeachingSampling)
data(Lucy)

renda = Lucy$Income
empreg = Lucy$Employees

N = length(renda)

TauY = sum(renda)
TauX = sum(empreg)

R = TauY/TauX
R

MiY = mean(renda)
MiX = mean(empreg)

cor(renda, empreg)
plot(empreg, renda)

# Amostra
n = 100

set.seed(2025)
bd.sort = Lucy[sample(1:N,n,replace=T), ]

####### RAZ�O

# Estimador da raz�o
mean(bd.sort$Income)/mean(bd.sort$Employees)

# Estimador do Total de Y pela raz�o
TauX*mean(bd.sort$Income)/mean(bd.sort$Employees)

# Estimador do Total de Y tradicional
N*mean(bd.sort$Income)

##### An�lise com o survey
##### precisa conhecer o N
library(survey)
daasc = svydesign(id=~1, probs=rep(n/N, n), data=bd.sort)

# Estimativa da raz�o
svyratio(~Income,~Employees, daasc)

# estimativa tradicional do total
svytotal(~Income, daasc)

# Estimativa do total pela raz�o
predict(svyratio(~Income,~Employees, daasc), total=TauX)


