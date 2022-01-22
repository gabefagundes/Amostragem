########################### RAZÃO ###############################################

# Exemplo 3

xpop = c(1,3,2)
ypop = c(12,30,18)

taux = sum(xpop)
tauy = sum(ypop)

R = tauy/taux # razão verdadeira

# Distribuição amostral da razão - amostras com reposição
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

mean(razoes) # estimador viesado para a razão

# Estimador do total de Y pela razão

tauRch = razoes*taux
mean(tauRch) # estimador viesado

tauYch = N*mediasy
mean(tauYch) # estimador não viciado

8*var(tauRch)/9
8*var(tauYch)/9

cor(xpop,ypop)

### Ilustração

library(animation)
?sample.ratio
sample.ratio(size = 30, m.col=c("black", "green"))

# Exercício: trocar os dados para y = c(30,12,18)


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

####### RAZÃO

# Estimador da razão
mean(bd.sort$Income)/mean(bd.sort$Employees)

# Estimador do Total de Y pela razão
TauX*mean(bd.sort$Income)/mean(bd.sort$Employees)

# Estimador do Total de Y tradicional
N*mean(bd.sort$Income)

##### Análise com o survey
##### precisa conhecer o N
library(survey)
daasc = svydesign(id=~1, probs=rep(n/N, n), data=bd.sort)

# Estimativa da razão
svyratio(~Income,~Employees, daasc)

# estimativa tradicional do total
svytotal(~Income, daasc)

# Estimativa do total pela razão
predict(svyratio(~Income,~Employees, daasc), total=TauX)


