#### Carregando banco de dados
#library(TeachingSampling)
library(SDaA)

data(syc)

dados = na.omit(subset(syc, select=c(stratum, psu, numarr)))
#### Informando vari vel que define os conglomerados e calculando seus tamanhos
congls = dados$stratum
# Para calcular valores relativos à variável psu, retire o comentario da linha a seguir
# congls = dados$psu

table(congls)

Balfas = table(congls)
Bbarra = mean(Balfas)

A = length(Balfas)

N = A*Bbarra

#### Informando vari vel de an lise
variavel = dados$numarr 

#### Par metros por conglomerado 
totais_congl = tapply(variavel,congls,sum)
medias_congl = tapply(variavel,congls,mean)
varpop = function(x){
    var(x)*(length(x)-1)/length(x)
}
vars_congl = tapply(variavel,congls,varpop)

#### Par metros gerais
# M dia populacional
mi = mean(variavel)

# M dia das m dias
mibarra = mean(medias_congl)

# Total populacional
totalp = sum(variavel)

# Total m dio
taubarra = mean(totais_congl)

# Variancia populacional
sigma2 = ((N-1)/N)*var(variavel)

# Vari ncia dentro de conglomerados
sigma2dc = sum((Balfas/Bbarra)*vars_congl)/A

# Vari ncia entre conglomerados
sigma2ec = sum((Balfas/Bbarra)*(medias_congl-mi)^2)/A

sigma2dc + sigma2ec

# Vari ncia entre totais
var_totais = varpop(totais_congl)




#### selecao de uma unica amostra
a = 8

set.seed(290601)

library(sampling)
am_cl = sampling::cluster(data=dados, size = a, clustername=c("stratum"), method="srswor")
amostra = sampling::getdata(dados, am_cl)
n = dim(amostra)[1]


#### 


####

library(survey)

dcl <- svydesign(id=~stratum, data=amostra, probs=~Prob, fpc=rep(A,n))

svymean(~numarr, dcl, deff = T)

confint(svymean(~numarr, dcl))
