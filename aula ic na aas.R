##### Ilustra��o animada
library(animation)
conf.int()


##### Exemplo com dados

library(TeachingSampling)
data(Lucy)
renda = Lucy$Income
spam = Lucy$SPAM

# tamanho da popula��o
N = length(renda)
N

# tamanho da amostra
n = 100

############### Vari�vel quantitativa

######## Selecionando uma AASc

set.seed(2025)
amostra = sample(renda,n,replace = TRUE)

########### IC com distrib t - APENAS PARA AASC
t.test(amostra, conf=0.9)

############# An�lise com pacote Survey

library(survey)

probs = rep(n/N,n) # apesar de ser AASc, se n�o identificar assim ele n�o acerta o N e o estimador do total fica correto.

banco = data.frame(amostra,probs)

daasc = svydesign(data=banco, ids=~1, probs =~probs)
svymean(~amostra, daasc)
svytotal(~amostra, daasc) 

######## ICs
confint(svymean(~amostra, daasc))
confint(svytotal(~amostra, daasc))


############### Vari�vel dicot�mica


# Sorteio de uma amostra com reposi��o
set.seed(2025)
amostra = sample(spam, n, replace=T)
amostra = as.numeric(amostra) - 1

############# PACOTE SURVEY

library(survey)

probs = rep(n/N,n) # apesar de ser AASc, se n�o identificar assim ele n�o acerta o N e o estimador do total fica correto.

banco = data.frame(amostra,probs)

daasc = svydesign(data=banco, ids=~1, probs =~probs)
svyciprop(~amostra, daasc, method="mean")
?svyciprop
