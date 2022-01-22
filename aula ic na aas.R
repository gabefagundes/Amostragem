##### Ilustração animada
library(animation)
conf.int()


##### Exemplo com dados

library(TeachingSampling)
data(Lucy)
renda = Lucy$Income
spam = Lucy$SPAM

# tamanho da população
N = length(renda)
N

# tamanho da amostra
n = 100

############### Variável quantitativa

######## Selecionando uma AASc

set.seed(2025)
amostra = sample(renda,n,replace = TRUE)

########### IC com distrib t - APENAS PARA AASC
t.test(amostra, conf=0.9)

############# Análise com pacote Survey

library(survey)

probs = rep(n/N,n) # apesar de ser AASc, se não identificar assim ele não acerta o N e o estimador do total fica correto.

banco = data.frame(amostra,probs)

daasc = svydesign(data=banco, ids=~1, probs =~probs)
svymean(~amostra, daasc)
svytotal(~amostra, daasc) 

######## ICs
confint(svymean(~amostra, daasc))
confint(svytotal(~amostra, daasc))


############### Variável dicotômica


# Sorteio de uma amostra com reposição
set.seed(2025)
amostra = sample(spam, n, replace=T)
amostra = as.numeric(amostra) - 1

############# PACOTE SURVEY

library(survey)

probs = rep(n/N,n) # apesar de ser AASc, se não identificar assim ele não acerta o N e o estimador do total fica correto.

banco = data.frame(amostra,probs)

daasc = svydesign(data=banco, ids=~1, probs =~probs)
svyciprop(~amostra, daasc, method="mean")
?svyciprop
