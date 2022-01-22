library(TeachingSampling)

data(BigLucy)







set.seed(1234)

a = 20

balfas = rep(10, a)



library(sampling)

m = mstage(BigLucy, stage=list("cluster",""), varnames=list("Zone","ID"), method=c("srswor","srswor"), size=list(a,balfas))

amostra = getdata(BigLucy,m)[[2]]

m[[1]] %>%
    filter(Zone == "county1")


# Questão 1 


Balfas = table(BigLucy$Zone)
A = length(Balfas)

emp11 = 20/A*10/Balfas[1] ### a/A*b_1/B_1



amostra %>% 
    count(Zone)


sorteados = Balfas[c(11,12,16,20,22,27,28, 33, 42,44,59,64,71, 79,8,80,84,86,9,92)]

amostra$fpc1 = rep(A, nrow(amostra))
amostra$fpc2 = rep(sorteados, balfas)

library(survey)
dclus2<-svydesign(id=~Zone+ID, fpc=~fpc1+fpc2, data=amostra)



svymean(~Income, dclus2, deff = T)
confint(svymean(~Income, dclus2))
