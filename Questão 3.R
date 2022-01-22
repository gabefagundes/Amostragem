
library(SDaA)
library(survey)
library(sampling)

data(syc)

dados = na.omit(subset(syc, select=c(stratum, agefirst, numarr, usewepn, everdrug)))

set.seed(00290601)

nh = rep(5, length(unique(dados$stratum)))


idamostra = sampling::strata(
    dados, 
    stratanames = c("stratum"), 
    size = nh, 
    method = c("srswor")
)


Nh = dados %>%
    group_by(stratum) %>%
    count() 


amostra = getdata(dados, idamostra)

amostra = amostra %>%
    mutate(tam_estrato = rep(Nh$n, each = 5))

dstrat = svydesign(
    id=~1, strata = ~Stratum, 
    probs = ~Prob, data = amostra, 
    fpc =~tam_estrato)


xb_numarr = svymean(~numarr, dstrat)
confint(xb_numarr)
xb_usewepn = svymean(~usewepn, dstrat)
svyciprop(~usewepn, dstrat)


est1 = subset(amostra, stratum == 1)

boxplot(est1$numarr)

boxplot(amostra$numarr)
