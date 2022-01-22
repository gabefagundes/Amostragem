##### media e total

# n necessário para estimar mi com margem de erro absoluta 5 e 95% confiança
library(samplingbook)
sample.size.mean(e = 5, S= sqrt(600), N = Inf, level = 0.95) # AASc
sample.size.mean(e = 5, S= sqrt(600), N = 2000, level = 0.95) # AASs


# n necessário para estimar mi com margem de erro relativa de 4% e 99% confiança
library(epiR)
CV = 0.5
epi.simplesize(epsilon.r = 0.04, Vsq = CV^2, method="mean", conf.level = 0.99) # AASc 
epi.simplesize(N = 1000, epsilon.r = 0.04, Vsq = CV^2, method="mean", conf.level = 0.99) # AASs

# para total troca-se mean por total em method, mas o resultado é igual

##### Tamanho de amostra para proporções com erro absoluto 
N = 4000
sample.size.prop(e=0.05, P = 0.6, N = Inf, level = 0.99) #AASc
sample.size.prop(e=0.05, P = 0.6, N = N, level = 0.99) #AASs

##### Tamanho de amostra para proporções com erro relativo
p = 0.5
epi.simplesize(epsilon.r = 0.10, Py = p, method="proportion", conf.level = 0.95) # AASc 
epi.simplesize(N=N, epsilon.r = 0.10, Py = p, method="proportion", conf.level = 0.95) # AASs 







## EXAMPLE 1:
## A city contains 20 neighbourhood health clinics and it is desired to take a 
## sample of clinics to estimate the total number of persons from all these 
## clinics who have been given, during the past 12 month period, prescriptions 
## for a recently approved antidepressant. If we assume that the average number 
## of people seen at these clinics is 1500 per year with the standard deviation 
## equal to 300, and that approximately 5% of patients (regardless of clinic) 
## are given this drug, how many clinics need to be sampled to yield an estimate 
## that is within 20% of the true population value?

pmean <- 1500 * 0.05
pvar <- (300 * 0.05)^2 + 0.05*0.95*1500
epi.simplesize(N = 20, Vsq = (pvar / pmean^2), epsilon.r = 0.20, 
               method = "total", conf.level = 0.95)

## Three clinics need to be sampled to meet the survey requirements. 

## EXAMPLE 2:
## We want to estimate the mean bodyweight of deer on a farm. There are 278
## animals present. We anticipate the mean body weight to be around 200 kg
## and the standard deviation of body weight to be 30 kg. We would like to
## be 95% certain that our estimate is within 10 kg of the true mean. How
## many deer should be sampled?

epi.simplesize(N = 278, Vsq = 30^2 / 200^2, epsilon.r = 10/200, 
               method = "mean", conf.level = 0.95)

## A total of 31 deer need to be sampled to meet the survey requirements.