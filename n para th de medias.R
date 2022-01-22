#### Slides 32 e 33 ####

## Teste de uma média

# Qual o poder da amostra de tamanho 50?

library(pwr)

pwr.t.test(d = 0.38, n = 50, sig.level = 0.05, type = "one.sample", alternative = "two.sided")

# Qual o n para poder de 80%? 

pwr.t.test(d = 0.38, power = 0.8, sig.level = 0.05, type = "one.sample", alternative = "two.sided")

#### Slides 39 e 40 ####

### Teste para amostras pareadas

# qual o poder?
pwr.t.test(d = -0.84, n = 10, sig.level = 0.05, type = "paired", alternative = "less")

# qual o n?
pwr.t.test(d = -0.84, power = 0.8, sig.level = 0.05, type = "paired", alternative = "less")

#### Slide 41 ####

# Informaçoes para WinPEPI

x=c(22,25,23,18,24,24,17,23,22,23)
y=c(18,20,20,17,16,20,20,20,20,24)

sd(x)
sd(y)
cor(x,y)

#### Slides 48 e 49 ####

### Teste para amostras independentes

n1 = 39
n2 = 11

xb1 = 6.17
xb2 = 6.71

s1 = 0.71
s2 = 0.80
s = sqrt((s1^2 + s2^2)/2)

d = (xb1-xb2)/s

# poder
pwr.t2n.test(n1=n1,n2=n2,d=d,alternative="two.sided")

# n por grupo (apenas grupos de mesmo tamanho)
pwr.t.test(power=0.8,d=d, type="two.sample",alternative="two.sided")
