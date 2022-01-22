#### Slide 57 ####
### Teste de uma proporcao

library(pwr)

# Medida de efeito com transformação arcsin
p1 = 0.10
p2 = 0.15
h = ES.h(p1,p2)

# Qual o poder da amostra de tamanho 200?
pwr.p.test(h = h, n = 200, sig.level = 0.05, alternative = "two.sided")

# Qual o n para poder de 80%? 
pwr.p.test(h = h, power=0.8, sig.level = 0.05, alternative = "two.sided")


#### Slide 62 ####
### Teste para amostras independentes

n1 = 80
n2 = 50

# Medida de efeito com transformação arcsin
p1 = 0.40
p2 = 0.52
h = ES.h(p1,p2)

# poder das amostras de tamanho 80 e 50
pwr.2p2n.test(h=h, n1=n1,n2=n2,alternative="two.sided")

# n por grupo para ter poder de 80% - assumindo novo estudo com grupos de mesmo tamanho
pwr.2p.test(power=0.8, h=h, alternative="two.sided")

# n de mulheres para ter poder de 80% - assumindo que se dobrará o número de homens (n1=160)
pwr.2p2n.test(power=0.8, n1=160, h=h, sig.level = 0.05, alternative="two.sided")
