const = 192.5

mi0 = 190
mi1 = 196
s2 = 225
n = 42

alfa = 1 - pnorm(const, mi0, sqrt(s2/n))
beta = pnorm(const, mi1, sqrt(s2/n))


##### Procedimento: fixar o alfa, obter a constante em função de n,
# fixar o beta, descobrir o n que garante o beta
# n = (z_alfa + z_(beta))^2*s2/(mi1-mi0)^2

alfa = 0.05
beta = 0.20

# no exemplo
(qnorm(1-alfa)+qnorm(1-beta))^2*s2/((mi1-mi0)^2)

n = 39
const = qnorm(1-alfa, mi0, sqrt(s2/n))
alfa = 1 - pnorm(const, mi0, sqrt(s2/n))
beta = pnorm(const, mi1, sqrt(s2/n))
alfa
beta
