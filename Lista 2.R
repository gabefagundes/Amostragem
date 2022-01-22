library(tidyverse)
library(sampling)
library(magrittr)

# Questão 1  --------------------------------------------------------------

dados = tibble(
    quarteirao = factor(c(1,2,3,4)),
    N_lojas = c(4,6,2,6),
    N_funcionarios = c(12,24,10,12)
)

A = 4 
B = dados$N_lojas

# tamanho médio dos conglomerados

B_bar = mean(B)
N = A*mean(B_bar)

# tamanho populacional de funcionarios

tau = sum(dados$N_funcionarios)
tau_bar = tau/A

# média populacional 

mu = tau/N

# média por conglomerado

mu_alpha = dados$N_funcionarios/dados$N_lojas

# média das médias

mu_barra = mean(mu_alpha)

# variancia entre totais

sig2_ec_t = sum((dados$N_funcionarios - tau_bar)^2)/A



# Questão 2 ---------------------------------------------------------------
a = 2

am_cl = cluster(data=dados, clustername=c("quarteirao"), a, method="srswor")

amostra = getdata(dados, am_cl)

T_bar = sum(amostra$N_funcionarios)/a

tau_hat = A*T_bar

mu_hat = T_bar/B_bar

sig2_tau_hat = A^2*(A-a)*sig2_ec_t/(a*(A-1))
sig2_mu_hat = (A-a)*sig2_ec_t/(B_bar^2*(A-1)*a)


## ICS

# tau

IC_tau = tau_hat + c(-qnorm(0.975)*sqrt(sig2_tau_hat), qnorm(0.975)*sqrt(sig2_tau_hat))


#mu [

IC_mu = mu_hat + c(-qnorm(0.975)*sqrt(sig2_mu_hat), qnorm(0.975)*sqrt(sig2_mu_hat))



# Questão 2 ---------------------------------------------------------------


U = tibble(
    x = c(2, 6, 8, 10, 10, 12),
    congC = factor(c(1,2,2,2,3,3)),
    congD = factor(c(1,1,1,2,2,3))
)

A = 3
Bc = c(1,3,2)
Bc_bar = mean(Bc)
Bd = c(3,2,1)
Bd_bar = mean(Bd)
mu = mean(U$x)


# medias

muC = U %>% group_by(congC) %>% summarise(media = mean(x))
muD = U %>% group_by(congD) %>% summarise(media = mean(x))

#Variancias

#C
varC_alfa = U %>%
    group_by(congC)%>%
    summarise(vard = sum((x - mean(x))^2/6))

varC_d = sum(varC_alfa$vard)
varC_e = sum(Bc*(muC$media - mu)^2)/6


#D
varD_alfa = U %>%
    group_by(congD)%>%
    summarise(vard = sum((x - mean(x))^2)/6)

varD_d = sum(varD_alfa$vard)

varD_e = sum(Bd*(muD$media - mu)^2)/6


# Amostra

sample(3, size = 1)


amostra_C = U %>%
    filter(congC == 1)


mean(amostra_C$x)
var(amostra_C$x)



# 4 -----------------------------------------------------------------------

dados = tibble(
    caixa = factor(c(rep(c(2,3,5), each = 5))),
    altura = c(40,25,35,20,30,25,20,20,20,25,20,25,25,30,30)
)

A = 50
B = 5
a = 3

mu_alpha = dados%>%
    group_by(caixa) %>%
    summarise(media = mean(altura)) 

tau_alpha = dados%>%
    group_by(caixa) %>%
    summarise(total = sum(altura))


s2_ect = sum((tau_alpha$total - mean(tau_alpha$total))^2)/(a-1)



# Estimação do total
ep_tau = sqrt(s2_ect*A^2/a)
tau_hat = A*mean(tau_alpha$total)
IC_tau = tau_hat + c(-qnorm(0.975)*ep_tau, qnorm(0.975)*ep_tau)

# Estimação da média

ep_mu = sqrt(s2_ect/(a*B^2))

mu_hat = tau_hat/(A*B)
IC_mu = mu_hat + c(-qnorm(0.975)*ep_mu, qnorm(0.975)*ep_mu)

# c - s2ec, s2dc, roint

s2_dc = dados %>%
    group_by(caixa) %>%
    summarise(SQ = sum((altura - mean(altura))^2)) %>%
    select(SQ) %>%
    sum() %>%
    divide_by(a*B)


s2_ec = (mu_alpha$media - mu_hat)^2 %>%
    sum() %>%
    divide_by(a-1)

r_int = (s2_ec - s2_dc/(B-1))/(s2_dc + s2_ec)
