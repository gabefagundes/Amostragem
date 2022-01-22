library(tidyverse)
# Questão 1 ---------------------------------------------------------------

dados_1 = tibble(
    n_agencias = c(299, 835, 351, 234, 305, 1025, 81, 201, 199, 2502, 6113, 1656, 263, 1035, 1105, 707),
    reg = factor(rep(c('ne', 'se', 's'), c(9,4,3)))
)


desc = dados_1 %>%
    group_by(reg) %>%
    summarise(tau = sum(n_agencias), mu = mean(n_agencias), B_alpha = n())

#tamanho médio dos conglomerados

desc$B_alpha %>% mean


# total dos conglomerados 

desc$tau

# total populacional

desc$tau %>% sum

# média populacional 

dados_1$n_agencias %>% mean

# média por conglomerado

desc$mu


#média das médias 

desc$mu %>% mean



# Questão 3 ---------------------------------------------------------------
library(tidyverse)
library(samplingbook)

dados_3 = tibble(
    N_h = c(60, 40, 100),
    n_h = c(15, 10, 20),
    pi= c(0.3, 0.2, 0.4),
    S_h = sqrt(pi*(1-pi))
)

e = 0.1

attach(dados_3)
stratasize(e = e, Nh= N_h, Sh = S_h, level = 0.95, 
           type="prop") #AEpr

stratasamp(n = 60, Nh= Nh, Sh = sigmah,
           type="prop") #AEpr
detach(dados_3)
