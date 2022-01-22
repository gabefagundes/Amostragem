
# Questao 1 ---------------------------------------------------------------

A = 1000
B_alpha = 50
a = 10
N = A*B
b_alpha = 2 

yb_alpha = c(10,7,14,17,12,6,6,8,5,15)/2

tau_alpha_h = B_alpha*yb_alpha

taub_alpha_h = sum(tau_alpha_h)/10

tau_hat = A*taub_alpha_h


mu_hat = sum(yb_alpha)/a

S2T = sum((tau_alpha_h - tau_hat)^2)/(a-1)
S2alpha = 

var = ((1-a/A))