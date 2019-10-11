library(dplyr)
library(tidyr)

M2 <- 0.066
T_n <- 200
C_n <- 150
T_event <- 74
C_event <- 62

phat.T <- T_event/T_n
phat.C <- C_event/C_n

phat.d <- phat.T - phat.C
alpha <- 0.025

# 95% Wald CI
phat.d - qnorm(1-alpha)*sqrt(phat.T*(1-phat.T)/T_n + phat.C*(1-phat.C)/C_n)
phat.d + qnorm(1-alpha)*sqrt(phat.T*(1-phat.T)/T_n + phat.C*(1-phat.C)/C_n)

# 95% FM CI
theta <- C_n/T_n
a <- 1 + theta
b <- -1*(1 + theta + phat.T + theta*phat.C + M2*(theta + 2))
c <- M2^2 + M2*(2*phat.T + theta + 1) + phat.T + theta*phat.C
d <- -phat.C*M2*(1 + M2)

v <- b^3/(27*a^3) - b*c/(6*a^2) + d/(2*a)
u <- sign(v)*(b^2/(9*a^2) - c/(3*a))^0.5
w <- 1/3*(pi + acos(v/u^3))

phat.T.rmle <- 2*u*cos(w) - b/(3*a)
phat.C.rmle <- phat.T.rmle + d

phat.d - qnorm(1-alpha)*sqrt(phat.T.rmle*(1-phat.T.rmle)/T_n + phat.C.rmle*(1-phat.C.rmle)/C_n)
phat.d + qnorm(1-alpha)*sqrt(phat.T.rmle*(1-phat.T.rmle)/T_n + phat.C.rmle*(1-phat.C.rmle)/C_n)

# 95% Newcombe-Wilson CI
z <-qnorm(1-alpha) 
l.C <- (phat.C + z^2/(2*C_n) - 
         z*sqrt((phat.C*(1 - phat.C) + z^2/(4*C_n))/C_n))/(1 + z^2/C_n)
u.C <- (phat.C + z^2/(2*C_n) + 
         z*sqrt((phat.C*(1 - phat.C) + z^2/(4*C_n))/C_n))/(1 + z^2/C_n)

l.T <- (phat.T + z^2/(2*T_n) - 
         z*sqrt((phat.T*(1 - phat.T) + z^2/(4*T_n))/T_n))/(1 + z^2/T_n)
u.T <- (phat.T + z^2/(2*T_n) + 
         z*sqrt((phat.T*(1 - phat.T) + z^2/(4*T_n))/T_n))/(1 + z^2/T_n)

phat.d - sqrt((phat.T - l.T)^2 + (u.C - phat.C)^2)
phat.d + sqrt((u.T - phat.T)^2 + (phat.C - l.C)^2)

