
#####  Generowanie zbioru danych do regresji logistycznej ######
# Parametry
n <- 300
p <- 20 # liczba zm. objaśniających
# X_1 jest stale równa 1

# Tworzymy wektor beta - "nieznane" współczynniki estymowane na podstawie próby

beta <- c(sample((1:8)/2, size = 10, replace = TRUE),rep(0,10))

# Dla każdego rekordu w danych
x_gen <- c(1,rnorm(p-1,mean = 0, sd = 1))

pi_x <- 1/(1+ exp(-sum(beta*x_gen)))
pi_x
