library(ggplot2)
library(readxl)

# Generujemy dane
b0 <- 4
b1 <- 1
n <- 200

x <- runif(n, min = 0, max = 4)
eps <- rnorm(n,mean = 0, sd = 0.5)
y <- b0 + b1*x^2 + eps 
  
dane <- data.frame(x=x,y=y)
#dane <- as.data.frame(read_excel("lab-3-2026.xlsx"))
plot(dane)

# Zadanie 1 - dopasowanie modelu
# Współczynnik R^2
# Analiza reszt

# Wyznacza beta0, beta1, R^2 i zwraca wektor predykcji y_pred
wyznaczenie_i_analiza_modelu <- function(x,y){
  
  sr_x <- mean(x)
  sr_y <- mean(y)
  
  beta_1 <- sum( (x-sr_x) *(y- sr_y) ) / (sum( (x- sr_x)^2 ))
  beta_0 <- sr_y - beta_1 * sr_x

  # Predykcja
  y_pred <- beta_0 + beta_1*x
  
  SSE <- sum((y-y_pred)^2)
  SST <- sum((y - mean(y))^2)
  R_kw <- 1- SSE/SST

  return(c(beta_0, beta_1, R_kw))
}

beta_0 <- wyznaczenie_i_analiza_modelu(x,y)[1]
beta_1 <- wyznaczenie_i_analiza_modelu(x,y)[2]

R_kw <- wyznaczenie_i_analiza_modelu(x,y)[3] # R^2 wynosi 0.89, nie jest to zły wynik
y_pred <- beta_0 + beta_1 *x

reszty <- y_pred - y
plot(x, reszty)
plot(y_pred, reszty)
# Wektor reszt wykazuje trend wielomianowy

hist(reszty)
qqnorm(reszty)
qqline(reszty, col = "blue")
# Histogram i wykres kwantylowy  wskazują na poważne odstępstwa

# Zadanie 2 - wykres rozrzutu
plot(x,y)
# zdecydowanie nie jest to charakter liniowy


# Zadanie 3 - przekształcenie danych
# Szukamy jakiejś funkcji f()

nowe_x <- x^2
nowe_dane <- data.frame(x=nowe_x, y=y)

plot(nowe_dane$x, nowe_dane$y)
# Teraz punkty układają się wokół funkcji liniowej

wyznaczenie_i_analiza_modelu(nowe_x,y) # Poprawiło to współczynnik R^2

nowe_beta_0 <- wyznaczenie_i_analiza_modelu(nowe_x,y)[1]
nowe_beta_1 <- wyznaczenie_i_analiza_modelu(nowe_x,y)[2]
nowe_R_kw <- wyznaczenie_i_analiza_modelu(nowe_x,y)[3]

nowe_y_pred <- nowe_beta_0 + nowe_beta_1*nowe_x

nowe_reszty <- y - nowe_y_pred
plot(nowe_x, nowe_reszty)
plot(nowe_y_pred, nowe_reszty)
hist(nowe_reszty, breaks = 20)
qqnorm(nowe_reszty)
qqline(nowe_reszty, col = "blue")
# Histogram i wykres kwantylowy wyglądają w porządku

podsumowanie_y <- data.frame(y=y, y_stare = y_pred, y_nowe = nowe_y_pred)

# Możemy obliczyc miary dokładności dopasowania
MAE_stary <- mean(abs(y - y_pred))
MAE_nowy <- mean(abs(y - nowe_y_pred))
MSE_stary <- mean((y - y_pred)^2)
MSE_nowy <- mean((y - nowe_y_pred)^2)

# W obu przypadkach wartości tych miar są mniejsze dla nowego modelu





########################## Analiza dla danych z excela ########################

dane <- as.data.frame(read_excel("lab-3-2026.xlsx"))
plot(dane)

x <- dane$x
y <- dane$y

# Zadanie 1 - dopasowanie modelu
# Współczynnik R^2
# Analiza reszt

# Wyznacza beta0, beta1, R^2 i zwraca wektor predykcji y_pred
wyznaczenie_i_analiza_modelu <- function(x,y){
  
  sr_x <- mean(x)
  sr_y <- mean(y)
  
  beta_1 <- sum( (x-sr_x) *(y- sr_y) ) / (sum( (x- sr_x)^2 ))
  beta_0 <- sr_y - beta_1 * sr_x
  
  # Predykcja
  y_pred <- beta_0 + beta_1*x
  
  SSE <- sum((y-y_pred)^2)
  SST <- sum((y - mean(y))^2)
  R_kw <- 1- SSE/SST
  
  return(c(beta_0, beta_1, R_kw))
}

beta_0 <- wyznaczenie_i_analiza_modelu(x,y)[1]
beta_1 <- wyznaczenie_i_analiza_modelu(x,y)[2]

R_kw <- wyznaczenie_i_analiza_modelu(x,y)[3] # R^2 wynosi 0.8101715, nie jest to zły wynik
y_pred <- beta_0 + beta_1 *x

reszty <- y_pred - y
plot(x, reszty)
plot(y_pred, reszty)
# Wektor reszt wykazuje trend (być może wielomianowy)

hist(reszty)
qqnorm(reszty)
qqline(reszty, col = "blue")
# Histogram i wykres kwantylowy  wskazują na poważne odstępstwa (zwłaszcza ostatni kwantyl)

# Zadanie 2 - wykres rozrzutu
plot(x,y)
# zdecydowanie nie jest to charakter liniowy
# Być może logarytm, albo pierwiastek


# Zadanie 3 - przekształcenie danych
# Szukamy jakiejś funkcji f()

nowe_x <- log(x)
nowe_dane <- data.frame(x=nowe_x, y=y)

plot(nowe_dane$x, nowe_dane$y)
# Teraz punkty układają się wokół funkcji liniowej

wyznaczenie_i_analiza_modelu(nowe_x,y) # Poprawiło to współczynnik R^2

nowe_beta_0 <- wyznaczenie_i_analiza_modelu(nowe_x,y)[1]
nowe_beta_1 <- wyznaczenie_i_analiza_modelu(nowe_x,y)[2]
nowe_R_kw <- wyznaczenie_i_analiza_modelu(nowe_x,y)[3]
# 0.9765857, zdecydowana poprawa dopasowania modelu

nowe_y_pred <- nowe_beta_0 + nowe_beta_1*nowe_x

nowe_reszty <- y - nowe_y_pred
plot(nowe_x, nowe_reszty)
plot(nowe_y_pred, nowe_reszty)
hist(nowe_reszty, breaks = 14) # Histogram nie mówi za dużo
qqnorm(nowe_reszty)
qqline(nowe_reszty, col = "blue")
# Wykres kwantylowy wyglądają lepiej, nie ma widocznych żadnych poważnych odstępstw

podsumowanie_y <- data.frame(y=y, y_stare = y_pred, y_nowe = nowe_y_pred)

# Możemy obliczyć miary dokładności dopasowania
MAE_stary <- mean(abs(y - y_pred))
MAE_nowy <- mean(abs(y - nowe_y_pred))
MSE_stary <- mean((y - y_pred)^2)
MSE_nowy <- mean((y - nowe_y_pred)^2)

# W obu przypadkach wartości tych miar są mniejsze dla nowego modelu



