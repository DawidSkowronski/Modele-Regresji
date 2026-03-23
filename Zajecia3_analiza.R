library(readxl)

dane <- as.data.frame(read_excel("lab 3 (2026).xlsx"))

n <- nrow(dane)
x <- dane$x
y <- dane$y

parametry_i_R_kw <- function(x,y){
  sr_x <- mean(x)
  sr_y <- mean(y)
  
  beta_1 <- sum((x-sr_x)*(y-sr_y))/sum((x-sr_x)^2)
  beta_0 <- sr_y - beta_1*sr_x
  
  y_pred <- beta_0 + beta_1*x
  
  SSR <- sum((y_pred - sr_y)^2)
  SST <- sum((y-sr_y)^2)
  
  R_kw <- SSR/SST
  
  return(c(beta_0,beta_1,R_kw))
}

model1 <- parametry_i_R_kw(x,y)

beta_0 <- model1[1]
beta_1 <- model1[2]
R_kw <- model1[3]

# R^2 = 0.8101715, nie jest to najgorszy wynik

y_pred <- beta_0 + beta_1*x

reszty <- y - y_pred

# UWAGA: Dane zawierają tylko 50 obserwacji
hist(reszty)
qqnorm(reszty)
qqline(reszty, col = "blue")
# Wykres kwantylowy wskazuje na odstępstwa

plot(x,reszty) # Na wykresie rozrzutu widoczny jest wyraźny trend (być może wielomianowy)
plot(y, reszty)
# Zatem nie jest spełnione założenie o zerowej korelacji między (x_i, e_i) i (y_i,e_i)

##### Zadanie 2 ####
plot(x,y)
# Na podstawie wykresu rozrzutu możemy stwierdzić, że zależność między zmiennymi X i Y nie
# ma charakteru liniowego. Jest on inny lub bardziej skomplikowany
# (być może logarytmiczny lub pierwiastkowy)

##### Zadanie 3 ####
# Pomysł: Zlogarytmowanie X

nowe_x <- log(x)
nowe_dane <- data.frame(x=nowe_x, y=y)

plot(nowe_x, y)
# Teraz zależność na wykresie rozrzutu wygląda na liniową.

model2 <- parametry_i_R_kw(nowe_x,y)

beta_0_nowe <- model2[1]
beta_1_nowe <- model2[2]
R_kw_nowe <- model2[3]

# R^2 = 0.9765857 bardzo dobre wyjaśnienie zmienności danych, dopasowanie modelu

nowe_y_pred <- beta_0_nowe + beta_1_nowe*nowe_x

nowe_reszty = y- nowe_y_pred

plot(nowe_x, nowe_reszty)
plot(y, nowe_reszty)
# Nie widać żadnych regularnych wzorców
hist(nowe_reszty)
qqnorm(nowe_reszty)
qqline(nowe_reszty, col = "blue")


# Zadanie 6
podsumowanie_y <- data.frame(
  y=y,
  y_pred = y_pred,
  nowe_y_pred = nowe_y_pred 
)

podsumowanie_y
# nowe predykcje są bliższe rzeczywistych 

model1 <- function(x){return(beta_0 + beta_1*x)}
model_nowy <- function(x){return(beta_0_nowe + beta_1_nowe*x)}

# Zestawienie modeli
plot(x,y)
lines(x,y_pred, col = "red")
lines(x, nowe_y_pred, col = "blue")



# Możemy policzyć MAE i MSE - miary dokładności dopasowania modelu
MAE <- 1/n * sum(abs(y-y_pred))
MAE_nowe <- 1/n * sum((y-nowe_y_pred)^2)

MSE <- 1/n * sum(abs(y-y_pred))
MSE_nowe <- 1/n * sum((y-nowe_y_pred)^2)
# W obu przypadkach otrzymujemy mniejsze wartości wskaźników



# Zadanie 7
# Opłacało się przekształcić zmienną X.
# Model dopasował się lepiej do przekształconych danych


