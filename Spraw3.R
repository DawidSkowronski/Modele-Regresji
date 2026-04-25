library(ggplot2)
library(patchwork)
library(readxl)
library(car)

dane <- as.data.frame(read_excel("regresja wielokrotna 2026.xlsx"))

n <- nrow(dane)
X <- dane[,-ncol(dane)]
Y <- dane[, ncol(dane)]

model_pelny <- lm(Y~. , data = dane)
X_prosty1 <- X[,-8]
X_prosty2 <- X_prosty1[,-8]
X_prosty3 <- X_prosty2[,-8]
model_prosty3 <- lm(Y~., data = cbind(Y=Y,X_prosty3))

# Liczba obserwacji
n <- nrow(dane)

# Ślad macierzy
p <- length(coef(model_prosty3))

# Elementy na diagonali macierzy H
lev <- hatvalues(model_prosty3)
# Odległość Cook'a
cook <- cooks.distance(model_prosty3)
# Studentyzowane rezydua
rstud <- rstudent(model_prosty3)

prog.lev <- 2*p/n
prog.cook <- 4/(n-p)
prog.rstud <- 2

indeks.lev <- which(lev>prog.lev)
indeks.cook <- which(cook>prog.cook)
indeks.rstud <- which(abs(rstud)>prog.rstud)

indeksy.do.usuniecia <- sort(unique(c(indeks.lev,
                                      indeks.cook,
                                      indeks.rstud)))
dane_nowe <- dane[-indeksy.do.usuniecia,]

X_nowe <- X_prosty3[-indeksy.do.usuniecia,]

Y_nowe <- Y[-indeksy.do.usuniecia]
Y_nowe <- as.matrix(Y_nowe)

# Nowe dane do sprawozdania 3
dane <- as.data.frame(cbind(X_nowe, Y=Y_nowe))
head(dane)
# Wykorzystać regresję krokową z kryterium AIC 
# do wyboru „najlepszego” modelu regresji M.

# Korzystamy z funkcji step

model <- lm(Y~., data = dane)
step(model)

# Model M bez X5 ma najniższą wartość AIC

dane_aic <- dane[,-5]

model_aic <- lm(Y~., data = dane_aic)

plot(model_aic)

# Estymatory MLE beta
model_aic$coefficients

summary(model_aic)


opis_modelu <- summary(model_aic)
wart_F_nowy <- opis_modelu$fstatistic
p_F_nowy <- pf(wart_F_nowy,
               opis_modelu$df[1],
               opis_modelu$df[2],
               lower.tail = FALSE)
p_wart <- p_F_nowy[1] # Bliskie 0, odrzucamy H0
# Oznacza to, że przynajmniej jedna ze zmiennych
# objaśniających ma liniowy wpływ na Y.

#1c
summary(model_aic)$coefficients[,4] # p-wartości

summary(model_aic)

# (d) Wyznaczyć przedziały ufności na poziomie 0,95 dla współczynników
# regresji odpowiadających zmiennym z modelu M.


confint(model_aic, , level = 0.95) # Bo bierze alpha/2
# Intercept ma najszerszą realizację przedziału 

# Dla zmiennych X1, X2, X3, X4, X6, X7 realizacja przedziału nie zawiera 0
# Potwierdza to wynik testu z poprzedniego punktu - zmienne te są statystycznie istotne

# Intercept zawiera 0 w realizacji przedziału, potwierdza to brak istotności 
# statystycznej wyrazu wolnego

# (e) Wyznaczyć skorygowany współczynnik determinacji R2 adj w modelu M
adj_R_kw <- summary(model_aic)$adj.r.squared



#### Zadanie 2 #####

rezydua <- residuals(model_aic)
qqnorm(rezydua)
qqline(rezydua, col = "red", lw = 2)


# (b) wykresy reszt względem każdej ze zmiennych objaśniających

zmienne <- c("X1", "X2", "X3", "X4", "X6", "X7")
par(mfrow = c(2,3))
for (zmienna in zmienne) {
  plot(dane_aic[[zmienna]], rezydua,
       main = paste("Reszty vs", zmienna),
       xlab = zmienna)
  abline(h=0, col = "red")
}
par(mfrow = c(1,1))

# Żaden z wykresów nie wskazuje na osobliwe zachowanie zmiennych
# Reszty są losowo rozproszone wokół osi y.

# (c) wykresy reszt względem wartości przewidywanych przez model.
y_predykcje <- model_aic$fitted.values 
plot(y_predykcje, rezydua)

# Chmura punktów jest losowo rozproszona, brak wzorców i trendów





