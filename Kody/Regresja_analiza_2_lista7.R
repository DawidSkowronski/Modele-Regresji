library(ggplot2)
library(readxl)
library(ppcor)

dane <- read_excel('lab 7 partial vs scatter.xlsx')

head(dane)

korelacja <- cor(dane)
korelacja_czastkowa <- pcor(dane)

korelacja
korelacja_czastkowa$estimate # Wartości zdecydowanie bliższe 1 lub -1

pairs(dane) # Widoczna silna dodatnia korelacja liniowa między X1 i X2
# Dla pozostałych nie widać wyraźnej korelacji

library(car)
model <- lm(y ~ X1 + X2 + X3, data = dane)
avPlots(model, main="Wykresy regresji cząstkowej")

# Korelacja Y i X1
# Warunkujemy poprzez X2 + X3
m.y.23 <- lm(y~X2+X3, data = dane)
m.x1.23 <- lm(X1~X2+X3, data = dane)
r.23 <- residuals(m.y.23)
r.x1.23 <- residuals(m.x1.23)

# Korelacja Y i X2
# Warunkujemy poprzez X1 + X3
m.y.13 <- lm(y~X1+X3, data = dane)
m.x2.13 <- lm(X2~X1+X3, data = dane)
r.13 <- residuals(m.y.13)
r.x2.13 <- residuals(m.x2.13)

# Korelacja Y i X3
# Warunkujemy poprzez X1 + X2
m.y.12 <- lm(y~X1+X2, data = dane)
m.x3.12 <- lm(X3~X1+X2, data = dane)
r.12 <- residuals(m.y.12)
r.x3.12 <- residuals(m.x3.12)


kor_cz.y.x1 <- cor(r.23,r.x1.23)
kor_cz.y.x2 <- cor(r.13,r.x2.13)
kor_cz.y.x3 <- cor(r.12,r.x3.12)

plot(x = r.x1.23, 
     y = r.23,
     main = "Y vs X1 | X2 + X3",
     xlab = "Reszty X1",
     ylab = "Reszty Y)",
     pch = 19,
     col = "deepskyblue")

plot(x = r.x2.13, 
     y = r.13,
     main = "Y vs X2 | X1 + X3",
     xlab = "Reszty X2",
     ylab = "Reszty Y)",
     pch = 19,
     col = "deepskyblue")

plot(x = r.x3.12, 
     y = r.12,
     main = "Y vs X3 | X1 + X2",
     xlab = "Reszty X3",
     ylab = "Reszty Y)",
     pch = 19,
     col = "deepskyblue")

# Widoczna jest silna liniowa korelacja między resztami Y a Xi
# Czyli każda ze zmiennych może być użyteczna


