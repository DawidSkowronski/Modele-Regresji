library(ggplot2)
library(kableExtra)
library(readxl)
library(GGally)

dane <- as.data.frame(read_excel("regresja wielokrotna 2026.xlsx"))
head(dane)

# Dla każdej z par zmiennych (Y, Xi ), (Xi, Xj ) wykonaj wykres rozrzutu*.
# Przeanalizuj otrzymane rysunki i odpowiedz na następujące pytania:

pairs(dane)

ggpairs(dane)

# Wyznaczamy osobno X i Y
X <- dane[,-ncol(dane)]
head(X)

Y <- dane[, ncol(dane)]
head(Y)

pairs(X)


par(mfrow = c(2, 5))
for (i in 1:ncol(X)) {
  plot( X[[i]],Y, xlab = names(X)[i], ylab = "Y")
}
par(mfrow = c(1, 1))


mac_korelacji <- cor(dane)
round(mac_korelacji,3)

library(corrplot)
corrplot(mac_korelacji, method = "number", type = "upper")



#### 3. Skonstruuj model regresji liniowej opisujący zależność ####
# między zmienną Y a zmiennymi objaśniającymi X1 , . . . , Xp−1.

# Wyznacz estymator najmniejszych kwadratów
X_rozsz <- as.matrix(cbind( rep(1,nrow(X)) , X))
Y_mac <- as.matrix(Y)
H <- X_rozsz %*% solve(t(X_rozsz) %*% X_rozsz) %*%t(X_rozsz)

beta_est <- solve(t(X_rozsz)%*%X_rozsz)%*%t(X_rozsz)%*%Y

# Można też za pomocą funkcji lm()
model_pelny <- lm(Y~. , data = dane)
coef(model_pelny)
summary(model_pelny)

# test F
anova(model_pelny)

# (b) Czy którakolwiek ze zmiennych objaśniających z tego (pełnego)
# modelu ma liniowy wpływ na zmienną objaśnianą?
wart_F <- summary(model_pelny)$fstatistic
p_val <- pf(wart_F[1], wart_F[2], wart_F[3], lower.tail = FALSE)
# p-value jest mniejsze od jakiegokolwiek sensownego poziomu istotności, 
# zatem odrzucamy H0. Czyli mamy H1, tzn. co najmniej jedna ze zmiennych 
# objaśniających ma linowy wpływ na Y

# R^2 i skorygowany R^2
R_kw <- summary(model_pelny)$r.squared
adj_R_kw <- summary(model_pelny)$adj.r.squared



# 4. Rozwiąż problem współliniowości.
library(car)
vif(model_pelny)
which.max(vif(model_pelny)) # X8 ma najwyższą wartość VIF

head(X)

X_prosty1 <- X[,-8]
head(X_prosty1)

model_prosty1 <- lm(Y~., data = cbind(Y=Y,X_prosty1))

vif(model_prosty1)
which.max(vif(model_prosty1)) # X9 najwyższa wartość powyżej 10

# Kolejne uproszczenie
X_prosty2 <- X_prosty1[,-8]
head(X_prosty2)

model_prosty2 <- lm(Y~., data = cbind(Y=Y,X_prosty2))
vif(model_prosty2)
which.max(vif(model_prosty2)) # X10 najwyższa wartość powyżej 10

# Kolejne uproszczenie
X_prosty3 <- X_prosty2[,-8]
head(X_prosty3)

model_prosty3 <- lm(Y~., data = cbind(Y=Y,X_prosty3))
vif(model_prosty3)
# Już żaden VIF nie jest większy od 10

# Czyli mamy prostszy model bez X8, X9, X10


#5. Zidentyfikuj i ewentualnie usuń z próby obserwacje, które mogą być wpływowe.

# a) wpływy
X_rozsz <- as.matrix(cbind( rep(1,nrow(X)) , X))
Y_mac <- as.matrix(Y)
H <- X_rozsz %*% solve(t(X_rozsz) %*% X_rozsz) %*%t(X_rozsz)
dim(H)

# Chcemy to policzyć dla uproszczonego modelu
X_rozsz <- as.matrix(cbind( rep(1,nrow(X_prosty3)) , X_prosty3))
Y_mac <- as.matrix(Y)
H <- X_rozsz %*% solve(t(X_rozsz) %*% X_rozsz) %*%t(X_rozsz)
dim(H)

# Liczba obserwacji
n <- nrow(H)

# elementy na diagonali macierzy H
dzwignie <- diag(H)

# Ślad macierzy
p <- sum(diag(H)) #powinno się równać liczbie zmiennych objaśniających

obs_odstajace <- dzwignie >= 3*p/nrow(H)
sum(obs_odstajace) # na podstawie samych dźwigni nie ma obs. odstajacych

# Studentyzowane rezydua
stud_rez <- rstudent(model_prosty3)

# Odległość Cooka
odl_Cook <- cooks.distance(model_prosty3)


# Ramka danych wszystkich obserwacji hii, ri, Di

obs_wplywowe <- data.frame( obserwacja = 1:nrow(H),
                            hii = round(dzwignie,4),
                            ri = round(stud_rez,4),
                            Di = round(odl_Cook,4)
                            )
head(obs_wplywowe)

# Prog odciecia
prog_odciecia <- 4/(n-p)

# Sprawdzamy które obserwacje są wpływowe, tzn. Di >= 4/(n-p)
which(obs_wplywowe$Di >= prog_odciecia)

obs_do_usuniecia <- which(obs_wplywowe$Di >= prog_odciecia)
# Obserwacje o tych indeksach są wpływowe
# 5  31  43  51  71 104 129 147 154 167 195

# Wykres rozproszenia dla (i,Di)
plot(obs_wplywowe$obserwacja, obs_wplywowe$Di)
abline(h = prog_odciecia, col = "red")
points(obs_wplywowe$obserwacja[obs_do_usuniecia], 
       obs_wplywowe$Di[obs_do_usuniecia], 
       col = "blue", pch = 19)

# Zatem 11 obserwacji jest odstających


# 6. Wykorzystując zmienne i obserwacje, które nie zostały usunięte, ponownie
# zbuduj model regresji liniowej opisujący zależność między zmienną
# Y a zmiennymi objaśniającymi

X_nowe <- X_prosty3[-obs_do_usuniecia,]
dim(X_nowe)

Y_nowe <- Y[-obs_do_usuniecia]
Y_nowe <- as.matrix(Y_nowe)
dim(Y_nowe)

model_nowy <- lm(Y_nowe~., data = cbind(Y_nowe = Y_nowe,X_nowe))

nowe_est_beta <- coefficients(model_nowy)


# test F
anova(model_nowy)

# (b) Czy którakolwiek ze zmiennych objaśniających z tego (pełnego)
# modelu ma liniowy wpływ na zmienną objaśnianą?
wart_F_nowy <- summary(model_nowy)$fstatistic
p_val_nowe <- pf(wart_F_nowy[1], wart_F_nowy[2], wart_F_nowy[3],
                 lower.tail = FALSE)

anova(model_nowy)

# p-value jest mniejsze od jakiegokolwiek sensownego poziomu istotności, 
# zatem odrzucamy H0. Czyli mamy H1, tzn. co najmniej jedna ze zmiennych 
# objaśniających ma linowy wpływ na Y

# R^2 i skorygowany R^2
R_kw_nowe <- summary(model_nowy)$r.squared
adj_R_kw_nowe <- summary(model_nowy)$adj.r.squared




