
#####  Generowanie zbioru danych do regresji logistycznej ######
# Parametry
n <- 300
p <- 20 # liczba zm. objaśniających
# X_1 jest stale równa 1

# Tworzymy wektor beta - "nieznane" współczynniki estymowane na podstawie próby
set.seed(2137)

beta <- c(sample((1:8)/2, size = 10, replace = TRUE),rep(0,10))


X <- matrix(0, nrow = n, ncol = p)
Y <- numeric(n)
# Dla każdego rekordu w danych

for (i in 1:n){
  x <- c(1,rnorm(p-1,mean = 0, sd = 1))

pi_x <- 1/(1+ exp(-sum(beta*x)))
pi_x

X[i,] <- x

Y[i] <- sample(c(1,0), size=1, prob=c(pi_x, 1-pi_x))
  
}

data <- data.frame(y=Y, x=X)
colnames(dane) <- c("y",paste0("x",1:p))

head(data,2)

# Zapisanie danych do pliku
write.csv(data, file = "reg_log_dane.csv", row.names = FALSE)

#### Sprawozdanie ####

dane <- read.csv("reg_log_dane.csv")
head(dane, 2)

#### Zadanie 1 #####

# (a) Podzielić losowo zbiór danych w proporcji 70 : 30 na zbiór
# treningowy i zbiór testowy, dbając o to, aby frakcja (odsetek) rekordów
# ze zmienną y = 1 w obu zbiorach była zbliżona.

# Mamy 300 wierszy
# zb treningowy 210
# zb testowy 90


# dzielimy na dane zawierające y=1 i y=0
dane.y0 <- dane[dane$y==0,]
dane.y1 <- dane[dane$y==1,]

ind_y_1 <- which(dane[,1]==1)
ind_y_0 <- which(dane[,1]==0)



id.trening.y1 <- sample(1:nrow(dane.y1), size = 0.7 * nrow(dane.y1))
id.trening.y0 <- sample(1:nrow(dane.y0), size = 0.7 * nrow(dane.y0))

length(id.trening.y1) + length(id.trening.y0)

# Zbiór uczący
train <- rbind(dane.y1[id.trening.y1,], dane.y0[id.trening.y0,])

# Zbiór testowy
test <- rbind(dane.y1[-id.trening.y1,], dane.y0[-id.trening.y0,])


# (b) Wykorzystując zbiór treningowy, skonstruować model regresji
# logistycznej opisujący zależność między zmienną objaśnianą Y a
# wszystkimi zmiennymi objaśniającymi X1, . . . , Xp.
  
model <- glm(y ~ .-1, data = train, family = binomial(link = "logit"))
# Usuwamy intercept

summary(model)

# (c) Za pomocą testu ilorazu wiarygodności zweryfikować hipotezę zerową
# H0 : β2 = . . . = βp = 0,
# która stwierdza, że żadna ze zmiennych objaśniających X2, . . . , Xp
# nie wpływa na zmienną objaśnianą Y . Wyznaczyć p-wartość testu
# oraz rozstrzygnąć, czy na poziomie istotności α = 0.05 należy
# odrzucić H0.

# Tworzymy dwa zagnieżdżone modele
# Model prosty
model.prosty <- glm(y ~ x.1-1, data = train, family = binomial(link = "logit"))
summary(model.prosty)

# Model pełny, utworzony w podpunkcie (b)

test_IW <- anova(model.prosty, model, test = 'Chisq')

# p-value
test_IW$`Pr(>Chi)`[2]
# Bardzo bliska 0, odrzucamy H0 na dowolnym rozsądnym poziomie istotności alpha



# (d) Dla każdego z parametrów βi
# , i = 2, . . . , p:
#   i. wyznaczyć estymatory βbi oraz ich błędy standardowe SEβi
# ,
# ii. skonstruować przedziały ufności na poziomie 1 − α,
# iii. zweryfikować hipotezę H0 : βi = 0 przeciwko H1 : βi ̸= 0 na
# poziomie istotności α.


# I
# Liczymy od beta_2
est_beta <- coefficients(model)
SE_beta <- summary(model)$coefficients[,2]

z <- qnorm(1-0.05/2, mean = 0 , sd = 1)
# II
lower <- est_beta-SE_beta*z
upper <- est_beta+SE_beta*z

# Przedziały Walda
confint.default(model, level = 1-0.05)
# III

# Jeśli 0 zawiera się w realizacji przedziału ufności to nie
# mamy podstaw do odrzucenia H0: beta_i = 0

# Powinniśmy odrzucić H0 dla i = 3,4,5,6,8,9,10,16


# (e) Porównać wektor parametrów β = (β1, . . . , βp) z jego estymatorem βb = (βb1, . . . , βbp). W tym celu sprawdzić, czy estymator βb
# poprawnie zidentyfikował wszystkie niezerowe elementy wektora
# β oraz czy prawidłowo określił ich znaki.

# estymowane
est_beta
beta

# Model słabo sobie poradził z identyfikacją niezerowych elementów beta.
# Najlepsze wyniki dla 1, 2, 4,6, 10
# Dla niezerowych elementów wektora beta znaki są poprawnie zidentyfikowane, 

# Jednak dla współrzędnych, które są zerami model przyjął fałszywe zależności?


# 2. Zinterpretować dwa niezerowe współczynniki estymatora βb w kategoriach
# ilorazu szans (odds ratio).

odds_ratio <- exp(est_beta)
odds_ratio


#### zadanie 3 #####

# 3. Dla dwóch poziomów odcięcia π0 = 0.5 oraz π0 = 0.7 wykorzystać
# zbiór testowy do skonstruowania classification table. Wyznaczyć miary
# zdolności predykcyjnej modelu: sensitivity, specificity, false positive rate,
# false negative rate, overall proportion of correct classifications.



est_pi <- numeric(nrow(test))

for (i in 1:nrow(test)){
  est_pi[i] <- (1/(1+ exp(-sum(test[i,-1]*est_beta))) )
}

est_pi # Wyniki zgadzają się z predict

# response aby zwracało z przedziału [0,1]
predict(model, test, type = "response")


prog_y_0.5 <- ifelse(est_pi >= 0.5, 1 , 0)
prog_y_0.7 <- ifelse(est_pi >= 0.7, 1 , 0)


class_table_0.5 <- table(prognozowane=prog_y_0.5, rzeczywiste=test$y)
class_table_0.5

class_table_0.7 <- table(prognozowane=prog_y_0.7, rzeczywiste=test$y)
class_table_0.7

miary_zdolnosci_pred <- function(tabela){

TP <- tabela["1","1"]
TN <- tabela["0","0"]
FP <- tabela["1","0"]
FN <- tabela["0","1"]

miary <- c(
  sensitivity  = TP/(TP+FN),
  specificity  = TN/(TN+FP),
  FPR = FP / (FP + TN),
  FNR = FN / (FN + TP),
  accuracy  = (TP + TN) / (TP + TN + FP + FN)
)
return(miary)
}

miary_zdolnosci_pred(class_table_0.5)
miary_zdolnosci_pred(class_table_0.7)


# 4. Wykorzystać zbiór testowy do narysowania krzywej ROC. Wyznaczyć
# pole pod krzywą ROC (AUC) i zinterpretować jego wartość w kontekście 
# wartości predykcyjnej modelu.

library(pROC)
library(ggplot2)

wyznacz_roc <- function(prawd, y, liczba_pkt=1000) {
  
  siatka <- seq(from=0.000001, to = 0.9999999, length.out =liczba_pkt)
  
  wynik <- data.frame()
  
  for (i in siatka){
    
    prog_y <- ifelse(prawd>=i,1,0)
  
    class_table <- table(prognozowane=prog_y, rzeczywiste=y)
    
    miary <- miary_zdolnosci_pred(class_table)
    sensiv <- miary[1]
    spec <- miary[2] 
    wynik <- rbind(wynik, data.frame(x_roc=1-spec,y_roc=sensiv))
  }
  
  
  return(wynik)
}


krzywa_roc <- wyznacz_roc(est_pi,test$y)

ggplot(krzywa_roc)+ geom_step(aes(x=x_roc,y=y_roc))


#wyższe prawdopodobieństwa oznaczają przewidywanie klasy pozytywnej (1).

krzywa_roc <- roc(response = test$y, 
                  predictor = est_pi, 
                  direction = "<", 
                  quiet = TRUE)

plot(krzywa_roc, 
     main = "Krzywa ROC dla modelu na zbiorze testowym", 
     col = "blue", 
     lwd = 2, 
     print.auc = TRUE,
     grid = TRUE)

# Wartość auc
wart_auc <- auc(krzywa_roc)
wart_auc
