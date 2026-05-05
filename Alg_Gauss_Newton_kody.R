library(ggplot2)
library(MASS)


# Parametry
n <- 10
x <- (1:10)/2
b1 <- 10
b2 <- 3
b3 <- 2
beta <- c(b1,b2,b3)
sig <- 1/2

set.seed(1)
y_gen <- (b1*x)/(b2+x)+b3 + rnorm(10,mean = 0, sd = sig)


fun_g <- function(x,beta){
  return((beta[1] * x) / (beta[2] + x) + beta[3])
}

grad_g <- function(x, beta){
  return(c(x/(beta[2]+x), - (beta[1] *x)/(beta[2]+x)^2,1))
}

grad_g(1/2, beta= beta)

# Wzór na g(x,beta)

# Definiujemy macierz G

mac_G <- function(wek_x, beta){
  n <- length(wek_x)
  p <- length(beta)
  mac <- matrix(nrow = n, ncol = p)
  for (i in 1:n){
    mac[i, ] <- grad_g(wek_x[i], beta = beta)
  }
  return(mac)
}


G_N <- function(x,y, beta_0 = c(8,2,1), eps=0.0001 , M=100){
  
  p <- length(beta_0)
  
  stare_beta <- beta_0
  for (i in 1:M){
    
    y_prog <- fun_g(x, stare_beta)
    G <- mac_G(x, beta = stare_beta)
      
    krok <- ginv(t(G) %*% G) %*%
      t(G)%*%(y-y_prog)
    
    nowe_beta <- stare_beta + krok

    # Warunek stopu
    if (sqrt(sum((nowe_beta-stare_beta)^2))<eps){
      break
    }
    stare_beta <- nowe_beta
  }
  
  # Oszacowanie wariancji
  y_koniec <- fun_g(x, nowe_beta)
  RSS <- sum((y - y_koniec)^2)
  sig_kw <- RSS / (n - p)
  
  # Oszacowanie macierzy kowariancji
  mac_cov <- sig_kw * ginv(t(G)%*%G)
  
  return(list(beta_est = nowe_beta, sigma2_est = sig_kw, iteracje = i, macierz_cov = mac_cov))
}

G_N_rozw <- G_N(x,y_gen)
beta

## Zadanie 4
# Prezentacja w tabeli kroków 0,1,2 i ostatniego

G_N_tab <- function(x,y, beta_0 = c(8,2,1), eps=0.0001 , M=100){
  
  n <- length(x)
  p <- length(beta_0)
  
  historia <- data.frame( )
  
  stare_beta <- beta_0
  for (i in 1:M){
    
    y_prog <- fun_g(x, stare_beta)
    G <- mac_G(x, beta = stare_beta)
    
    
    krok <- ginv(t(G) %*% G) %*%
      t(G)%*%(y-y_prog)
    
    
    RSS <- sum((y-y_prog)^2)
    L_log <- -n/2*log(2*pi) - n/2 * log(RSS/(n-p)) - (n-p)/2
    
    historia <- rbind(historia, data.frame( krok = i,
                                            beta1 = stare_beta[1],
                                            beta2 = stare_beta[2],
                                            beta3 = stare_beta[3],
                                            sigma_kw = RSS/(n-p),
                                            L = L_log))
    
    nowe_beta <- stare_beta + krok
    
    # Warunek stopu
    if (sqrt(sum((nowe_beta-stare_beta)^2))<eps){
      break
    }
    stare_beta <- nowe_beta
  }
  
  # Oszacowanie wariancji
  y_koniec <- fun_g(x, nowe_beta)
  RSS <- sum((y - y_koniec)^2)
  sig_kw <- RSS / (n - p)
  
  return(list(beta_est = nowe_beta, sigma2_est = sig_kw, iteracje = i, historia))
}

# Algorytm z historią
G_N_tab(x,y_gen)



# Zadanie 6
y_pred <- fun_g(x, G_N_rozw$beta_est)
  
plot(x,y_gen)
plot(x,y_pred)

ggplot()+
  geom_point(aes(x=x,y=y_gen), col = "black")+
  geom_line(aes(x=x,y=y_gen), col = "gray")+
  geom_point(aes(x=x,y=y_pred), col = "red1") +
  geom_line(aes(x=x,y=y_pred), col = "tomato1") +
  theme_minimal()
  

G_N(x,y_gen,beta_0 = c(9.7,2.9,2))
G_N(x,y_gen,beta_0 = c(1,1,1)) # Nie da się wyznaczyć macierzy numerycznie (jest osobliwa)
