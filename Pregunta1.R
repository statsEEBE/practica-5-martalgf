#Distribución poblacional

mu <- 95.3
sigma <- 5.7
#N distribuye N(mu, sigma^2)
curve(dnorm(x, mean=mu, sd=sigma),xlim=c(80,120),col="red")

#a)
Y <- function(i){sum(rnorm(4, mean=mu, sd=sigma))}
Y(1)
Y100000 <- sapply(1:100000, Y)  
mean(Y100000)
hist(Y100000, freq=FALSE)
curve(dnorm(x, 4*mu, 2*sigma), col="red", add=TRUE)

#Respuesta
4*mu

#b) Varianza
Y <- function(i){sum(rnorm(4, mean=mu, sd=sigma))}
Y(1)
Y100000 <- sapply(1:100000, Y)  
var(Y100000)

#respuesta
100*sigma^2

#c)
curve(dorm(x, mean=mu, sd=sigma), xlim=c(80, 120), col="red")
1-pnorm(103, mu, sigma)

#d) 
Xbar <- function(i){mean(rnorm(4, mean=mu, sd=sigma))}
Xbar100000 <- sapply(1:100000, Xbar)

hist(Xbar100000, freq=FALSE)
mean(Xbar100000<98)
curve(dnorm(x, mu, sigma/sqrt(4)), add=TRUE, col="red")
pnorm(98, mu, sigma/sqrt(4))

#e) 
Ssq <- function(i){var(rnorm(100, mean=mu, sd=sigma))}
Ssq100000 <- sapply(1:100000, Ssq)
hist(Ssq100000, freq=FALSE)
mean(Ssq100000>32)
hist(Ssq100000*(100-1)/sigma^2)
curve(dchisq(x, 100-1), add=TRUE, col="red")

w <- 32*(100-1)/sigma^2
w
1-pchisq(w, 100-1)
mean(Ssq100000>32)

#Apuntes
#Y =suma muestral -> en R: sum
#Xbar = media muestra  -> en R: mean
#S^2 = Varianza muestra  -> en R: var