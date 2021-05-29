#Alle nuttige R berekingen samen
## by Simon Bettens
## Version : 1.5
## gl & hf!

##bert.vanvreckem@hogent.be
##jens.buysse@hogent.be
##wim.debruyn@hogent.be
##pieterjan.maenhaut@hogent.be


## Libraries
library(tidyverse)
library(ggmosaic)
library(vcd)
library(MASS)
library(TTR)
library(forecast)
library(DescTools)

## ================================================  HSTK 2 ===========================================

#toont de titles
names(x)
#toont het een beetje
glimpse(x)
#toont de tabel
View(x)
#haalt de uniqe waarden omhoog
unique(x)
#maakt een lijst waar de kolom aan de restricities klopen
list <- x[which(x$y=="..."),]

## ================================================  HSTK 3 ===========================================

### gem
mean(x)
### mediaan
median(x)
### variantie
var(x)
### standaardafwijking
sd(x)
### kwartielen
quantile(x)
IQR(x)
### bereik
range(x)
### samenvatting
summary(x)
### plots 
hist(x,main = "...",xlab= "...",breaks = 6 )
boxplot(x,main = "...",xlab= "...",horizontal = TRUE)
### matrix  met waarde
x <- matrix(data = c(..., ..., ..., ...),
                    ncol = 2,
                    byrow = TRUE,
                    dimnames = list(c("...", "..."),
                                    c("...", "...")))

## ===============================================   HSTK 4 ===========================================

### linkerstaart kans P(X<x)
pnorm(x,mean= mu,sd = s)

### rechterstaart kans P(X>x)
1- pnorm(x,mean= mu ,sd = s)

### P(x1<X<x2) = P(z1 < Z < z2)    (standaardiseren is altijd wel goed)
pnorm(x2) - pnorm(x1)

### hoogte van de gausscurve op punt x  , kansdichtheidsfunctie
dnorm(x,mean= mu ,sd = s)

### onder welke grens 
qnorm(x,mean= mu ,sd = s)

### geneer een random normale curve met n aantal 
rnorm(n,mean= mu ,sd = s)
### steekproefstandaardafwijking
stdv <- (s/sqrt(n)) 

### z-score
z <- (x-mu)/s

### steekproefgemiddelde met lijst van waarde x van grote n
gem <- sum(x)/n

### gestandaardiseert gem  (wnr je van een pop naar steekproef gaat)
### in oefeningen is enkel (s/sqrt(n)) nodig geweest want gemiddelde mag je van een 
### populatie overnemen 
z <- (x-mu)/ (s/sqrt(n))

#### legende:
##### x : waarde
##### mu : gemmiddelde
##### s : standaardafwijking
##### n : grote steekproef

## t-verdeling
### linkerstaart kans P(X<x)
pt(x,df = n-1)

### rechterstaart kans P(X>x)
1- pt(x,df = n-1)

### hoogte van de gausscurve op punt x
dt(x,df = n-1)

### onder welke grens 
qt(x,df = n-1)

### geneer een random normale curve met n aantal 
rt(n,df = n-1)

### t verdeling (wnr je van een pop naar steekproef gaat)
t <- (x-mu)/ (s/sqrt(n))

#### legende:
##### x : waarde
##### mu : gemmiddelde
##### s : standaardafwijking
##### n : grote steekproef

### betrouwbaarheidsinterval voor fractie
n <- n              # sample size, number of minutes in januari
k <- n - 1          # number of successes
p <- k / n          # probability of success for the sample
q <- 1 - p          # probability of failure for the sample
m <- p              # mean 
s <- sqrt(p*q/n)    # stdev
g<-0.99999          # if p < g, the H0 is *not* met
pnorm(g,m,stdv)

## ===============================================   HSTK 5 ===========================================
###### mu0 is populatie gemiddelde en m is steekproef gemiddelde
### z - waarde
z <- (m - m0)/(s/sqrt(n))
qnorm(1-a)

##### a : significatie niveau
### g - waarde of x  (grenswaarde)
g <- m0 + (z *(s/sqrt(n)))

### ======================== eenzijdige rechts testen ================================================
#H0 m = x
#H1 m > x
m0 <- . #populatie gemiddelde
m <- .  #steekproef gemiddelde
s<- . # populatie standaardafwijking
n<- . # steekproefgrote
a <- . # significantieniveau
stdv <- (s/sqrt(n)) #steekproefstandaardafwijking

#### p < a   (kans < significatie niveau)
p <- 1 - pnorm(m, m0, stdv)
if(p < a) {
  print("H0 verwerpen")
} else {
  print("H0 niet verwerpen")
}
#### z>g    (oude gemiddelde > g)    (qnorm(1-a) = z - waarde)
g<-m0+qnorm(1-a)*stdv
if (m > g){
  print("H0 verwerpen")
} else {
  print("H0 niet verwerpen")
}


### ====================== eenzijdige links testen ===================================================
#H0 m = x
#H1 m < x
m0 <- . #populatie gemiddelde
m <- .  #steekproef gemiddelde
s<- . # populatie standaardafwijking
n<- . # steekproefgrote
a <- . # significantieniveau
stdv <- (s/sqrt(n)) #steekproefstandaardafwijking

#### p < a   (kans < significatie niveau)
p <- pnorm(m,m0,stdv)
if(p < a) {
  print("H0 verwerpen")
} else {
  print("H0 niet verwerpen")
}
#### z<g    (oude gemiddelde < g)
#### laat plus staan wordt een min door qnorm(a)
g<-m0+qnorm(a)*stdv
if (m < g){
  print("H0 verwerpen")
} else {
  print("H0 niet verwerpen")
}

### ================================ tweezijdig testen ===============================================
#H0 m = x
#H1 m != x
m0 <- . #populatie gemiddelde
m <- .  #steekproef gemiddelde
s<- . # populatie standaardafwijking
n<- . # steekproefgrote
a <- ./2 # significantieniveau/2
stdv <- (s/sqrt(n)) #steekproefstandaardafwijking

#### overschrijdingskans p
p <- 1 - pnorm(m, m0, stdv)
if(p < a) {
  print("H0 verwerpen")
} else {
  print("H0 niet verwerpen")
}
#### z<g    (oude gemiddelde < g)
g_1<-m0+qnorm(a)*stdv
g_2<-m0+qnorm(1-a)*stdv
if (g_1 < m & m < g_2) {
  print("H0 niet verwerpen")
} else {
  print("H0 verwerpen")
}

### ====================================== t-toets ========================================================
### voor kleinere steekproeven     (enkel n < 30)
#### p waarde
p <- 1 - pt((m - m0) / stdv, df = n - 1)

## test op de juiste test
#### g waarde
t <- qt(1-a,df = n -1 )
g <- m0 + (t *stdv)

### t.test  (verander "..." en "." door "greater" en "1-a" bij rechtzijdig 
#of "lesser" en "a" bij linkszijdig )
t.test(x, alternative = "...", mu = m0, conf.level = .)

### zelfde testen als hierboven voor g en p

## ===============================================   HSTK 6 ===========================================
### ===========================================  2 kwalitatieve =======================================
### chi-kwadraat
x <- table(...$...,...$...)
addmargins(x)
row_sums <- rowSums(x)              
col_sums <- colSums(x) 
n <- sum(x)                                # totaal hele tabel
expected <- outer(row_sums, col_sums) / n  # verwachte waarden
expected
chisq <- sum((x - expected)^2 / expected)

#### of
chisq.test(x)
#### opgelet! wanneer geen kruistabel maar een table met maar kwalitieve waarde 
#(in p zet je de kans per groep)
chisq.test(table(x),p=c(...,...,...,...))
##### controle staat hieronder !

### geobserveerde waarde 
e <- (kolomtotaal*rijtotaal)/n 

#### of
e = n * relatievefrenquetie

### vrijheidswaarden
k <- (nrow(x)-1) * (ncol(x)-1)
df<- k

#### of 
df<- n-1

### r functies in verband met χ2
pchisq(x, df) # the left-tail probability P(χ2<x)

dchisq(x, df) # the density function

qchisq(p, df) # the inverse of pchisq, “find a number x so that P(χ2<x)=p”

rchisq(n, df) # generate n random numbers for a χ2 distribution


### Kritieke grenswaarde   P(χ2 <g) = 1 -a
a <- .
l <- (length(levels(x$y1)) - 1) *
  (length(levels(x$y2)) - 1)
p <- 1- pchisq(chisq,df=l)
if(p < a){
  print("We kunnen H_0 verwerpen")
}else{
  print("We kunnen H_0 NIET verwerpen")
}

g <- qchisq(1-a,df=l)
if(chisq > g){
  print("We kunnen H_0 verwerpen")
}else{
  print("We kunnen H_0 NIET verwerpen")
}

### cramers v
k <- min(nrow(x), ncol(x))  # 3 
cramers_v <- sqrt(chisq / ((k - 1) * n))
cramers_v

#### of 
model1 <- assocstats(x)
cat("
  V = 0 geen samenhang\n
  V +- 0,1 zwakke samenhang\n
  V +- 0,25 redelijk sterke samenhang\n
  V +- 0,50 sterke samenhang\n
  V +- 0,75 zeer sterke samenhang\n
  V = 1 volledige samenhang\n
  ")

### ========================================  kwalitatief/kwantitatief =================================
### t-test  (... aanpassen naar lesser of greater)
t.test(x,y,alternative = "...", mu= m)
if(p < a){
  print("We kunnen H_0 verwerpen")
}else{
  print("We kunnen H_0 NIET verwerpen")
}

### cohan's d   (s is gecombineerde standaardafwijking)
d <- (mu1 - mu2)/s

#### of
CohenD(x)
cat("
    |d|  effect\n
    ----|------------\n
    0,01  Zeer klein\n
    0,20  Klein\n
    0,50  Middelmatig\n
    0,80  Groot\n
    1,20  Zeer groot\n
    2,00  Reusachtig
    ")
### gecombineerde standaardafwijking
s<- sqrt(((n_1 - 1)*s_1^2) + ((n_2 - 1)*s_2^2))/(n_1+n_2 -2)

### ======================================== 2 kwantitatief =======================================
### regressierechte
y<- B_1*x+B_0

### B_1
x <- c(...)
y <- c(...)
mean(y)
x_X <- x-mean(x)
y_Y <- y-mean(y)
com <- x_X * y_Y
sum(com)
x_X_2 <- x_X^2
sum(x_X_2)
B1 <-sum(com)/sum(x_X_2)

### B_0
B0 <- mean(y) -(B1*mean(x))

#### of
rechte <- lm(y ~ x)
##### (intercept) is B0
##### (x) is B1 of rico

### covariantie
cov(x)

### pearsons (pas "greater" aan indien nodig)
cor.test(x,y,method = "pearson",alternative = "greater")

### determinatiecoefficient
cor(x,y,method = "pearson")^2

### evalutatie
cat("
        |R|       R^2       effect\n
    ----------|----------|------------\n
    <0.3        <0.1      Zeer klein\n
    0.3-0.5     0.1-0.25  Klein\n
    0.5-0.7     0.25-0.5  Middelmatig\n
    0.7-0.85    0.5-0.75  Groot\n
    0.85-0.95   0.75-0.9  Zeer groot\n
    >0.95       >0.9      Reusachtig
    ")
## =============================================== HSTK 7 =================================
### SMA
SMA(x,n = k)

### EMA
x_ema <- HoltWinters(x, alpha = FALSE,beta = FALSE, gamma = FALSE, seasonal = "multiplicative", 
                     s.start = x[1])
plot(x_ema)
omzet_ema_fc <- forecast(x_ema, h = 20)
plot(x_ema_fc)
pass <- ts(x$y, frequency = n, start = c(start_year, start), end = c(end_year, end))

### decomposed
decomposed <- decompose(x,type = "multiplicative")
plot(x - decomposed$seasonal)
x - decomposed$seasonal

### functie
lm(pass ~ time(x))

### holtwinters
ema <- HoltWinters(x, alpha = NULL,beta = NULL,gamma = NULL,seasonal  = "multiplicative")
##### !!!!!! bij voorbeeld examen was het enkel dus test deze shit ook (vermeld alpha en beta en gamma niet)
ema <- HoltWinters(x)
plot(ema)
forecast(ema, h= 12)

### misschien, mogelijke waarde voor gamma
mean(decomposed$seasonal,na.rm = TRUE)


