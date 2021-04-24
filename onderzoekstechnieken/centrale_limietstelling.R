m <- 1000
s <- 50
n <- 50
p <- 0.5
x <- 0.5
df <- 10
#linkerstaartkans, P(X < x)
pnorm(x, m, s)

#hoogte van ge Gausscurve op x
dnorm(x, m, s)

#onder welke grens zal p% van de waarnemingen liggen?
qnorm(p, m, s)

#genereer n getallen die normaal zijn verdeeld
rnorm(n, m, s)


#QQ plot 


#for normal
observations <- rnorm(n, m, s)

qqnorm(observations)

#plotting the line of the expected position of the observations
x <- seq(-3, +3, length = n)
lines(x, m+s*x, col = 'red')

# voor kleine steekproeven (< 30): df staat voor het aantal vrijheidsgraden
pt(x, df) #linkerstaart kans
dt(x, df) #hoogte van de curve op punt x
#qt(x, df) #onder welke grens zal p% van de waarnemingen liggen, geeft errors 
rt(n, df) #genereer n random getallen volgens deze verdeling

#algemene prefixen : 
#d = de hoogte van de respectievelijke kansdichtheidsfunctie
#p = cumulatieve kansdichtheidsfunctie
#q = geeft de omgekeerde cumulatieve dichtheidsfunctie
#r = willekeurige waarde

gausplot <- function(m = 0, s = 1) {
    x <- seq(m-4*s, m + 4*s, length.out = 101)
    y <- dnorm(x, mean = m, sd = s)
    plot(x, y, type = 'l')
}

gausplot()