#de z-toets

# voor de steekproef hebben we ; 
n <- 30         #steekproefgrootte
sm <- 3.483     #steekproefgemiddeld
s <- 0.55       #standaard afwijkin (gekend)
a <- 0.05       #significantieniveau (gekozen door onderzoeker)
m0 <- 3.3       #hypothetisch populatiegemiddelde (H0)


#methode 1
#bereken de kans dat in een steekproef het gegeven steekproefgemiddelde ziet, dus P(M > sm), in een verdeling M ~ Nor(m0, s / sqrt(n))
p <- 1 - pnorm(sm, mean = 3.3, s/sqrt(n)) # => 0.0341...

#als deze kans kleiner is dan het significantieniveau, dan kunnen we de nulhypothese verwerpen

#methode 2
#berekenen van het kritieke grensgebied

g <- m0 + qnorm(1 - a) * s /sqrt(n)

#g is dan de laagste waarde waarvoor je H0 kan verwerpen, is sm groter, dan mag je dus H0 verwerpen.


# plot van de grafiek (H0)
x <- seq(m0 - 4*s/sqrt(n),m0 + 4*s/sqrt(n), length = 200)
dist <- dnorm(x, m0, s/sqrt(n))
plot(x, dist, type = 'l', xlab = '', ylab='')

#tonen van het gevonden steekproefgemiddelde 
abline(v= sm, col ='red')
text(sm, 2, sm)

#plot van het aanvaardingsgebied
i <- x <= g
polygon(
    c(x[i],     g,  g), 
    c(dist[i], dnorm(g, m0, s/sqrt(n)), 0),
    col = 'lightgreen')

text(g,.5, signif(g, digits=4))

text(m0, 0.1, m0)
abline(v=m0)
text(m0, 1.5, 'aanvaardingsgebied van H0')
