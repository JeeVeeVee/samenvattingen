# de T-toets (voor kleinere steekproeven)
# we gaan er van uit dat sigma gekend is 

n <- 25         #steekproefgrootte
sm <- 3.483     #steekproefgemiddeld
ss <- 0.55       #standaard afwijking (gekend)
a <- 0.05       #significantieniveau (gekozen door onderzoeker)
m0 <- 3.3       #hypothetisch populatiegemiddelde (H0)

#methode 1 : KRITIEKE GRENSGEBIED

    g <- m0 + qt(1 - a, df = n- 1) * ss / sqrt(n)

# als sm groter is dan g, dan kunnen de H0 verwerpen 

#methode 2 : OVERSCHRIJDINGSKANS

p <- 1 - pt((sm - m0) / (ss/sqrt(n)), df = n - 1)

# als deze kans p kleiner is dan a (significantieniveau), dan kunnen we h0 verwerpen 

# plot van de grafiek (H0)
x <- seq(m0 - 4*ss/sqrt(n),m0 + 4*ss/sqrt(n), length = 200)
dist <- dt((x - m0) / (ss/sqrt(n)), df = n - 1) * ss/sqrt(n)
plot(x, dist, type = 'l', xlab = '', ylab='')

#tonen van het gevonden steekproefgemiddelde 
abline(v= sm, col ='red')
text(sm, 2, sm)

#plot van het aanvaardingsgebied
i <- x <= g
polygon(
    c(x[i],     g,  g), 
    c(dist[i], dt((g - m0) / (ss/sqrt(n)), df = n - 1), 0),
    col = 'lightgreen')

text(g + 0.25,.02, signif(g, digits=4))

text(m0, 0.1, m0)
abline(v=sm, col = 'red')
text(m0, 0.02, 'aanvaardingsgebied van H0')
