controle <- c(91,87,99,77,88,91)
interventie <- c(101, 110, 103, 93, 99, 104)
t.test(controle, interventie, alternative = "less", mu = 0)