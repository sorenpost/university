n <- 1:100
fail_rate <- 0.05
z <- 1 - fail_rate

# likelihood of at least one failue
plot(n, 1 - z^n)
