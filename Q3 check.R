samplek4 <- replicate(10000, abs(rnorm(4)))
k4mean <- apply(samplek4, 2, mean)
k4min <- apply(samplek4, 2, min)

samplek100 <- replicate(10000, abs(rnorm(100)))
k100mean <- apply(samplek100, 2, mean)
k100min <- apply(samplek100, 2, min)

sqrt(var(k4mean-k4min)/10000)
sqrt(var(k100mean-k100min)/10000)
