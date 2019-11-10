#
# Importance Sampling
#


impsamp <- function(phi, f, g, samples, a=0.05) {

    n <- length(samples)
    fsamp <- phi(samples) * (f(samples) / g(samples))

    muis <- mean(fsamp)
    sdev <- sd(fsamp) 
    estimators.sd <- sdev / sqrt(n) 
    conf <- muis + estimators.sd * qnorm(c(a/2, 1 - a/2))

    list(estimate = muis, estimate.sd=sdev, conf.interval=conf, stnd.err=estimators.sd)

}


