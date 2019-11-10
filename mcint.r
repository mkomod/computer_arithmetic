#
# Monte Carlo Integration
#

mcint <- function(f, samples, a=0.05) {
    #
    # Monte Carlo integration using some function f and samples
    # 
    # Args:
    #	f: (function) function to integrate over
    #   samples: (numeric) vector of random samples
    #   [a]: (float) 1-a confidence interval
    #
    # Returns:
    #   (list) containing monte carlo estimate, confidence interval of estimate
    # 
    n <- length(samples)
    fsamp <- f(samples)

    mumc <- mean(fsamp)
    conf <- mumc + (sd(fsamp) / sqrt(n)) * qnorm(c(a/2, 1 - a/2))

    list(estimate = mumc, interval=conf)
}


