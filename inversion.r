#
# Inversion method for generating non uniform random samples
#


inversion <- function(cdf, lwr, upr, n) {
    #
    # Generate random samples from a cdf using the inversion method
    #
    # Args:
    #	cdf: (function) the cdf of the random varaible
    #   lwr, upr: (float) bounds of the function
    #   n: (int) numer of samples
    #
    func <- function(x) { cdf(x) - prob }
    samples <- c()
    for (i in 1:n) {
	prob <- runif(1)
	samples <- c(samples, uniroot(f=func, c(lwr, upr))$root)
    }
    samples
}
