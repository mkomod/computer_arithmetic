
boxmuller <- function(n=10) {
    #
    # Generates samples from a standard normal distribution using
    # Box-Muller method
    #
    # Args: 
    #	[n]: (int) number of samples to generate
    #
    # Returns:
    #	(numeric) vector of samples
    #
    sqrt(rexp(n, 1/2)) * cos(runif(n, 0, 2*pi))
}

