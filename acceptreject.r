source("inversion.r")

ar_getC <- function(f, g, domain=seq(-100, 100, length.out=1000000)) {
    # Gets the value of C for which the accept reject method is valid
    #
    # Args:
    #   f, g: (function) density functions
    #   [domain]: (numeric) the domain of functions f, g
    #
    # Returns:
    #   (numeric) Value of C
    #
    max( f(domain) / g(domain) )
}


rpdf <- function(f, g, a, b, n) {
    #
    # Accept reject method to generate random sample from a pdf
    # Automatically finds the smallest value C, and generates samples
    # from the pdf g via inversion
    #
    # Args:
    #   f, g: (function) pdfs
    #   a, b: (numeric) lower and upper bounds of pdfs
    #   n: (int) number of samples
    #
    # Returns:
    #   (numeric) vector of random samples of rv given by f
    # 

    Cval <- ceiling( 100 * ar_getC(f, g, seq(a, b, length.out=1000000)) ) / 100
    G <- function(x) integrate(g, a, x)$value # CDF of G 

    rsamp <- c()
    while (length(rsamp) != n) {
	rg <- inversion(G, a, b, 1) # RV from G using inversion
	if (runif(1) <= f(rg) / (Cval * g(rg))) {
	    rsamp <- c(rsamp, rg)
	}
    }
    rsamp
}


