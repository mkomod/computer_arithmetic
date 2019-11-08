

midpoint <- function(func, a, b, n=1000) {
    # Midpoint algorithm for determining the integral of a
    # 1 - Dimensional function
    #
    # Args:
    #   func: (function) the function you want to integrate
    #   a, b: (float) the start and endpoints of the itegral
    #   n: (int) the number of midpoints to use
    #
    # Returns:
    #   (float) the integral of the funciton.
    #
    if (a >= b) {
	stop("starting value larger than end")
    } 
    if (!all(is.finite(c(a, b, n)))) {
	stop("Please provide finite values for a, b, n")
    }
    if (n < 1) {
	stop("Please provide a valid number of midpoints")
    }
    dif <- 1/(2*n)
    value <- mean(func(seq(a + dif, b - dif, by=1/n))) * (b - a)

    # TODO: finish error implementation
    # error <- (optim(par=a, fn=func, method="Brent", lower=a, upper=b, hessian=TRUE))$hessian / (24 * n ^ 2) * (b -a)^2
    list(value = value)
}


