

f <- function(x1, x2) (2 * x1 + x2)^2 - x1
Ix1 <- function(x1) {
    sapply(x1, function(a) { 
	integrate(function(x2) f(a, x2), 0, 1)$value
    })
}
integrate(Ix1, 0, 1)

