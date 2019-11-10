
require(testthat)

source("inversion.r")
source("acceptreject.r")
source("boxmuller.r")
source("mcint.r")
source("impsampling.r")
source("midpoint.r")

#
# Inversion Method
#

test_that("Inversion method generates random samples", {
    set.seed(12345)
    cdf <- function(x) (1 - exp(-x))/(1-exp(-3)) 
    obs <- inversion(cdf, 0, 3, 3)
    expect_equal(round(obs, 6), c(1.155220, 1.784797, 1.28406))
})


#
# Accept Reject
#

test_that("Accept reject method, correct value of C evaluated", {
    f <- function(x) 1 / sqrt( 2 * pi ) * exp(-(x^2)/2)
    g <- function(x) 1/2 * exp(-abs(x))
    Cval <- ar_getC(f, g)
    expect_equal(round(Cval, 3), 1.315) # to 3 dp
}) 


test_that("Accept reject method, correct value of C evaluated", {
    set.seed(4321)
    f <- function(x) 1 / sqrt( 2 * pi ) * exp(-(x^2)/2)
    g <- function(x) 1/2 * exp(-abs(x))
    expect_equal(round(rpdf(f, g, -100, 100, 3), 7), 
		 c(-0.1947240, 0.7486541, 0.9160027))
}) 

#
# Box-muller
#

test_that("Box Muller method generates standard norm samples", {
    set.seed(5642)
    smp <- boxmuller(3)
    expect_equal(round(smp, 8), 
		 c(0.03757714, -1.71699944, -0.11660633))
}) 


#
# Monte Carlo Integration
#

test_that("Monte Carlo integration", {
    set.seed(6892)
    mc <- mcint(function(x) x^2 - 5 * x + sin(x), rexp(10000, 1/2))
    expect_equal(round(mc$estimate, 6), -1.500528)
    expect_equal(round(mc$interval, 6), c(-1.708699, -1.292357))
}) 


#
# Importance Sampling
#

test_that("Importance sampling", {
    set.seed(8575)
    est <- impsamp(function(x) x > 4, 
	    function(x) dnorm(x), 
	    function(x) dexp(x-4), 
	    4 + rexp(10^4))$estimate
    expect_equal(round(est, 8), 3.176635e-05)
})


#
# Midpoint Integration
#

test_that("Midpoint integral of f(x) = 2x from 0, 5 is 25", {
    f <- function(x) {
	 2 * x
    }
    val <- (midpoint(f, 0, 5, 100))$value
    expect_equal(val, 25)
})












