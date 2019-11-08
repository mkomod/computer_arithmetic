
require(testthat)

source("midpoint.r")


#
# MIDPOINT INTEGRATION 
#

test_that("Midpoint integral of f(x) = 2x from 0, 5 is 25", {
    f <- function(x) {
	 2 * x
    }
    val <- (midpoint(f, 0, 5, 100))$value
    expect_equal(val, 25)
})


