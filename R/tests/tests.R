library(euler)
library(RUnit)

problem01_is_correct <- function() {
	checkEquals(233168, problem01())
}

problem02_is_correct <- function() {
	checkEquals(4613732, problem02())
}