library(euler)
library(RUnit)
library(jsonlite)
library(hash)

ans <- fromJSON("../answers.json")
ans <- lapply(ans, as.numeric)

problem01_is_correct <- function() {
	checkEquals(ans$`1`, problem01())
}

problem02_is_correct <- function() {
	checkEquals(ans$`2`, problem02())
}

problem03_is_correct <- function() {
	checkEquals(ans$`3`, problem03())
}