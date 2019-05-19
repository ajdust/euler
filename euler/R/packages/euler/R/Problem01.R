# Multiples of 3 and 5
#
# If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6
# and 9. The sum of these multiples is 23. Find the sum of all the multiples of 3 or 5
# below 1000.

problem01 <- function () {
	v <- 1:999
	# careful! note here how R uses `|` for the logical vector OR
	# this is distinct from `||` (typical short-circuit OR, which R also has)
	sum(v[v %% 3 == 0 | v %% 5 == 0])
}