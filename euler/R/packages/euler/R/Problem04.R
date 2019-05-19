# Aaron Johnson
# 2016-12-03

# Largest palindrome product

# A palindromic number reads the same both ways. The largest palindrome
# made from the product of two 2-digit numbers is 9009 = 91 × 99.
# Find the largest palindrome made from the product of two 3-digit numbers.

isPalindrome <- function(n) {
	s <- strsplit(toString(n), NULL)[[1]]
	l <- length(s)
	check <- l %/% 2
	all(s[1:check] == s[l:(l-check+1)])
}

problem04 <- function() {
	pals <- vapply(101:999, function(v1) {
		vx <- vapply(v1:999, function(v2) v1*v2, 1)
		vxPal <- vx[vapply(vx, isPalindrome, TRUE)]
		if (length(vxPal) == 0) {
			return(0)
		}
		max(vxPal)
	}, 1)

	max(pals)
}