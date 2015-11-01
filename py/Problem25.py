# Author: Aaron Johnson
# Date:   2015-02-22
#
# Solves Euler Problem 25:
# 1000-digit Fibonacci number
#
# The Fibonacci sequence is defined by the recurrence relation:
# F_n = F_n−1 + F_n−2, where F_1 = 1 and F_2 = 1.
# What is the first term in the Fibonacci sequence to contain 1000 digits?
#
# Answer: 4782

def fib():
	a, b = 0, 1
	yield a
	yield b
	while True:
		a, b = b, a + b
		yield b

def fib_bigger_than(n):
	for i, x in enumerate(fib()):
		if x > n:
			return i, x

print(fib_bigger_than(10**999))
