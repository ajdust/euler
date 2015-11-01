# Author: Aaron Johnson
# Date:   2015-02-22
#
# Solves problem 24:
# Lexicographic permutations
#
# A permutation is an ordered arrangement of objects. For example, 3124 is one possible permutation of the digits 1, 2, 3 and 4. If all of the permutations are listed numerically or alphabetically, we call it lexicographic order. The lexicographic permutations of 0, 1 and 2 are:
#
# 012   021   102   120   201   210
#
# What is the millionth lexicographic permutation of the digits 0, 1, 2, 3, 4, 5, 6, 7, 8 and 9?
#
# Answer: 2783915460

from math import factorial as fac

def get_lexigraphic_permutation_n(s, n):
	""" Get the element at index n of the lexigraphically ordered set of permutations of s. """
	distance = n 								# the distance left to the goal n
	p_indexes = [None for e in s]				# the indexes into s representing the permutations
	index_pool = [i for i, e in enumerate(s)]   # the pool of indexes as used up

	cur_index = 0
	while cur_index < len(s):

		permutation_counter = fac(len(s) - cur_index - 1) # how many permutations there are between steps at this counter
		p_index = distance // permutation_counter         # how many permutation counts we can deal out without going over the limit

		p_indexes[cur_index] = index_pool.pop(p_index)    # pop from the index pool into the permutation index
		distance -= p_index * permutation_counter         # reduce by the distance we traveled

		cur_index += 1

	return p_indexes

print(get_lexigraphic_permutation_n('0123456789', 999999))

# this problem could also be simply solved by brute force as well using itertools.permutations
# sorted(list(permutations('0123456789')))[999999]