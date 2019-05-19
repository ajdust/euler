# Author: Aaron Johnson
# Date:   2015-05-25
#
# Solves problem 32:
# Pandigital Products
#
# We shall say that an n-digit number is pandigital if it makes use of all the digits 1 to n exactly once; for example, the 5-digit number, 15234, is 1 through 5 pandigital.
#
# The product 7254 is unusual, as the identity, 39 × 186 = 7254, containing multiplicand, multiplier, and product is 1 through 9 pandigital.
#
# Find the sum of all products whose multiplicand/multiplier/product identity can be written as a 1 through 9 pandigital.
# HINT: Some products can be obtained in more than one way so be sure to only include it once in your sum.

from math import factorial as fac
from itertools import permutations as P


def check_permutation_has_products(permutation, dic):
    p = ''.join(permutation)
    for i in range(1,9):
        for j in range(i + 1,9):
            a = p[0:i]
            b = p[i:j]
            c = p[j:9]
            key = ' '.join([min(a, b), max(a, b), c])
            if key not in dic and int(a) * int(b) == int(c):
                dic[key] = True

def calculate_pandigital():
    permutations = P(['1','2','3','4','5','6','7','8','9'])
    dic = {}
    for p in permutations:
        check_permutation_has_products(p, dic)
    return dic

result = calculate_pandigital()
print(len(result))
print(result)
print(sum(set(int(k.split(' ')[2]) for k in result))) # Weed out the duplicate products

# 9
# {'159 48 7632': True, '1738 4 6952': True, '12 483 5796': True, '1963 4 7852': True, '18 297 5346': True, '138 42 5796': True, '198 27 5346': True, '186 39 7254': True, '157 28 4396': True}
# 45228