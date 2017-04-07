#include <string>
#include <iostream>
#include <sstream>
#include <limits>
#include <vector>
#include <map>
#include <algorithm>

namespace Problems {

/* Aaron Johnson
   2017-04-04 */


/*
 * Problem 1: Multiples of 3 and 5
 * If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6
 * and 9. The sum of these multiples is 23. Find the sum of all the multiples of 3 or 5
 * below 1000.
 */
std::string problem01() {
    int sum = 0;
    for (int i = 1; i < 1000; i++) {
        if (i % 3 == 0 || i % 5 == 0) {
            sum += i;
        }
    }

    return std::to_string(sum);
}


/*
 * Problem 2: Even Fibonacci Numbers
 * Each new term in the Fibonacci sequence is generated by adding
 * the previous two terms. By starting with 1 and 2, the first 10 terms will be:
 *
 * 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ...
 *
 * By considering the terms in the Fibonacci sequence whose values do not exceed
 * four million, find the sum of the even-valued terms.
 */

// borrowing from http://en.cppreference.com/w/cpp/iterator/iterator
class Fibonacci {
public:
    class iterator : public std::iterator<
        std::input_iterator_tag, int, int, const int*, int> {
        int a = 0;
        int b = 1;
        bool is_end = false;
    public:
        explicit iterator(bool _is_end = false) : is_end(_is_end) {}

        iterator& operator++() {
            if (a + b <= a) {
                is_end = true;
                return *this;
            }

            int temp = b;
            b = b + a;
            a = temp;
            return *this;
        }

        iterator operator++(int) {
            iterator retval = *this;
            ++(*this);
            return retval;
        }

        bool operator==(iterator other) const {
            return a == other.a && b == other.b && is_end == other.is_end;
        }

        bool operator!=(iterator other) const {
            return !(*this == other);
        }

        reference operator*() const {
            return a;
        }
    };

    iterator begin() {
        return iterator();
    }

    iterator end() {
        return iterator(true);
    }
};

std::string problem02() {

    int sum = 0;
    for (auto f : Fibonacci()) {
        if (f < 4000000) {
            if (f % 2 == 0) {
                sum += f;
            }
        } else {
            break;
        }
    }

    return std::to_string(sum);
}


/*
 * Problem 3: Largest prime factor
 * The prime factors of 13195 are 5, 7, 13 and 29.
 * What is the largest prime factor of the number 600851475143 ?
 */
class Primes {
public:
    class iterator : public std::iterator<
        std::input_iterator_tag, long, long, const long*, long> {
        long n = 3;
        long last = 2;
        std::map<long, long> sieve;
        bool is_end = false;
    public:
        explicit iterator(bool _is_end = false) : is_end(_is_end) {}

        iterator& operator++() {

            // check for presence of composite
            auto it = sieve.find(n);
            while (it != sieve.end()) {

                // save space by removing key we've moved past
                sieve.erase(it);
                auto prime = it->second;

                // add composites for this prime
                long n_ = n + prime + prime;
                while (sieve.find(n_) != sieve.end()) {
                    n_ += prime + prime;
                }
                sieve[n_] = prime;
                n += 2;

                it = sieve.find(n);
            }

            // set composite for next round to the prime's square
            sieve[n * n] = n;
            last = n;
            n += 2;
            return *this;
        }

        iterator operator++(int) {
            iterator retval = *this;
            ++(*this);
            return retval;
        }

        bool operator==(iterator other) const {
            return last == other.last && is_end == other.is_end;
        }

        bool operator!=(iterator other) const {
            return !(*this == other);
        }

        reference operator*() const {
            return last;
        }
    };

    iterator begin() {
        return iterator();
    }

    iterator end() {
        return iterator(true);
    }
};

std::vector<long> prime_factors(long of) {
    std::vector<long> pfacts;
    auto quotient = of;

    for (auto prime : Primes()) {
        if (prime > quotient) {
            break;
        }

        auto remainder = quotient % prime;
        while (remainder == 0) {
            quotient = quotient / prime;
            remainder = quotient % prime;
            pfacts.push_back(prime);
        }
    }

    return pfacts;
}

std::string problem03() {
    auto pf = prime_factors(600851475143);
    return std::to_string(*max_element(pf.begin(), pf.end()));
}

/*
 * Problem 4: Largest palindromic product
 * A palindromic number reads the same both ways. The largest palindrome
 * made from the product of two 2-digit numbers is 9009 = 91 × 99.
 * Find the largest palindrome made from the product of two 3-digit numbers.
*/
bool is_palindrome(std::string s) {
    auto len = s.length();
    for (int i = 0, j = len - 1; i < j; i++, j--) {
        if (s[i] != s[j]) {
            return false;
        }
    }

    return true;
}

std::string problem04() {
    int max = 0;
    for (int i = 100; i < 999; i++) {
        for (int j = 100; j < i; j++) {
            int prod = i * j;
            if (prod > max && is_palindrome(std::to_string(prod))) {
                max = prod;
            }
        }
    }
    return std::to_string(max);
}


/*
 * Problem 5: Smallest multiple
 * 2520 is the smallest number that can be divided by each of the numbers from 1 to 10
 * without any remainder. What is the smallest positive number that is evenly divisible
 * by all of the numbers from 1 to 20?
 */
std::map<long, int> count_duplicates(std::vector<long> v) {
    std::map<long, int> counted;
    for (auto n : v) {
        auto currentCount = counted.find(n);
        if (currentCount == counted.end()) {
            counted[n] = 1;
        } else {
            counted[n] = currentCount->second + 1;
        }
    }

    return counted;
}

std::map<long, int> union_highest_count(std::vector<std::map<long, int>> counts) {
    std::map<long, int> highest;
    for (auto count : counts) {
        for (auto& kv : count) {
            auto currentCount = highest.find(kv.first);
            if (currentCount == highest.end()
                || currentCount->second < kv.second) {
                highest[kv.first] = kv.second;
            }
        }
    }

    return highest;
}

std::string problem05() {
    std::vector<std::map<long, int>> counts(19);
    for (int n = 2, i = 0; n <= 20; n++, i++) {
        counts[i] = count_duplicates(prime_factors(n));
    }

    auto unioned = union_highest_count(counts);
    long answer = 1;
    for (auto& kv : unioned) {
        for (int i = 0; i < kv.second; i++) {
            answer *= kv.first;
        }
    }

    return std::to_string(answer);
}

/*
 * Problem 6: Sum square difference
 *
 * The sum of the squares of the first ten natural numbers is,
 * 12 + 22 + ... + 102 = 385
 * The square of the sum of the first ten natural numbers is,
 * (1 + 2 + ... + 10)2 = 552 = 3025
 * Hence the difference between the sum of the squares of the first ten natural numbers
 * and the square of the sum is 3025 − 385 = 2640.
 *
 * Find the difference between the sum of the squares of the first one hundred natural
 * numbers and the square of the sum.
 */

/*
 * Problem 7: 10001st prime
 *
 * By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13,
 * we can see that the 6th prime is 13. What is the 10001st prime number?
 */

/*
 * Problem 8: Largest product in a series
 *
 * The four adjacent digits in the 1000-digit number that have the greatest
 * product are 9 × 9 × 8 × 9 = 5832.
 *
 * 73167176531330624919225119674426574742355349194934
 * 96983520312774506326239578318016984801869478851843
 * 85861560789112949495459501737958331952853208805511
 * 12540698747158523863050715693290963295227443043557
 * 66896648950445244523161731856403098711121722383113
 * 62229893423380308135336276614282806444486645238749
 * 30358907296290491560440772390713810515859307960866
 * 70172427121883998797908792274921901699720888093776
 * 65727333001053367881220235421809751254540594752243
 * 52584907711670556013604839586446706324415722155397
 * 53697817977846174064955149290862569321978468622482
 * 83972241375657056057490261407972968652414535100474
 * 82166370484403199890008895243450658541227588666881
 * 16427171479924442928230863465674813919123162824586
 * 17866458359124566529476545682848912883142607690042
 * 24219022671055626321111109370544217506941658960408
 * 07198403850962455444362981230987879927244284909188
 * 84580156166097919133875499200524063689912560717606
 * 05886116467109405077541002256983155200055935729725
 * 71636269561882670428252483600823257530420752963450
 *
 * Find the thirteen adjacent digits in the 1000-digit number that have the
 * greatest product. What is the value of this product?
 */


/*
 * Problem 9: Special Pythagorean triplet
 *
 *
 * A Pythagorean triplet is a set of three natural numbers, a LT b LT c, for which,
 * a^2 + b^2 = c^2
 *
 * For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.
 *
 * There exists exactly one Pythagorean triplet for which a + b + c = 1000.
 * Find the product a*b*c.
 */



/*
 * Problem 10: Summation of primes
 *
 * The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
 * Find the sum of all the primes below two million.
 */



} // end of Problems namespace






