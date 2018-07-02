#include <stdint.h>
#include <stdio.h>
#include <inttypes.h>
#include <utarray.h>
#include <uthash.h>
/*
   Using https://github.com/troydhanson/uthash
   Helpers for C equivalent to:
   Map<int64_t, int64_t>, Set<int64_t>
   Map<int64_t, Set<int64_t>>
*/

typedef struct KeyValuePair {
    int64_t key;
    int64_t value;
    UT_hash_handle hh;
} KeyValuePair;
typedef KeyValuePair* Map;

KeyValuePair* map_find(Map map, int64_t key) {
    KeyValuePair* s;
    HASH_FIND_INT(map, &key, s);
    return s;
}

void map_add(Map* map, int64_t key, int64_t value) {
    if (map_find(*map, key) != NULL)
        return;

    KeyValuePair* kvp = malloc(sizeof(KeyValuePair));
    kvp->key = key;
    kvp->value = value;
    HASH_ADD_INT(*map, key, kvp);
}

typedef struct Value {
    int64_t value;
    UT_hash_handle hh;
} Value;
typedef Value* Set;

Value* set_find(Set set, int64_t value) {
    Value* s;
    HASH_FIND_INT(set, &value, s);
    return s;
}

void set_add(Set* set, int64_t value) {
    if (set_find(*set, value) != NULL)
        return;

    Value* new_value = malloc(sizeof(Value));
    new_value->value = value;
    HASH_ADD_INT(*set, value, new_value);
}

void set_free(Set set) {
    Value* s = NULL;
    Value* tmp = NULL;
    HASH_ITER(hh, set, s, tmp) { free(s); }
    free(set);
}

typedef struct SetMapPair {
    int64_t key;
    Set set;
    UT_hash_handle hh;
} SetMapPair;
typedef SetMapPair* SetMap;

SetMapPair* setmap_find(SetMap setmap, int64_t key) {
    SetMapPair* s;
    HASH_FIND_INT(setmap, &key, s);
    return s;
}

void setmap_add(SetMap* setmap, int64_t key, Set set) {
    if (setmap_find(*setmap, key) != NULL)
        return;

    SetMapPair* new_ksp = malloc(sizeof(SetMapPair));
    new_ksp->key = key;
    new_ksp->set = set;
    HASH_ADD_INT(*setmap, key, new_ksp);
}

void setmap_free(SetMap setmap) {
    SetMapPair* smp = NULL;
    SetMapPair* tmp = NULL;
    HASH_ITER(hh, setmap, smp, tmp) {
        set_free(smp->set);
        free(smp);
    }
}

/* End data structure helpers */

/* Prime generating */

typedef struct PrimeGen {
    int64_t n;
    int64_t last;
    Map sieve;
} PrimeGen;

PrimeGen* primegen_new(void) {
    PrimeGen* pg = malloc(sizeof(PrimeGen));
    pg->n = 3;
    pg->last = 2;
    pg->sieve = NULL;
    return pg;
}

void primegen_free(PrimeGen* pg) {
    KeyValuePair* kv;
    KeyValuePair* tmp;
    KeyValuePair* sieve = pg->sieve;

    HASH_ITER(hh, sieve, kv, tmp) {
        HASH_DEL(sieve, kv);  /* delete it */
        free(kv);             /* free it */
    }

    free(pg);
}


int64_t primegen_next(PrimeGen* pgp) {
    KeyValuePair* it = map_find(pgp->sieve, pgp->n);
    while (it != NULL) {

        HASH_DEL(pgp->sieve, it);
        int64_t prime = it->value;

        int64_t composite = pgp->n + prime + prime;
        while (map_find(pgp->sieve, composite) != NULL) {
            composite += prime + prime;
        }
        map_add(&pgp->sieve, composite, prime);
        pgp->n = pgp->n + 2;

        it = map_find(pgp->sieve, pgp->n);
    }

    map_add(&pgp->sieve, pgp->n * pgp->n, pgp->n);
    int64_t r = pgp->last;
    pgp->last = pgp->n;
    pgp->n = pgp->n + 2;
    return r;
}

/* End prime generating */

/* Factor finding */

UT_icd int64_icd = {sizeof(int64_t), NULL, NULL, NULL};

typedef struct FactorFinder {
    SetMap known;
    PrimeGen* next_primes;
    UT_array* known_primes;
} FactorFinder;

FactorFinder* factorfinder_new(void) {
    Set justone = NULL;
    set_add(&justone, 1);

    SetMap setmap = NULL;
    setmap_add(&setmap, 1, justone);

    UT_array* array = NULL;
    utarray_new(array, &int64_icd);

    FactorFinder* ff = malloc(sizeof(FactorFinder));
    ff->known = setmap;
    ff->next_primes = primegen_new();
    ff->known_primes = array;
    return ff;
}

UT_array* factorfinder_get_prime_factors(FactorFinder* ff, int64_t of) {

    UT_array* factors;
    utarray_new(factors, &int64_icd);

    int64_t quotient = of;

    for (int64_t* primep = (int64_t*)utarray_front(ff->known_primes);
        primep != NULL;
        primep = (int64_t*)utarray_next(ff->known_primes, primep)) {
        int64_t prime = *primep;

        if (prime > quotient) {
            return factors;
        }

        int64_t remainder = quotient % prime;
        while (remainder == 0) {
            quotient = quotient / prime;
            remainder = quotient % prime;
            utarray_push_back(factors, &prime);
        }
    }

    for (;;) {
        int64_t prime = primegen_next(ff->next_primes);
        utarray_push_back(ff->known_primes, &prime);

        if (prime > quotient) {
            return factors;
        }

        int64_t remainder = quotient % prime;
        while (remainder == 0) {
            quotient = quotient / prime;
            remainder = quotient % prime;
            utarray_push_back(factors, &prime);
        }
    }

    return factors;
}

Set factorfinder_get_factors(FactorFinder* ff, int64_t of) {

    SetMapPair* existing = setmap_find(ff->known, of);
    if (existing != NULL) {
        return existing->set;
    }

    UT_array* pfactors = factorfinder_get_prime_factors(ff, of);
    Set factor_set = NULL;
    set_add(&factor_set, 1);
    set_add(&factor_set, of);

    for (int64_t* primep = (int64_t*)utarray_front(pfactors);
        primep != NULL;
        primep = (int64_t*)utarray_next(pfactors, primep)) {
        int64_t prime = *primep;

        int64_t factor = of / prime;
        Set subfactors = factorfinder_get_factors(ff, factor);

        for (Value* v = subfactors; v != NULL; v = v->hh.next) {
            int64_t subfactor = v->value;
            set_add(&factor_set, subfactor);
        }
    }

    setmap_add(&ff->known, of, factor_set);
    return factor_set;
}

int64_t solve(void) {

    FactorFinder* ff = factorfinder_new();

    int64_t adder = 0;
    int64_t tn = 0;

    for (;;) {
        adder += 1;
        tn += adder;

        Set factors = factorfinder_get_factors(ff, tn);
        if (HASH_COUNT(factors) > 1000) {
            printf("Got %" PRId64 "\n", tn);
            set_free(factors);
            utarray_free(ff->known_primes);
            free(ff);

            return tn;
        }
    }
}

void print_primes(int n) {
    PrimeGen* pg = primegen_new();
    for (int i = 0; i < n; i++) {
        int64_t next = primegen_next(pg);
        printf("Prime: %" PRId64 "\n", next);
    }
    primegen_free(pg);
}

void print_prime_factors_test(void) {
    FactorFinder* ff = factorfinder_new();
    UT_array* factors = factorfinder_get_prime_factors(ff, 135);
    for (int64_t* factorp = (int64_t*)utarray_front(factors);
        factorp != NULL;
        factorp = (int64_t*)utarray_next(factors, factorp)) {
        printf("Prime Factor: %" PRId64 "\n", *factorp);
    }
}

void print_factors_test(void) {
    FactorFinder* ff = factorfinder_new();
    Set factors = factorfinder_get_factors(ff, 20);
    Value* v = NULL;
    for (v = factors; v != NULL; v = v->hh.next) {
        printf("Factor: %" PRId64 "\n", v->value);
    }
}

int main() {
    // print_primes(20);
    // print_prime_factors_test();
    // print_factors_test();
    int64_t answer = solve();
    printf("Answer: %" PRId64 "\n", answer);
}


