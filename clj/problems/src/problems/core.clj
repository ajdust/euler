(ns problems.core
  (:gen-class)
  (:import (java.util HashMap ArrayList)))

(require 'clojure.set)
(require 'clojure.math.numeric-tower)

; Paredit is EVIL
; Oh how I miss static typing, in which all of these issues would be fixed
; - used infix accidentally
; - lost track of parenthesis and ended up with incorrect scoping
; - spent 10 minutes scratching my head to find this mistake:
;   "if [<expr>]" vs "if (<expr>)" because of truthiness (not a fan of truthiness at all)
; - spent far too long debugging an extra pair of parenthesis in a let
;   as e.g. "((println 5) #{1 5})" treats nil returned from print as a function
; - spent far too long debugging a missing pair of parenthesis in a let
;   e.g. "(let [pg (init-prime-gen)]
 ;         last (repeatedly 10001 (...)))"
; - many times accidentally calling a number a function, range not castable to number, etc.
; - using get wrong: it's "(get collection index)", not "(get index collection)", but Clojure can't tell me that
; let feels pretty verbose considering most languages don't need parenthesis/scope to declare variables
;   e.g "(let [half (quot .length 2)] ...)"  vs. "val half = s.length/2 ..."
; almost every function can take/return nil, ew, so strange constructs like "(or (get (get grid 5) 10) 0)" work
; requires rainbow parenthesis plugin to be readable, the way I've have parenthesis on their own line
;   is no-doubt not idiomatic

(defn sum [coll] (reduce + coll))
(defn product [coll] (reduce * coll))

(defn problem01 []
  (->>
    (take 1000 (range))
    (filter #(or (= 0 (mod % 3)) (= 0 (mod % 5))))
    sum))

(defn fib []
  (map #(get % 0) (iterate (fn [[a b]] [b (+ a b)]) [0 1])))

(defn problem02 []
  (->>
    (fib)
    (filter #(= 0 (mod % 2)))
    (take-while #(< % 4e6))
    sum))

(definterface IPrimeGenState (nextprime []))

(defn add-composite [init step sieve]
  (if (.containsKey sieve init)
    (add-composite (+ init step) step sieve)
    init))

(deftype PrimeGenState
  [sieve
   ^{:volatile-mutable true} n
   ^{:volatile-mutable true} last]
  IPrimeGenState
  (nextprime [this]
    (while (.containsKey sieve n)
      (let [prime (.getOrDefault sieve n 0)]
        (.remove sieve n)
        (.put sieve (add-composite (+ n prime prime) (+ prime prime) sieve) prime)
        (set! n (+ n 2))
      )
    )

    (.put sieve (* n n) n)
    (let [r last]
      (set! last n)
      (set! n (+ n 2))
      r
    )
  )
)

(defn init-prime-gen [] (PrimeGenState. (new HashMap) 3 2))

(definterface IFactorFinder
  (getprimefactors [of])
  (getfactors [of]))

(defn count-whole-divisions
  ([quotient remainder divisor count]
  (if (= 0 remainder)
    (let [nq (quot quotient divisor)]
      (count-whole-divisions nq (mod nq divisor) divisor (inc count)))
    [count quotient]
  ))

  ([quotient divisor]
  (count-whole-divisions quotient (mod quotient divisor) divisor 0))
)

(defn get-prime-factors-with-known
  [iterator quotient factors]
  (if (.hasNext iterator)
    (let [kp (.next iterator)]
      (if (> kp quotient)
        [0 factors]
        (let [[count dquotient] (count-whole-divisions quotient kp)]
          (get-prime-factors-with-known iterator dquotient (concat (repeat count kp) factors))
        )
      )
    )
    [quotient factors]
  )
)

(defn conj-rev [element coll] (conj coll element))

(defn get-prime-factors-with-prime-gen
  [prime-gen quotient factors known-primes]
  (let [np (.nextprime prime-gen)]
    (.add known-primes np)
    (if (or (> np quotient) (= 1 quotient))
      factors
      (let [[count dquotient] (count-whole-divisions quotient np)]
        (if (= 0 count)
          (get-prime-factors-with-prime-gen prime-gen dquotient factors known-primes)
          (get-prime-factors-with-prime-gen prime-gen dquotient (concat factors (repeat count np)) known-primes)
        )
      )
    )
  )
)

(deftype FactorFinder
  [prime-gen known-primes known-factors]
  IFactorFinder
  (getprimefactors [this of]
    (let [[quotient factors] (get-prime-factors-with-known (.iterator known-primes) of [])]
      (if (= 0 quotient)
        factors
        (get-prime-factors-with-prime-gen prime-gen quotient factors known-primes)
      )
    )
  )

  (getfactors [this of]
    (let [kfactors (.get known-factors of)]
      (if (nil? kfactors)
        (let [factors
          (->>
            (.getprimefactors this of)
            (map #(.getfactors this (quot of %)))
            (apply clojure.set/union)
            (conj-rev 1)
            (conj-rev of)
          )]
          (.put known-factors of factors)
          factors
        )
        kfactors
      )
    )
  )
)

(defn init-factor-finder []
  (let [known-factors (new HashMap)]
    (.put known-factors 1 #{1})
    (FactorFinder. (init-prime-gen) (new ArrayList) known-factors)
  )
)

(defn problem03 []
  (let [ff (init-factor-finder)]
    (->>
      (.getprimefactors ff 600851475143)
      (apply max)
    )
  )
)

(defn is-palindrome [s]
  (let [half (quot (.length s) 2)]
    (= (take half s) (take half (reverse s)))
  )
)

(defn problem04 []
  (->>
    (for [r1 (range 100 1001) r2 (range 101 1000)
          :when (is-palindrome (str (* r1 r2)))] (* r1 r2))
    (apply max)
  )
)

(defn problem05 []
  (let [bi #(BigInteger. (str %))
        ff (init-factor-finder)
        count-common-factors (fn [i] (->> (.getprimefactors ff i) frequencies))]
    (->>
      (range 1 20)
      (map count-common-factors)
      (apply merge-with max)
      (reduce-kv #(* %1 (.pow (bi %2) (bi %3))) (bi 1))
    )
  )
)

(defn problem06 []
  (let [square #(* % %)
        sum-squares (->> (range 1 101) (map square) sum)
        square-sums (square (->> (range 1 101) sum))]
    (clojure.math.numeric-tower/abs (- sum-squares square-sums))
  )
)

(defn problem07 []
  (let [pg (init-prime-gen)]
    (last (repeatedly 10001 #(.nextprime pg)))
  )
)

(defn problem08 []
  (let [digits (->> "7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450"
                 (map #(java.lang.Character/getNumericValue %)))
        product13 (fn [start-index]
                    (->> digits
                         (drop start-index) (take 13)
                         (reduce *)))
        ]
    (->> (range 0 (- (count digits) 14)) (map product13) (apply max))
  )
)

(defn squares []
  (map (fn [x] [x (* x x)]) (drop 1 (range))))
(defn pyth-triplets []
  (for [[c c2] (squares)
        [b b2] (->> (squares) (take-while (fn [[b _]] (< b c))))
        [a a2] (->> (squares) (take-while (fn [[a _]] (< a b))))
        :when (= c2 (+ a2 b2))]
    [a b c]
  )
)

(defn problem09 []
  (some
    (fn [[a b c]] (if (= 1000 (+ a b c)) (* a b c)))
    (pyth-triplets)
  )
)

(defn problem10 []
  (let [pg (init-prime-gen)]
    (->>
      (repeatedly #(.nextprime pg))
      (take-while #(< % 2000000))
      sum
    )
  )
)

(defn get-lines [x y]
  [[ [x y] [(+ x 1) y]       [(+ x 2) y]       [(+ x 3) y]       ]
   [ [x y] [x (+ y 1)]       [x (+ y 2)]       [x (+ y 3)]       ]
   [ [x y] [(+ x 1) (+ y 1)] [(+ x 2) (+ y 2)] [(+ x 3) (+ y 3)] ]
   [ [x y] [(- x 1) (+ y 1)] [(- x 2) (+ y 2)] [(- x 3) (+ y 3)] ]]
)

(defn problem11 []
  (let [grid [[  8  2 22 97 38 15    40  0 75  4  5  7 78 52 12 50 77 91  8 ]
              [ 49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48  4 56 62  0 ]
              [ 81 49 31 73 55 79 14 29 93 71 40 67 53 88 30  3 49 13 36 65 ]
              [ 52 70 95 23  4 60 11 42 69 24 68 56  1 32 56 71 37  2 36 91 ]
              [ 22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80 ]
              [ 24 47 32 60 99  3 45  2 44 75 33 53 78 36 84 20 35 17 12 50 ]
              [ 32 98 81 28 64 23 67 10 26 38 40 67 59 54 70 66 18 38 64 70 ]
              [ 67 26 20 68  2 62 12 20 95 63 94 39 63  8 40 91 66 49 94 21 ]
              [ 24 55 58  5 66 73 99 26 97 17 78 78 96 83 14 88 34 89 63 72 ]
              [ 21 36 23  9 75  0 76 44 20 45 35 14  0 61 33 97 34 31 33 95 ]
              [ 78 17 53 28 22 75 31 67 15 94  3 80  4 62 16 14  9 53 56 92 ]
              [ 16 39  5 42 96 35 31 47 55 58 88 24  0 17 54 24 36 29 85 57 ]
              [ 86 56  0 48 35 71 89  7  5 44 44 37 44 60 21 58 51 54 17 58 ]
              [ 19 80 81 68  5 94 47 69 28 73 92 13 86 52 17 77  4 89 55 40 ]
              [  4 52  8 83 97 35 99 16  7 97 57 32 16 26 26 79 33 27 98 66 ]
              [ 88 36 68 87 57 62 20 72  3 46 33 67 46 55 12 32 63 93 53 69 ]
              [  4 42 16 73 38 25 39 11 24 94 72 18  8 46 29 32 40 62 76 36 ]
              [ 20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74  4 36 16 ]
              [ 20 73 35 29 78 31 90  1 74 31 49 71 48 86 81 16 23 57  5 54 ]
              [  1 70 54 71 83 51 54 69 16 92 33 48 61 43 52  1 89 19 67 48 ]]
        at-or-one (fn [[x y]] (or (get (get grid x) y) 1))]
    (->>
      (for [x (range 0 20) y (range 0 20)] (get-lines x y))
      (mapcat identity)
      (map (fn [line] (map at-or-one line)))
      (map (fn [line] {:line line :product (product line)}))
      (apply max-key :product)
      :product
    )
  )
)

(defn triangle-numbers []
  (->>
    (iterate
      (fn [[trinum increment]] [(+ trinum increment) (inc increment)])
      [0 1])
    (map #(get % 0))
  )
)

(defn problem12 [n]
  (let [ff (init-factor-finder)]
    (->>
      (triangle-numbers)
      (map (fn [tn] { :tn tn :count (count (.getfactors ff tn)) }))
      (some #(if (> (:count %) n) %))
    )
  )
)

(defn -main [& args]
  (println "Hello, World! Calculating..")
  (println (problem12 500)))
