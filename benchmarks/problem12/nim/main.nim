import tables, sets, sequtils

type
  PrimeGeneratorState = ref object
    n: int64
    last: int64
    sieve: TableRef[int64, int64]

proc initialPrimeGeneratorState(): PrimeGeneratorState =
  result = new(PrimeGeneratorState)
  result.n = 3'i64
  result.last = 2'i64
  result.sieve = newTable[int64,int64]()

proc next(self: PrimeGeneratorState): int64 =

  var prime = self.sieve.getOrDefault(self.n)
  while prime != 0:
    # don't need this key, remove it to save space
    self.sieve.del(self.n)

    # add composite
    var composite = self.n + prime + prime
    while self.sieve.contains(composite):
      composite = composite + prime + prime

    self.sieve[composite] = prime
    self.n = self.n + 2'i64
    prime = self.sieve.getOrDefault(self.n)

  # add composite in prep for next round
  self.sieve[self.n * self.n] = self.n
  result = self.last
  self.last = self.n
  self.n = self.n + 2'i64


type
  FactorFinder = ref object
    known: TableRef[int64, HashSet[int64]]
    nextPrimes: PrimeGeneratorState
    knownPrimes: seq[int64]

proc initialFactorFinder(): FactorFinder =
  result = new(FactorFinder)
  result.nextPrimes = initialPrimeGeneratorState()
  result.knownPrimes = newSeq[int64]()
  var one = initSet[int64]()
  one.incl(1'i64)
  result.known = newTable({1'i64:one})

proc getPrimeFactors(self: FactorFinder, ofn: int64): seq[int64] =
  var factors = newSeq[int64]()
  var quotient = ofn

  for prime in self.knownPrimes:
    if prime > quotient:
      return factors

    var remainder = quotient mod prime
    while remainder == 0'i64:
      quotient = quotient div prime
      remainder = quotient mod prime
      factors.add(prime)

  while true:
    let prime = self.nextPrimes.next()
    self.knownPrimes.add(prime)
    if prime > quotient:
      return factors

    var remainder = quotient mod prime
    while remainder == 0'i64:
      quotient = quotient div prime
      remainder = quotient mod prime
      factors.add(prime)


proc getFactors(self: FactorFinder, ofn: int64): HashSet[int64] =

  let existing = self.known.getOrDefault(ofn)
  if len(existing) > 0:
    return existing

  let pfs = self.getPrimeFactors(ofn)
  var factorSet = initSet[int64]()
  factorSet.incl(1'i64)
  factorSet.incl(ofn)

  for prime in pfs:
    let factor = ofn div prime
    for subfactor in self.getFactors(factor):
      factorSet.incl(subfactor)

  self.known[ofn] = factorSet
  factorSet

proc solve(): int64 =
  let finder = initialFactorFinder()

  var adder = 0'i64
  var tn = 0'i64

  while true:
    adder = adder + 1
    tn = tn + adder

    let tnFactors = finder.getFactors(tn)
    if len(tnFactors) > 1000:
      return tn

echo solve()