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

proc next(state: PrimeGeneratorState): int64 =
  while true:

    var prime = state.sieve.getOrDefault(state.n)
    if prime == 0'i64:
      break

    # don't need this key, remove it to save space
    state.sieve.del(state.n)

    # add composite
    var composite = state.n + prime + prime
    while state.sieve.contains(composite):
      composite = composite + prime + prime

    state.sieve[composite] = prime
    state.n = state.n + 2'i64

  # add composite in prep for next round
  state.sieve[state.n * state.n] = state.n
  result = state.last
  state.last = state.n
  state.n = state.n + 2'i64


type
  FactorFinder = ref object
    known: TableRef[int64, HashSet[int64]]
    nextPrimes: PrimeGeneratorState
    knownPrimes: seq[int64]

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
    # echo prime
    self.knownPrimes.add(prime)
    if prime > quotient:
      return factors

    var remainder = quotient mod prime
    while remainder == 0'i64:
      quotient = quotient div prime
      remainder = quotient mod prime
      factors.add(prime)


proc getFactors(self: FactorFinder, ofn: int64): HashSet[int64] =

  if self.known.contains(ofn):
    return self.known[ofn]

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
  let finder = new(FactorFinder)
  finder.nextPrimes = initialPrimeGeneratorState()
  finder.knownPrimes = newSeq[int64]()
  var one = initSet[int64]()
  one.incl(1'i64)
  finder.known = newTable({1'i64:one})

  var adder = 0'i64
  var tn = 0'i64
  var i = 10000

  while true:
    adder = adder + 1
    tn = tn + 1
    if tn > i:
      echo tn
      i = i + i

    let tnFactors = finder.getFactors(tn)
    if tnFactors.len() > 500:
      return tn

proc test() =
  let finder = new(FactorFinder)
  finder.nextPrimes = initialPrimeGeneratorState()
  finder.knownPrimes = newSeq[int64]()
  var one = initSet[int64]()
  one.incl(1'i64)
  finder.known = newTable({1'i64:one})

  echo finder.getFactors(20)

# test()
echo solve()