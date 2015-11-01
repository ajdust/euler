module eulerfsharp.Problem007

/// <title>10001st prime</title>
/// <summary>
/// By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.
/// What is the 10001st prime number?
/// </summary>

let Solve n = Seq.head (Seq.skip n Sequences.Primes)

