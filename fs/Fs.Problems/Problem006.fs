module eulerfsharp.Problem006

/// <title>Sum square difference</title>
/// <summary>
/// The sum of the squares of the first ten natural numbers is,
/// 12 + 22 + ... + 102 = 385 
/// The square of the sum of the first ten natural numbers is,
/// (1 + 2 + ... + 10)2 = 552 = 3025
/// Hence the difference between the sum of the squares of the first ten natural numbers
/// and the square of the sum is 3025 − 385 = 2640.
/// 
/// Find the difference between the sum of the squares of the first one hundred natural
/// numbers and the square of the sum.
/// </summary>
/// <remarks>
/// This problem can be solved with brute force easily, but it is better to use the formulas.
/// Formula for sum of squares:    1^2 + 2^2 + 3^2 ... + n^2 = n(n+1)(2n+1)/6 
/// Formula for square of the sum: (1 + 2 + 3 + ... + n)^2   = (n(n+1)/2)^2
/// </remarks>

open System.Numerics

let SumOfSquares n = n*(n + 1)*(2*n + 1) / 6

let SumOfNumbers n = n*(n + 1) / 2

let Solve n = pown (SumOfNumbers n) 2 - SumOfSquares n