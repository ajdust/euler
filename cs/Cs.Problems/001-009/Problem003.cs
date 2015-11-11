namespace Cs.Problems
{
    /// <title>Largest prime factor</title>
    /// <summary>
    /// The prime factors of 13195 are 5, 7, 13 and 29. 
    /// What is the largest prime factor of the number 600851475143 ?
    /// </summary>
    /// <answer>6857</answer>
    public class Problem003 : IProblem
    {
        public string Solve()
        {
            var factors = Sequences.GetPrimeFactors(600851475143);
            return factors[factors.Count - 1].ToString();
        }
    }
}
