using System.Collections.Generic;

namespace Cs.Problems
{
    public static class Combinatorics
    {
        public static IEnumerable<char[]> PermuteBackward(char[] characters)
        {
            yield return characters;
            while (PreviousPermutation(characters))
                yield return characters;
        }

        public static IEnumerable<char[]> PermuteForward(char[] characters)
        {
            yield return characters;
            while (NextPermutation(characters))
                yield return characters;
        }

        /// https://github.com/eoincampbell/combinatorics/blob/master/Combinatorics.Net40/Collections/Permutations.cs
        /// http://www.cut-the-knot.org/do_you_know/AllPerm.shtml
        private static bool NextPermutation(char[] characters)
        {
            int i = characters.Length - 1;
            while (characters[i - 1] >= characters[i])
            {
                i--;
                if (i == 0)
                    return false;
            }

            int j = characters.Length;
            while (characters[j - 1] <= characters[i - 1])
            {
                j--;
            }

            Swap(characters, i - 1, j - 1);
            i++;
            j = characters.Length;
            while (i < j)
            {
                Swap(characters, i - 1, j - 1);
                i++;
                j--;
            }

            return true;
        }

        private static bool PreviousPermutation(char[] characters)
        {
            int i = characters.Length - 1;
            while (characters[i - 1] <= characters[i])
            {
                i--;
                if (i == 0)
                    return false;
            }

            int j = characters.Length;
            while (characters[j - 1] >= characters[i - 1])
            {
                j--;
            }

            Swap(characters, i - 1, j - 1);
            i++;
            j = characters.Length;
            while (i > j)
            {
                Swap(characters, i - 1, j - 1);
                i++;
                j--;
            }

            return true;
        }

        private static void Swap(char[] a, int i, int j)
        {
            char c = a[i];
            a[i] = a[j];
            a[j] = c;
        }
    }
}
