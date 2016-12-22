using System;
using System.Collections.Generic;
using System.Linq;

namespace Cs.Problems
{
    /// <title>XOR decryption</title>
    /// <summary>
    /// Each character on a computer is assigned a unique code and the preferred standard is ASCII
    /// (American Standard Code for Information Interchange). For example, uppercase A = 65,
    /// asterisk (*) = 42, and lowercase k = 107.
    ///
    /// A modern encryption method is to take a text file, convert the bytes to ASCII, then XOR
    /// each byte with a given value, taken from a secret key. The advantage with the XOR function
    /// is that using the same encryption key on the cipher text, restores the plain text; for
    /// example, 65 XOR 42 = 107, then 107 XOR 42 = 65.
    ///
    /// For unbreakable encryption, the key is the same length as the plain text message, and the
    /// key is made up of random bytes. The user would keep the encrypted message and the
    /// encryption key in different locations, and without both "halves", it is impossible to
    /// decrypt the message.
    ///
    /// Unfortunately, this method is impractical for most users, so the modified method is to use
    /// a password as a key. If the password is shorter than the message, which is likely, the key
    /// is repeated cyclically throughout the message. The balance for this method is using a
    /// sufficiently long password key for security, but short enough to be memorable.
    ///
    /// Your task has been made easy, as the encryption key consists of three lower case
    /// characters. Using cipher.txt (right click and 'Save Link/Target As...'), a file containing
    /// the encrypted ASCII codes, and the knowledge that the plain text must contain common
    /// English words, decrypt the message and find the sum of the ASCII values in the original
    /// text.
    /// </summary>
    public class Problem059 : IProblem
    {
        private IEnumerable<char[]> PossibleKeys()
        {
            var alpha = Enumerable.Range('a', 26).Select(x => (char)x).ToArray();
            for (var i = 0; i < alpha.Length; i++)
            for (var j = 0; j < alpha.Length; j++)
            for (var k = 0; k < alpha.Length; k++)
            {
                yield return new char[] { alpha[i], alpha[j], alpha[k] };
            }
        }

        private int[] GetBytes()
        {
            var s = Resources.Resources.Get("Problem059_cipher.txt");
            return s.Trim().Split(',').Select(x => int.Parse(x)).ToArray();
        }

        private bool MeetsCondition(int[] d)
        {
            for (var i = 0; i < d.Length - 4; i++)
            {
                // look for 'the '
                if (d[i] == 't' && d[i+1] == 'h' && d[i+2] == 'e' && d[i+3] == ' ')
                    return true;
            }
            return false;
        }

        private IEnumerable<Tuple<string, string>> FindCandidateKeys()
        {
            var bytes = GetBytes();

            foreach (var key in PossibleKeys())
            {
                // process with key
                for (var i = 0; i < bytes.Length; i++)
                    bytes[i] = (byte)(bytes[i] ^ key[i % 3]);

                if (MeetsCondition(bytes))
                {
                    var possible = new string(bytes.Select(x => (char)x).ToArray());
                    yield return Tuple.Create(new string(key), possible);
                }

                // undo processing
                for (var i = 0; i < bytes.Length; i++)
                    bytes[i] = (byte)(bytes[i] ^ key[i % 3]);
            }
        }

        public string Solve()
        {
            var possibleAnswers = FindCandidateKeys().ToList();

            // looking in debug, the key was clearly 'god'
            var key = possibleAnswers.First(x => x.Item1 == "god").Item1.ToArray();
            var bytes = GetBytes();
            for (var i = 0; i < bytes.Length; i++)
                bytes[i] = (byte)(bytes[i] ^ key[i % 3]);

            var answer = bytes.Sum().ToString();
            return answer;
        }
    }
}
