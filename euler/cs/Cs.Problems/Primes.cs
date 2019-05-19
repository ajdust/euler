using System;
using System.Collections.Generic;

namespace Cs.Problems
{
    public class Primes
    {
        private byte[] _bitmap;
        private int _primeUpperBound;
        private int _sieveUpperBound;
        private int _primeByteUpperBound;
        private int _sieveByteUpperBound;
        private bool _calculated;

        /// <summary>
        /// Implement the modulo operator (not the remainder operator) for C#.
        /// </summary>
        /// <returns>Modulo m of x</returns>
        public static int Mod(int x, int m)
        {
            var r = x % m;
            return r < 0 ? r + m : r;
        }

        private static readonly Dictionary<int, byte> Bitoffsetmap = new Dictionary<int, byte>
        {
            // 0 is not prime, recall - so to apply a sieve, bitwise-and to the byte with 0 to sieve out
            {0, 254}, // 11111110
            {1, 253}, // 11111101
            {2, 251}, // 11111011
            {3, 247}, // 11110111
            {4, 239}, // 11101111
            {5, 223}, // 11011111
            {6, 191}, // 10111111
            {7, 127}  // 01111111
        };

        public Primes(int primeUpperBound)
        {
            _calculated = false;
            _primeUpperBound = (primeUpperBound % 8) + primeUpperBound;
            _sieveUpperBound = (int)Math.Sqrt(_primeUpperBound);
            _primeByteUpperBound = _primeUpperBound / 8;
            _sieveByteUpperBound = _sieveUpperBound / 8;
            _bitmap = new byte[_primeByteUpperBound];
            // each index in the array represents that number
            // prime bit is 1, not prime bit is 0
            // initialize with odd numbers assumed to be not prime
            // 7 6 5 4 3 2 1 0
            // 1 0 1 0 1 1 0 0
            _bitmap[0] = Convert.ToByte("10101100", 2);
            for (var i = _bitmap.Length - 1; i > 0; i--)
            {
                // 15 14 13 12 11 10  9  8
                //  1  0  1  0  1  0  1  0
                _bitmap[i] = 0xAA;
            }
        }

        private Tuple<int, int> getOffsetToNextSieveMultiple(int sieve, int byteIndex)
        {
            var byteStartingNumber = byteIndex * 8;
            var bitOffset = Mod(-byteStartingNumber, sieve);
            var subBitOffset = 0;
            var byteOffset = Math.DivRem(bitOffset, 8, out subBitOffset);
            return new Tuple<int, int>(byteOffset, subBitOffset);
        }

        private void SieveWith(int sieve, int startAtByte, int endAtByte)
        {
            var offset = getOffsetToNextSieveMultiple(sieve, startAtByte);
            var offStartAtByte = startAtByte + offset.Item1;
            var bitoffset = offset.Item2;

            for (var bi = offStartAtByte; bi <= endAtByte;)
            {
                _bitmap[bi] &= Bitoffsetmap[bitoffset];
                // now, calculate the next location of the sieve
                // recalculate byte offset, bit offset
                var byteOffset = Math.DivRem(sieve + bitoffset, 8, out bitoffset);
                bi += byteOffset;
            }
        }

        private IEnumerable<int> SievePrimesAt(int byteIndex)
        {
            var b = _bitmap[byteIndex];
            var initialNumber = byteIndex << 3;
            if ((b & 0x01) > 0)
                yield return initialNumber + 0;
            if ((b & 0x02) > 0)
                yield return initialNumber + 1;
            if ((b & 0x04) > 0)
                yield return initialNumber + 2;
            if ((b & 0x08) > 0)
                yield return initialNumber + 3;
            if ((b & 0x10) > 0)
                yield return initialNumber + 4;
            if ((b & 0x20) > 0)
                yield return initialNumber + 5;
            if ((b & 0x40) > 0)
                yield return initialNumber + 6;
            if ((b & 0x80) > 0)
                yield return initialNumber + 7;
        }

        public void CalculatePrimes()
        {
            // loop through the bitmap until the sieve cutoff point
            // calling SieveWith for every prime you find
            for (var i = 0; i <= _sieveByteUpperBound; i++)
            {
                var b = _bitmap[i];
                // find any primes (1), and call it
                foreach (var prime in SievePrimesAt(i))
                {
                    SieveWith(prime, i + 1, _primeByteUpperBound - 1);
                }
            }
            _calculated = true;
        }

        public IEnumerable<int> PrimeList()
        {
            if (!_calculated) CalculatePrimes();
            var byteStart = 0;
            for (var i = 0; i < _primeByteUpperBound; i++, byteStart += 8)
            {
                var b = _bitmap[i];
                if ((b & 0x01) > 0) yield return byteStart + 0;
                if ((b & 0x02) > 0) yield return byteStart + 1;
                if ((b & 0x04) > 0) yield return byteStart + 2;
                if ((b & 0x08) > 0) yield return byteStart + 3;
                if ((b & 0x10) > 0) yield return byteStart + 4;
                if ((b & 0x20) > 0) yield return byteStart + 5;
                if ((b & 0x40) > 0) yield return byteStart + 6;
                if ((b & 0x80) > 0) yield return byteStart + 7;
            }
        }

        public IEnumerable<int> PrimesListBackwards()
        {
            if (!_calculated) CalculatePrimes();
            var byteStart = (_primeByteUpperBound - 1)*8;
            for (var i = _primeByteUpperBound - 1; i >= 0; i--, byteStart -= 8)
            {
                var b = _bitmap[i];
                if ((b & 0x80) > 0) yield return byteStart + 7;
                if ((b & 0x40) > 0) yield return byteStart + 6;
                if ((b & 0x20) > 0) yield return byteStart + 5;
                if ((b & 0x10) > 0) yield return byteStart + 4;
                if ((b & 0x08) > 0) yield return byteStart + 3;
                if ((b & 0x04) > 0) yield return byteStart + 2;
                if ((b & 0x02) > 0) yield return byteStart + 1;
                if ((b & 0x01) > 0) yield return byteStart + 0;
            }
        }
    }
}