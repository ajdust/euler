using System;
using System.Collections.Generic;
using System.Linq;

namespace Cs.Problems
{
    /// <title>Poker hands</title>
    /// <summary>
    /// In the card game poker, a hand consists of five cards and are ranked, from lowest to
    /// highest, in the following way:
    ///
    ///     High Card: Highest value card.
    ///     One Pair: Two cards of the same value.
    ///     Two Pairs: Two different pairs.
    ///     Three of a Kind: Three cards of the same value.
    ///     Straight: All cards are consecutive values.
    ///     Flush: All cards of the same suit.
    ///     Full House: Three of a kind and a pair.
    ///     Four of a Kind: Four cards of the same value.
    ///     Straight Flush: All cards are consecutive values of same suit.
    ///     Royal Flush: Ten, Jack, Queen, King, Ace, in same suit.
    ///
    /// The cards are valued in the order:
    /// 2, 3, 4, 5, 6, 7, 8, 9, 10, Jack, Queen, King, Ace.
    ///
    /// If two players have the same ranked hands then the rank made up of the highest value wins;
    /// for example, a pair of eights beats a pair of fives (see example 1 below). But if two
    /// ranks tie, for example, both players have a pair of queens, then highest cards in each hand
    /// are compared (see example 4 below); if the highest cards tie then the next highest cards
    /// are compared, and so on.
    ///
    /// Consider the following five hands dealt to two players:
    ///
    /// | Hand          | Player 1          | Player 2            | Winner   |
    /// | ------------- | ----------------- | ------------------- | -------- |
    /// | 1             | 5H 5C 6S 7S KD    | 2C 3S 8S 8D TD      |          |
    /// |               | Pair of Fives     | Pair of Eights      | Player 2 |
    /// |               |                   |                     |          |
    /// | 2             | 5D 8C 9S JS AC    | 2C 5C 7D 8S QH      |          |
    /// |               | Highest card Ace  | Highest card Queen  | Player 1 |
    /// |               |                   |                     |          |
    /// | 3             | 2D 9C AS AH AC    | 3D 6D 7D TD QD      |          |
    /// |               | Three Aces        | Flush with Diamonds | Player 2 |
    /// |               |                   |                     |          |
    /// | 4             | 4D 6S 9H QH QC    | 3D 6D 7H QD QS      |          |
    /// |               | Pair of Queens    | Pair of Queens      |          |
    /// |               | Highest card Nine | Highest card Seven  | Player 1 |
    /// |               |                   |                     |          |
    /// | 5             | 2H 2D 4C 4D 4S    | 3C 3D 3S 9S 9D      |          |
    /// |               | Full House        | Full House          |          |
    /// |               | With Three Fours  | with Three Threes   | Player 1 |
    ///
    /// The file, poker.txt, contains one-thousand random hands dealt to two players.
    /// Each line of the file contains ten cards (separated by a single space):
    /// the first five are Player 1's cards and the last five are Player 2's cards.
    /// You can assume that all hands are valid (no invalid characters or repeated cards),
    /// each player's hand is in no specific order, and in each hand there is a clear winner.
    ///
    /// How many hands does Player 1 win?
    /// </summary>
    public class Problem054 : IProblem
    {
        private enum Suit
        {
            Clubs = 1,
            Hearts = 2,
            Diamonds = 3,
            Spades = 4,
        }

        private class Card : IComparable<Card>
        {
            public Suit Suit { get; set; }
            public int Value { get; set; }

            public int CompareTo(Card other)
            {
                if (other == null)
                    return 1;

                return Value.CompareTo(other.Value);
            }
        }

        private enum Rank
        {
            None = 0,
            HighCard = 1,
            OnePair = 2,
            TwoPair = 3,
            ThreeKind = 4,
            Straight = 5,
            Flush = 6,
            FullHouse = 7,
            FourKind = 8,
            StraightFlush = 9
        }

        private class HandRank : IComparable<HandRank>
        {
            public HandRank(Rank rank, int highValue = 0, int secondHighValue = 0)
            {
                Rank = rank;
                HighValue = highValue;
                SecondHighValue = secondHighValue;
            }

            public Rank Rank { get; set; }
            public int HighValue { get; set; }

            // for full house comparison
            public int SecondHighValue { get; set; }

            public int CompareTo(HandRank other)
            {
                if (other == null)
                    return 1;

                if (Rank == other.Rank)
                {
                    if (HighValue == other.HighValue)
                    {
                        if (SecondHighValue == other.SecondHighValue)
                            return 0;
                        return SecondHighValue > other.SecondHighValue ? 1 : -1;
                    }

                    return HighValue > other.HighValue ? 1 : -1;
                }

                return Rank > other.Rank ? 1 : -1;
            }
        }

        private class CardGroup
        {
            public int Count { get; set; }
            public Card Card { get; set; }
        }

        private class Hand : IComparable<Hand>
        {
            public Card[] Cards { get; set; }

            public IEnumerable<Card> HighToLow => Cards.OrderByDescending(x => x);
            public Card HighCard => HighToLow.First();
            public bool Flush => Cards.Select(x => x.Suit).Distinct().Count() == 1;
            public bool Straight => Cards.OrderBy(x => x).Select((x, i) => x.Value - i).Distinct().Count() == 1;
            public IEnumerable<CardGroup> CardGroups => Cards
                .GroupBy(x => x.Value)
                .Select(x => new CardGroup { Count = x.Count(), Card = x.First() })
                .OrderByDescending(x => x.Count);

            public List<HandRank> HandRanks()
            {
                var ranks = new List<HandRank>();

                // straight flush
                if (Straight && Flush)
                    ranks.Add(new HandRank(Rank.StraightFlush, HighCard.Value));

                var grp = CardGroups.First();
                var grp2 = CardGroups.Skip(1).First();

                // four of a kind
                if (grp.Count == 4)
                    ranks.Add(new HandRank(Rank.FourKind, grp.Card.Value));

                // full house
                if (grp.Count == 3 && grp2.Count == 2)
                    ranks.Add(new HandRank(Rank.FullHouse, grp.Card.Value, grp2.Card.Value));

                // flush
                if (Flush)
                    ranks.Add(new HandRank(Rank.Flush));

                // straight
                if (Straight)
                    ranks.Add(new HandRank(Rank.Straight));

                // three of a kind
                if (grp.Count == 3)
                    ranks.Add(new HandRank(Rank.ThreeKind, grp.Card.Value));

                // two pairs
                if (grp.Count == 2 && grp2.Count == 2)
                    ranks.Add(new HandRank(Rank.TwoPair, grp.Card.Value, grp2.Card.Value));

                // one pair
                if (grp.Count == 2)
                    ranks.Add(new HandRank(Rank.OnePair, grp.Card.Value));

                ranks.AddRange(HighToLow.Select(x => new HandRank(Rank.HighCard, x.Value)));
                return ranks;
            }

            public int CompareTo(Hand other)
            {
                if (other == null)
                    return 1;

                return HandRanks()
                    .Zip(other.HandRanks(), (x, y) => x.CompareTo(y))
                    .FirstOrDefault(x => x != 0);
            }
        }

        private class PlayerHands
        {
            public Hand Player1 { get; set; }
            public Hand Player2 { get; set; }
            public string Line { get; set; }
        }

        private int ParseValue(char c)
        {
            switch (c)
            {
                case 'T': return 10;
                case 'J': return 11;
                case 'Q': return 12;
                case 'K': return 13;
                case 'A': return 14;
                default: return (int)char.GetNumericValue(c);
            }
        }

        private Suit ParseSuit(char c)
        {
            switch (c)
            {
                case 'H': return Suit.Hearts;
                case 'D': return Suit.Diamonds;
                case 'S': return Suit.Spades;
                case 'C': return Suit.Clubs;
                default: throw new ArgumentOutOfRangeException();
            }
        }

        private Card ParseCard(string abbr)
        {
            return new Card
            {
                Value = ParseValue(abbr[0]),
                Suit = ParseSuit(abbr[1]),
            };
        }

        private PlayerHands ParseHands(string line)
        {
            var cards = line.Split(new[] { ' ' }, StringSplitOptions.RemoveEmptyEntries);
            var player1 = cards.Take(5).Select(ParseCard);
            var player2 = cards.Skip(5).Select(ParseCard);
            return new PlayerHands
            {
                Player1 = new Hand { Cards = player1.ToArray() },
                Player2 = new Hand { Cards = player2.ToArray() },
                Line = line
            };
        }

        private PlayerHands[] Parse()
        {
            var contents = Resources.Resources.Get("Problem054_pokerhands.txt");
            return contents.Split('\n')
                .Where(x => !string.IsNullOrWhiteSpace(x))
                .Select(x => ParseHands(x.Trim())).ToArray();
        }

        public string Solve()
        {
            var hands = Parse();
            return hands.Count(x => {
                if (x.Player1.CompareTo(x.Player2) == 1)
                    Console.WriteLine(x.Line);
                return x.Player1.CompareTo(x.Player2) == 1;
            }).ToString();
        }
    }
}
