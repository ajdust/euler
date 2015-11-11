// Author: Aaron Johnson
// Date:   2015-02-14
//
// Solves Euler Problem 19.

using System;
using System.Linq;

namespace Cs.Problems
{
    /// <title>Counting Sundays</title>
    /// <summary>
    /// You are given the following information, but you may prefer to do some research for yourself.
    /// 1 Jan 1900 was a Monday. Thirty days has September, April, June and November.
    /// All the rest have thirty-one, Saving February alone, Which has twenty-eight, rain or shine.
    /// And on leap years, twenty-nine.
    /// A leap year occurs on any year evenly divisible by 4, but not on a century unless it is divisible by 400.
    /// How many Sundays fell on the first of the month during the twentieth century (1 Jan 1901 to 31 Dec 2000)?
    /// </summary>
    /// <answer>171</answer>
    public class Problem019 : IProblem
    {
        public string Solve()
        {
            return Sequences.DateTimeRange("1901-01-01T12:00:00", "2000-12-31T12:00:00", 60 * 60 * 24)
                    .Count(n => n.Day == 1 && n.DayOfWeek == DayOfWeek.Sunday).ToString();
        }
    }
}
