module eulerTests;
import problems;

import std.json;
import std.file;
import std.stdio;
import std.conv;

void main()
{
    auto answer = problem12();
    writeln(answer);
}

// ran with `rdmd -unittest --main tests.d` (add extra '..' to json path)
// or with `dub test`
class Tester
{
    unittest
    {
        void assertEqual(T)(string message, T expected, T actual)
        {
            if (expected != actual)
                writeln("NOT EQUAL " ~ message ~ ":"
                        ~ "\n  expected " ~ to!string(expected)
                        ~ "\n    actual " ~ to!string(actual));
            assert(expected == actual);
            writeln("Test for " ~ message ~ " passed");
        }

        auto answersJsonText = readText("../../answers.json");
        auto json = parseJSON(answersJsonText);

        assertEqual("problem01()", json["1"].str, problem01());
        assertEqual("problem02()", json["2"].str, problem02());
        assertEqual("problem03()", json["3"].str, problem03());
        assertEqual("problem04()", json["4"].str, problem04());
        assertEqual("problem05()", json["5"].str, problem05());
        assertEqual("problem06()", json["6"].str, problem06());
        assertEqual("problem07()", json["7"].str, problem07());
        assertEqual("problem08()", json["8"].str, problem08());
        assertEqual("problem09()", json["9"].str, problem09());
        assertEqual("problem10()", json["10"].str, problem10());
        assertEqual("problem11()", json["11"].str, problem11());
        assertEqual("problem12()", json["12"].str, problem12());
    }
}
