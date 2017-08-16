module eulerTests;
import problems;

// ran with rdmd -unittest --main tests.d
class Tester
{
    version(unittest)
    {
        import std.json;
        import std.file;
        import std.stdio;
    }

    /// Problem 1 test
    unittest
    {
        auto answersJsonText = readText("../answers.json");
        auto json = parseJSON(answersJsonText);

        assert(json["1"].str == problem01());
        assert(json["2"].str == problem02());
        assert(json["3"].str == problem03());
        assert(json["4"].str == problem04());
        assert(json["5"].str == problem05());
        assert(json["6"].str == problem06());
        assert(json["7"].str == problem07());
        assert(json["8"].str == problem08());
        assert(json["9"].str == problem09());
        assert(json["10"].str == problem10());
    }
}