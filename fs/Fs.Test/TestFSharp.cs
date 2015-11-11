using System.Collections.Generic;
using eulerfsharp;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Newtonsoft.Json;

namespace Fs.Test
{
    [TestClass]
    public class TestFSharp
    {
        public Dictionary<string, string> Answers;

        [TestInitialize]
        public void Initialize()
        {
            var json = System.IO.File.ReadAllText(@"../../../../answers.json");
            Answers = JsonConvert.DeserializeObject<Dictionary<string, string>>(json);
        }

        [TestMethod]
        public void IsCorrectProblem001()
        {
            var answer = new Problem001().Solve();
            Assert.AreEqual(Answers["1"], answer);
        }

        [TestMethod]
        public void IsCorrectProblem002()
        {
            var answer = Problem002.Solve(4000000).ToString();
            Assert.AreEqual(Answers["2"], answer);
        }

        [TestMethod]
        public void IsCorrectProblem003()
        {
            var answer = Problem003.Solve(600851475143).ToString();
            Assert.AreEqual(Answers["3"], answer);
        }

        [TestMethod]
        public void IsCorrectProblem004()
        {
            var answer = Problem004.Solve.ToString();
            Assert.AreEqual(Answers["4"], answer);
        }

        [TestMethod]
        public void IsCorrectProblem005()
        {
            var answer = Problem005.Solve(20).ToString();
            Assert.AreEqual(Answers["5"], answer);
        }

        [TestMethod]
        public void IsCorrectProblem006()
        {
            var answer = Problem006.Solve(100).ToString();
            Assert.AreEqual(Answers["6"], answer);
        }

        [TestMethod]
        public void IsCorrectProblem007()
        {
            var answer = Problem007.Solve(10000).ToString();
            Assert.AreEqual(Answers["7"], answer);
        }

        [TestMethod]
        public void IsCorrectProblem008()
        {
            var answer = Problem008.Solve.ToString();
            Assert.AreEqual(Answers["8"], answer);
        }

        [TestMethod]
        public void IsCorrectProblem009()
        {
            var answer = Problem009.Solve.ToString();
            Assert.AreEqual(Answers["9"], answer);
        }

        [TestMethod]
        public void IsCorrectProblem010()
        {
            var answer = Problem010.Solve(2000000).ToString();
            Assert.AreEqual(Answers["10"], answer);
        }
    }
}
