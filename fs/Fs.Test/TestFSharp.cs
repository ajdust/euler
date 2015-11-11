using System.Collections.Generic;
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
            var answer = new Problem002().Solve();
            Assert.AreEqual(Answers["2"], answer);
        }

        [TestMethod]
        public void IsCorrectProblem003()
        {
            var answer = new Problem003().Solve();
            Assert.AreEqual(Answers["3"], answer);
        }

        [TestMethod]
        public void IsCorrectProblem004()
        {
            var answer = new Problem004().Solve();
            Assert.AreEqual(Answers["4"], answer);
        }

        [TestMethod]
        public void IsCorrectProblem005()
        {
            var answer = new Problem005().Solve();
            Assert.AreEqual(Answers["5"], answer);
        }

        [TestMethod]
        public void IsCorrectProblem006()
        {
            var answer = new Problem006().Solve();
            Assert.AreEqual(Answers["6"], answer);
        }

        [TestMethod]
        public void IsCorrectProblem007()
        {
            var answer = new Problem007().Solve();
            Assert.AreEqual(Answers["7"], answer);
        }

        [TestMethod]
        public void IsCorrectProblem008()
        {
            var answer = new Problem008().Solve();
            Assert.AreEqual(Answers["8"], answer);
        }

        [TestMethod]
        public void IsCorrectProblem009()
        {
            var answer = new Problem009().Solve();
            Assert.AreEqual(Answers["9"], answer);
        }

        [TestMethod]
        public void IsCorrectProblem010()
        {
            var answer = new Problem010().Solve();
            Assert.AreEqual(Answers["10"], answer);
        }

        [TestMethod]
        public void IsCorrectProblem011()
        {
            var answer = new Problem011().Solve();
            Assert.AreEqual(Answers["11"], answer);
        }
    }
}
