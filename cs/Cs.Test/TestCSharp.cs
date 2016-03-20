using System.Collections.Generic;
using Cs.Problems;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Newtonsoft.Json;

namespace Cs.Test
{
    [TestClass]
    public class TestCSharp
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
            var problem = new Problem001();
            Assert.AreEqual(Answers["1"], problem.Solve());
        }

        [TestMethod]
        public void IsCorrectProblem002()
        {
            var problem = new Problem002();
            Assert.AreEqual(Answers["2"], problem.Solve());
        }

        [TestMethod]
        public void IsCorrectProblem003()
        {
            var problem = new Problem003();
            Assert.AreEqual(Answers["3"], problem.Solve());
        }

        [TestMethod]
        public void IsCorrectProblem004()
        {
            var problem = new Problem004();
            Assert.AreEqual(Answers["4"], problem.Solve());
        }

        [TestMethod]
        public void IsCorrectProblem005()
        {
            var problem = new Problem005();
            Assert.AreEqual(Answers["5"], problem.Solve());
        }

        [TestMethod]
        public void IsCorrectProblem006()
        {
            var problem = new Problem006();
            Assert.AreEqual(Answers["6"], problem.Solve());
        }

        [TestMethod]
        public void IsCorrectProblem007()
        {
            var problem = new Problem007();
            Assert.AreEqual(Answers["7"], problem.Solve());
        }

        [TestMethod]
        public void IsCorrectProblem008()
        {
            var problem = new Problem008();
            Assert.AreEqual(Answers["8"], problem.Solve());
        }

        [TestMethod]
        public void IsCorrectProblem009()
        {
            var problem = new Problem009();
            Assert.AreEqual(Answers["9"], problem.Solve());
        }

        [TestMethod]
        public void IsCorrectProblem010()
        {
            var problem = new Problem010();
            Assert.AreEqual(Answers["10"], problem.Solve());
        }

        [TestMethod]
        public void IsCorrectProblem011()
        {
            var problem = new Problem011();
            Assert.AreEqual(Answers["11"], problem.Solve());
        }

        [TestMethod]
        public void IsCorrectProblem012()
        {
            var problem = new Problem012();
            Assert.AreEqual(Answers["12"], problem.Solve());
        }

        [TestMethod]
        public void IsCorrectProblem013()
        {
            var problem = new Problem013();
            Assert.AreEqual(Answers["13"], problem.Solve());
        }

        [TestMethod]
        public void IsCorrectProblem014()
        {
            var problem = new Problem014();
            Assert.AreEqual(Answers["14"], problem.Solve());
        }

        [TestMethod]
        public void IsCorrectProblem015()
        {
            var problem = new Problem015();
            Assert.AreEqual(Answers["15"], problem.Solve());
        }

        [TestMethod]
        public void IsCorrectProblem016()
        {
            var problem = new Problem016();
            Assert.AreEqual(Answers["16"], problem.Solve());
        }

        [TestMethod]
        public void IsCorrectProblem017()
        {
            var problem = new Problem017();
            Assert.AreEqual(Answers["17"], problem.Solve());
        }

        [TestMethod]
        public void IsCorrectProblem018()
        {
            var problem = new Problem018();
            Assert.AreEqual(Answers["18"], problem.Solve());
        }

        [TestMethod]
        public void IsCorrectProblem019()
        {
            var problem = new Problem019();
            Assert.AreEqual(Answers["19"], problem.Solve());
        }

        [TestMethod]
        public void IsCorrectProblem020()
        {
            var problem = new Problem020();
            Assert.AreEqual(Answers["20"], problem.Solve());
        }

        [TestMethod]
        public void IsCorrectProblem021()
        {
            var problem = new Problem021();
            Assert.AreEqual(Answers["21"], problem.Solve());
        }

        [TestMethod]
        public void IsCorrectProblem022()
        {
            var problem = new Problem022();
            Assert.AreEqual(Answers["22"], problem.Solve());
        }

        [TestMethod]
        public void IsCorrectProblem023()
        {
            var problem = new Problem023();
            Assert.AreEqual(Answers["23"], problem.Solve());
        }

        [TestMethod]
        public void IsCorrectProblem024()
        {
            var problem = new Problem024();
            Assert.AreEqual(Answers["24"], problem.Solve());
        }

        [TestMethod]
        public void IsCorrectProblem025()
        {
            var problem = new Problem025();
            Assert.AreEqual(Answers["25"], problem.Solve());
        }

        [TestMethod]
        public void IsCorrectProblem026()
        {
            var problem = new Problem026();
            Assert.AreEqual(Answers["26"], problem.Solve());
        }

        [TestMethod]
        public void IsCorrectProblem027()
        {
            var problem = new Problem027();
            Assert.AreEqual(Answers["27"], problem.Solve());
        }

        [TestMethod]
        public void IsCorrectProblem028()
        {
            var problem = new Problem028();
            Assert.AreEqual(Answers["28"], problem.Solve());
        }

        [TestMethod]
        public void IsCorrectProblem029()
        {
            var problem = new Problem029();
            Assert.AreEqual(Answers["29"], problem.Solve());
        }

        [TestMethod]
        public void IsCorrectProblem030()
        {
            var problem = new Problem030();
            Assert.AreEqual(Answers["30"], problem.Solve());
        }

        [TestMethod]
        public void IsCorrectProblem031()
        {
            var problem = new Problem031();
            Assert.AreEqual(Answers["31"], problem.Solve());
        }

        [TestMethod]
        public void IsCorrectProblem032()
        {
            var problem = new Problem032();
            Assert.AreEqual(Answers["32"], problem.Solve());
        }

        [TestMethod]
        public void IsCorrectProblem033()
        {
            var problem = new Problem033();
            Assert.AreEqual(Answers["33"], problem.Solve());
        }

        [TestMethod]
        public void IsCorrectProblem034()
        {
            var problem = new Problem034();
            Assert.AreEqual(Answers["34"], problem.Solve());
        }

        [TestMethod]
        public void IsCorrectProblem035()
        {
            var problem = new Problem035();
            Assert.AreEqual(Answers["35"], problem.Solve());
        }

        [TestMethod]
        public void IsCorrectProblem036()
        {
            var problem = new Problem036();
            Assert.AreEqual(Answers["36"], problem.Solve());
        }

        [TestMethod]
        public void IsCorrectProblem037()
        {
            var problem = new Problem037();
            Assert.AreEqual(Answers["37"], problem.Solve());
        }

        [TestMethod]
        public void IsCorrectProblem038()
        {
            var problem = new Problem038();
            Assert.AreEqual(Answers["38"], problem.Solve());
        }

        [TestMethod]
        public void IsCorrectProblem039()
        {
            var problem = new Problem039();
            Assert.AreEqual(Answers["39"], problem.Solve());
        }
    }
}