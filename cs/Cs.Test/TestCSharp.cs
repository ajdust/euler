using System.Collections.Generic;
using Cs.Problems;
using Newtonsoft.Json;
using Xunit;

namespace Cs.Test
{
    public class TestCSharp
    {
        public Dictionary<string, string> Answers;

        public TestCSharp()
        {
            var json = System.IO.File.ReadAllText(@"../../../../../answers.json");
            Answers = JsonConvert.DeserializeObject<Dictionary<string, string>>(json);
        }

        [Fact]
        public void IsCorrectProblem001()
        {
            var problem = new Problem001();
            Assert.Equal(Answers["1"], problem.Solve());
        }

        [Fact]
        public void IsCorrectProblem002()
        {
            var problem = new Problem002();
            Assert.Equal(Answers["2"], problem.Solve());
        }

        [Fact]
        public void IsCorrectProblem003()
        {
            var problem = new Problem003();
            Assert.Equal(Answers["3"], problem.Solve());
        }

        [Fact]
        public void IsCorrectProblem004()
        {
            var problem = new Problem004();
            Assert.Equal(Answers["4"], problem.Solve());
        }

        [Fact]
        public void IsCorrectProblem005()
        {
            var problem = new Problem005();
            Assert.Equal(Answers["5"], problem.Solve());
        }

        [Fact]
        public void IsCorrectProblem006()
        {
            var problem = new Problem006();
            Assert.Equal(Answers["6"], problem.Solve());
        }

        [Fact]
        public void IsCorrectProblem007()
        {
            var problem = new Problem007();
            Assert.Equal(Answers["7"], problem.Solve());
        }

        [Fact]
        public void IsCorrectProblem008()
        {
            var problem = new Problem008();
            Assert.Equal(Answers["8"], problem.Solve());
        }

        [Fact]
        public void IsCorrectProblem009()
        {
            var problem = new Problem009();
            Assert.Equal(Answers["9"], problem.Solve());
        }

        [Fact]
        public void IsCorrectProblem010()
        {
            var problem = new Problem010();
            Assert.Equal(Answers["10"], problem.Solve());
        }

        [Fact]
        public void IsCorrectProblem011()
        {
            var problem = new Problem011();
            Assert.Equal(Answers["11"], problem.Solve());
        }

        [Fact]
        public void IsCorrectProblem012()
        {
            var problem = new Problem012();
            Assert.Equal(Answers["12"], problem.Solve());
        }

        [Fact]
        public void IsCorrectProblem013()
        {
            var problem = new Problem013();
            Assert.Equal(Answers["13"], problem.Solve());
        }

        [Fact]
        public void IsCorrectProblem014()
        {
            var problem = new Problem014();
            Assert.Equal(Answers["14"], problem.Solve());
        }

        [Fact]
        public void IsCorrectProblem015()
        {
            var problem = new Problem015();
            Assert.Equal(Answers["15"], problem.Solve());
        }

        [Fact]
        public void IsCorrectProblem016()
        {
            var problem = new Problem016();
            Assert.Equal(Answers["16"], problem.Solve());
        }

        [Fact]
        public void IsCorrectProblem017()
        {
            var problem = new Problem017();
            Assert.Equal(Answers["17"], problem.Solve());
        }

        [Fact]
        public void IsCorrectProblem018()
        {
            var problem = new Problem018();
            Assert.Equal(Answers["18"], problem.Solve());
        }

        [Fact]
        public void IsCorrectProblem019()
        {
            var problem = new Problem019();
            Assert.Equal(Answers["19"], problem.Solve());
        }

        [Fact]
        public void IsCorrectProblem020()
        {
            var problem = new Problem020();
            Assert.Equal(Answers["20"], problem.Solve());
        }

        [Fact]
        public void IsCorrectProblem021()
        {
            var problem = new Problem021();
            Assert.Equal(Answers["21"], problem.Solve());
        }

        [Fact]
        public void IsCorrectProblem022()
        {
            var problem = new Problem022();
            Assert.Equal(Answers["22"], problem.Solve());
        }

        [Fact]
        public void IsCorrectProblem023()
        {
            var problem = new Problem023();
            Assert.Equal(Answers["23"], problem.Solve());
        }

        [Fact]
        public void IsCorrectProblem024()
        {
            var problem = new Problem024();
            Assert.Equal(Answers["24"], problem.Solve());
        }

        [Fact]
        public void IsCorrectProblem025()
        {
            var problem = new Problem025();
            Assert.Equal(Answers["25"], problem.Solve());
        }

        [Fact]
        public void IsCorrectProblem026()
        {
            var problem = new Problem026();
            Assert.Equal(Answers["26"], problem.Solve());
        }

        [Fact]
        public void IsCorrectProblem027()
        {
            var problem = new Problem027();
            Assert.Equal(Answers["27"], problem.Solve());
        }

        [Fact]
        public void IsCorrectProblem028()
        {
            var problem = new Problem028();
            Assert.Equal(Answers["28"], problem.Solve());
        }

        [Fact]
        public void IsCorrectProblem029()
        {
            var problem = new Problem029();
            Assert.Equal(Answers["29"], problem.Solve());
        }

        [Fact]
        public void IsCorrectProblem030()
        {
            var problem = new Problem030();
            Assert.Equal(Answers["30"], problem.Solve());
        }

        [Fact]
        public void IsCorrectProblem031()
        {
            var problem = new Problem031();
            Assert.Equal(Answers["31"], problem.Solve());
        }

        [Fact]
        public void IsCorrectProblem032()
        {
            var problem = new Problem032();
            Assert.Equal(Answers["32"], problem.Solve());
        }

        [Fact]
        public void IsCorrectProblem033()
        {
            var problem = new Problem033();
            Assert.Equal(Answers["33"], problem.Solve());
        }

        [Fact]
        public void IsCorrectProblem034()
        {
            var problem = new Problem034();
            Assert.Equal(Answers["34"], problem.Solve());
        }

        [Fact]
        public void IsCorrectProblem035()
        {
            var problem = new Problem035();
            Assert.Equal(Answers["35"], problem.Solve());
        }

        [Fact]
        public void IsCorrectProblem036()
        {
            var problem = new Problem036();
            Assert.Equal(Answers["36"], problem.Solve());
        }

        [Fact]
        public void IsCorrectProblem037()
        {
            var problem = new Problem037();
            Assert.Equal(Answers["37"], problem.Solve());
        }

        [Fact]
        public void IsCorrectProblem038()
        {
            var problem = new Problem038();
            Assert.Equal(Answers["38"], problem.Solve());
        }

        [Fact]
        public void IsCorrectProblem039()
        {
            var problem = new Problem039();
            Assert.Equal(Answers["39"], problem.Solve());
        }

        [Fact]
        public void IsCorrectProblem040()
        {
            var problem = new Problem040();
            Assert.Equal(Answers["40"], problem.Solve());
        }

        [Fact]
        public void IsCorrectProblem041()
        {
            var problem = new Problem041();
            Assert.Equal(Answers["41"], problem.Solve());
        }

        [Fact]
        public void IsCorrectProblem042()
        {
            var problem = new Problem042();
            Assert.Equal(Answers["42"], problem.Solve());
        }

        [Fact]
        public void IsCorrectProblem043()
        {
            var problem = new Problem043();
            Assert.Equal(Answers["43"], problem.Solve());
        }

        [Fact]
        public void IsCorrectProblem044()
        {
            var problem = new Problem044();
            Assert.Equal(Answers["44"], problem.Solve());
        }

        [Fact]
        public void IsCorrectProblem045()
        {
            var problem = new Problem045();
            Assert.Equal(Answers["45"], problem.Solve());
        }

        [Fact]
        public void IsCorrectProblem046()
        {
            var problem = new Problem046();
            Assert.Equal(Answers["46"], problem.Solve());
        }

        [Fact]
        public void IsCorrectProblem047()
        {
            var problem = new Problem047();
            Assert.Equal(Answers["47"], problem.Solve());
        }

        [Fact]
        public void IsCorrectProblem048()
        {
            var problem = new Problem048();
            Assert.Equal(Answers["48"], problem.Solve());
        }

        [Fact]
        public void IsCorrectProblem049()
        {
            var problem = new Problem049();
            Assert.Equal(Answers["49"], problem.Solve());
        }

        [Fact]
        public void IsCorrectProblem050()
        {
            var problem = new Problem050();
            Assert.Equal(Answers["50"], problem.Solve());
        }

        [Fact]
        public void IsCorrectProblem051()
        {
            var problem = new Problem051();
            Assert.Equal(Answers["51"], problem.Solve());
        }

        [Fact]
        public void IsCorrectProblem052()
        {
            var problem = new Problem052();
            Assert.Equal(Answers["52"], problem.Solve());
        }

        [Fact]
        public void IsCorrectProblem053()
        {
            var problem = new Problem053();
            Assert.Equal(Answers["53"], problem.Solve());
        }

        [Fact]
        public void IsCorrectProblem054()
        {
            var problem = new Problem054();
            Assert.Equal(Answers["54"], problem.Solve());
        }

        [Fact]
        public void IsCorrectProblem055()
        {
            var problem = new Problem055();
            Assert.Equal(Answers["55"], problem.Solve());
        }

        [Fact]
        public void IsCorrectProblem056()
        {
            var problem = new Problem056();
            Assert.Equal(Answers["56"], problem.Solve());
        }

        [Fact]
        public void IsCorrectProblem057()
        {
            var problem = new Problem057();
            Assert.Equal(Answers["57"], problem.Solve());
        }

        [Fact]
        public void IsCorrectProblem058()
        {
            var problem = new Problem058();
            Assert.Equal(Answers["58"], problem.Solve());
        }

        [Fact]
        public void IsCorrectProblem059()
        {
            var problem = new Problem059();
            Assert.Equal(Answers["59"], problem.Solve());
        }
    }
}