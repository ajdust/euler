/**
  * Created by aaron on 8/14/2016.
  */
import java.io.File
import java.util.Scanner

import Problems._
import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import com.fasterxml.jackson.module.scala.experimental.ScalaObjectMapper
import org.scalatest._

class ProblemTest extends fixture.FunSuite {

  type FixtureParam = Map[String,String]

  def withFixture(test : OneArgTest): Outcome = {

    // why, Java? why?
    // Reading into text should be one line as in C#, built-in method, c'mon
    val jsonFile = new File("../../answers.json")
    val json = new Scanner(jsonFile).useDelimiter("\\Z").next()
      .replaceAll("\uFEFF", "")

    val mapper = new ObjectMapper() with ScalaObjectMapper
    mapper.registerModule(DefaultScalaModule)
    val answers = mapper.readValue[Map[String,String]](json)

    test(answers)
  }

  test("problem 1 is correct") { answers =>
    val problem = new Problem001
    val solution = problem.solve
    assert(solution === answers("1"))
  }

  test("problem 2 is correct") { answers =>
    val problem = new Problem002
    val solution = problem.solve
    assert(solution === answers("2"))
  }
}
