/**
  * Created by aaron on 8/14/2016.
  * Run tests with 'sbt test'
  */

import problems._

import java.io.File
import java.util.Scanner

import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import com.fasterxml.jackson.module.scala.experimental.ScalaObjectMapper
import org.scalatest._

class ProblemTests extends fixture.FunSuite {

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

  test("1 is correct") { answers =>
    val solution = new Problem001().solve
    assert(solution === answers("1"))
  }

  test("2 is correct") { answers =>
    val solution = new Problem002().solve
    assert(solution === answers("2"))
  }

  test("3 is correct") { answers =>
    val solution = new Problem003().solve
    assert(solution === answers("3"))
  }

  test("4 is correct") { answers =>
    val solution = new Problem004().solve
    assert(solution === answers("4"))
  }

  test("5 is correct") { answers =>
    val solution = new Problem005().solve
    assert(solution === answers("5"))
  }

  test("6 is correct") { answers =>
    val solution = new Problem006().solve
    assert(solution === answers("6"))
  }

  test("7 is correct") { answers =>
    val solution = new Problem007().solve
    assert(solution === answers("7"))
  }

  test("8 is correct") { answers =>
    val solution = new Problem008().solve
    assert(solution === answers("8"))
  }

  test("9 is correct") { answers =>
    val solution = new Problem009().solve
    assert(solution === answers("9"))
  }

  test("10 is correct") { answers =>
    val solution = new Problem010().solve
    assert(solution === answers("10"))
  }

  test("11 is correct") { answers =>
    val solution = new Problem011().solve
    assert(solution === answers("11"))
  }

  test("12 is correct") { answers =>
    val solution = new Problem012().solve
    assert(solution === answers("12"))
  }
}
