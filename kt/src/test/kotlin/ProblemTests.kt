/**
 * Created by Aaron Johnson on 2017-06-10
 * Updated 2018-12-27
 */

package problemtests

import org.junit.jupiter.api.Test
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.BeforeAll
import problems.*
import java.io.File
import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.module.kotlin.readValue
import com.fasterxml.jackson.module.kotlin.registerKotlinModule

class ProblemTests {

    companion object {
        lateinit var answers: Map<String,String>

        @BeforeAll @JvmStatic fun setup() {
            // things to execute once and keep around for the class
            val json = File("../answers.json").readText()
            val mapper = ObjectMapper().registerKotlinModule()
            answers = mapper.readValue(json)
        }
    }

    @Test fun problem01IsCorrect() {
        val problem = Problem01()
        assertEquals(answers["1"], problem.solve(), "Problem 1 error")
    }

    @Test fun problem2IsCorrect() {
        val problem = Problem02()
        assertEquals(answers["2"], problem.solve(), "Problem 2 error")
    }

    @Test fun problem3IsCorrect() {
        val problem = Problem03()
        assertEquals(answers["3"], problem.solve(), "Problem 3 error")
    }

    @Test fun problem4IsCorrect() {
        val problem = Problem04()
        assertEquals(answers["4"], problem.solve(), "Problem 4 error")
    }

    @Test fun problem5IsCorrect() {
        val problem = Problem05()
        assertEquals(answers["5"], problem.solve(), "Problem 5 error")
    }

    @Test fun problem6IsCorrect() {
        val problem = Problem06()
        assertEquals(answers["6"], problem.solve(), "Problem 6 error")
    }

    @Test fun problem7IsCorrect() {
        val problem = Problem07()
        assertEquals(answers["7"], problem.solve(), "Problem 7 error")
    }

    @Test fun problem8IsCorrect() {
        val problem = Problem08()
        assertEquals(answers["8"], problem.solve(), "Problem 8 error")
    }

    @Test fun problem9IsCorrect() {
        val problem = Problem09()
        assertEquals(answers["9"], problem.solve(), "Problem 9 error")
    }

    @Test fun problem10IsCorrect() {
        val problem = Problem10()
        assertEquals(answers["10"], problem.solve(), "Problem 10 error"  )
    }
}