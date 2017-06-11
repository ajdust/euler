/**
 * Created by Aaron Johnson on 2017-06-10
 */

package problemtests

import org.junit.Test
import org.junit.Assert.*
import org.junit.BeforeClass
import problems.*
import java.io.File
import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.module.kotlin.readValue
import com.fasterxml.jackson.module.kotlin.registerKotlinModule

class ProblemTests {

    companion object {
        lateinit var answers: Map<String,String>

        @BeforeClass @JvmStatic fun setup() {
            // things to execute once and keep around for the class
            val json = File("../answers.json").readText()
            var mapper = ObjectMapper().registerKotlinModule()
            answers = mapper.readValue(json)
        }
    }

    @Test fun problem01IsCorrect() {
        val problem = Problem01()
        assertEquals("Problem 1 error", answers["1"], problem.solve())
    }

    @Test fun problem2IsCorrect() {
        val problem = Problem02()
        assertEquals("Problem 2 error", answers["2"], problem.solve())
    }

    @Test fun problem3IsCorrect() {
        val problem = Problem03()
        assertEquals("Problem 3 error", answers["3"], problem.solve())
    }

    @Test fun problem4IsCorrect() {
        val problem = Problem04()
        assertEquals("Problem 4 error", answers["4"], problem.solve())
    }

    @Test fun problem5IsCorrect() {
        val problem = Problem05()
        assertEquals("Problem 5 error", answers["5"], problem.solve())
    }

    @Test fun problem6IsCorrect() {
        val problem = Problem06()
        assertEquals("Problem 6 error", answers["6"], problem.solve())
    }

    @Test fun problem7IsCorrect() {
        val problem = Problem07()
        assertEquals("Problem 7 error", answers["7"], problem.solve())
    }

    @Test fun problem8IsCorrect() {
        val problem = Problem08()
        assertEquals("Problem 8 error", answers["8"], problem.solve())
    }

    @Test fun problem9IsCorrect() {
        val problem = Problem09()
        assertEquals("Problem 9 error", answers["9"], problem.solve())
    }

    @Test fun problem10IsCorrect() {
        val problem = Problem10()
        assertEquals("Problem 10 error", answers["10"], problem.solve())
    }
}