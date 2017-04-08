#include <fstream>
#include "gtest/gtest.h"
#include "problems.h"
#include "json.hpp"
using json = nlohmann::json;

class ProblemsTest : public ::testing::Test {
protected:
    ProblemsTest() {
        std::ifstream answers_file("../../answers.json");
        answers_file >> answers;
    }

    json answers;
};

TEST_F(ProblemsTest, Problem_1_IsCorrect) {
    auto answer = Problems::problem01();
    EXPECT_EQ(answer, answers["1"]);
}

TEST_F(ProblemsTest, Problem_2_IsCorrect) {
    auto answer = Problems::problem02();
    EXPECT_EQ(answer, answers["2"]);
}

TEST_F(ProblemsTest, Problem_3_IsCorrect) {
    auto answer = Problems::problem03();
    EXPECT_EQ(answer, answers["3"]);
}

TEST_F(ProblemsTest, Problem_4_IsCorrect) {
    auto answer = Problems::problem04();
    EXPECT_EQ(answer, answers["4"]);
}

TEST_F(ProblemsTest, Problem_5_IsCorrect) {
    auto answer = Problems::problem05();
    EXPECT_EQ(answer, answers["5"]);
}

TEST_F(ProblemsTest, Problem_6_IsCorrect) {
    auto answer = Problems::problem06();
    EXPECT_EQ(answer, answers["6"]);
}

TEST_F(ProblemsTest, Problem_7_IsCorrect) {
    auto answer = Problems::problem07();
    EXPECT_EQ(answer, answers["7"]);
}

TEST_F(ProblemsTest, Problem_8_IsCorrect) {
    auto answer = Problems::problem08();
    EXPECT_EQ(answer, answers["8"]);
}

TEST_F(ProblemsTest, Problem_9_IsCorrect) {
    auto answer = Problems::problem09();
    EXPECT_EQ(answer, answers["9"]);
}

TEST_F(ProblemsTest, Problem_10_IsCorrect) {
    auto answer = Problems::problem10();
    EXPECT_EQ(answer, answers["10"]);
}

int main(int argc, char **argv) {
  ::testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}