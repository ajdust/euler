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

int main(int argc, char **argv) {
  ::testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}