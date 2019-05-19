library(RUnit)

suite <- defineTestSuite("Euler Problems",
            dirs = file.path("tests"),
            testFileRegexp = 'tests\\.R',
            testFuncRegexp = 'problem.+')

result <- runTestSuite(suite)
printTextProtocol(result)