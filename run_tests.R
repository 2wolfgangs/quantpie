require('RUnit')

source('momentum.R')

test.suite <- defineTestSuite("testfunction",
                              dirs = file.path("tests"),
                              testFileRegexp = '^\\d+\\.R')

test.result <- runTestSuite(test.suite)

printTextProtocol(test.result)