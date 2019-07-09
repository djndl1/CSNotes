Before writing code, writing unit tests forces you to detail the requirements in a useful fashion. While writing code, unit tests keep from overcoding. When all the test cases pass, the function is complete. When refactoring code, they can help prove that the new version behaves the same way as the old version. When maintaining code, having tests avoids breaking the old code.

The `unittest` unit testing framework supports test automation, sharing of setup and shutdown code for tests, aggregation of tests into collections and independence of the tests from reporting framework.

# Important concepts

- test fixture: represents the preparation needed to perform one or more tests, and any associate cleanup actions. A working environment for the testing code.

- test case: the individual unit of testing. It checks for a specific response to a particular set of inputs.

- test suite: a collection of test cases, test suites, or both. It is used to aggregate tests that should be executed together.

- test runner: a component which orchestrates the executation of tests and provides the outcome to the user.

# Principles and Caveats

A test case should be able to run completely by itself without any human input. Unit test is about automation, determine by itself whether the function it is testing has passed or failed, without a human interpreting the results, and run in isolation, separate from any other test cases. It is better to place test code in separate modules.

Test code should be modified much less frequently than the code it tests. 
