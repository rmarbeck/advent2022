// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class MySuite extends munit.FunSuite:
  test("main test on final data"):
    val (score1, score2) = Solver.solve
    assertEquals(score1, "8072")
    assertEquals(score2, "2567")

  test("main test on initial data"):
    val (score1, score2) = Solver.solveTest
    assertEquals(score1, "157")
    assertEquals(score2, "70")
