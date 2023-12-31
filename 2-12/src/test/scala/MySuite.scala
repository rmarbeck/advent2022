// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class MySuite extends munit.FunSuite:
  test("2-12 : main test on final data"):
    val (score1, score2) = Solver.solve
    assertEquals(score1, "13526")
    assertEquals(score2, "14204")

  test("2-12 : main test on initial data"):
    val (score1, score2) = Solver.solveTest
    assertEquals(score1, "15")
    assertEquals(score2, "12")
