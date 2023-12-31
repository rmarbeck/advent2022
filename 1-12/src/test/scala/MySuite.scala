// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class MySuite extends munit.FunSuite:
  test("1-12 : main test on final data"):
    val (score1, score2) = Solver.solve
    assertEquals(score1, "71502")
    assertEquals(score2, "208191")

  test("1-12 : main test on initial data"):
    val (score1, score2) = Solver.solveTest
    assertEquals(score1, "24000")
    assertEquals(score2, "45000")
