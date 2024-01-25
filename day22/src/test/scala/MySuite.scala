// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class MySuite extends munit.FunSuite:
  test("Day22 : main test on final data"):
    val (score1, score2) = Solver.solve
    assertEquals(score1, "11464")
    assertEquals(score2, "197122")

  test("Day22 : main test on initial data"):
    val (score1, score2) = Solver.solveTest
    assertEquals(score1, "6032")
    assertEquals(score2, "5031")
