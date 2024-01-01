// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class MySuite extends munit.FunSuite:
  test("4-12 : main test on final data"):
    val (score1, score2) = Solver.solve
    assertEquals(score1, "657")
    assertEquals(score2, "938")

  test("4-12 : main test on initial data"):
    val (score1, score2) = Solver.solveTest
    assertEquals(score1, "2")
    assertEquals(score2, "4")
