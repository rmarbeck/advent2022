// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class MySuite extends munit.FunSuite:
  test("Day20 : main test on final data"):
    val (score1, score2) = Solver.solve
    assertEquals(score1, "8302")
    assertEquals(score2, "656575624777")

  test("Day20 : main test on initial data"):
    val (score1, score2) = Solver.solveTest
    assertEquals(score1, "3")
    assertEquals(score2, "1623178306")
