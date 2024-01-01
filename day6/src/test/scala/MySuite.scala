// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class MySuite extends munit.FunSuite:
  test("6-12 : main test on final data"):
    val (score1, score2) = Solver.solve
    assertEquals(score1, "1802")
    assertEquals(score2, "3551")

  test("6-12 : main test on initial data"):
    val (score1, score2) = Solver.solveTest
    assertEquals(score1, "7-5-6-10-11")
    assertEquals(score2, "19-23-23-29-26")