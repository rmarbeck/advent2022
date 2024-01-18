// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class MySuite extends munit.FunSuite:
  test("Day17 : main test on final data"):
    val (score1, score2) = Solver.solve
    assertEquals(score1, "3067")
    assertEquals(score2, "1514369501484")

  test("Day17 : main test on initial data"):
    val (score1, score2) = Solver.solveTest
    assertEquals(score1, "3068")
    assertEquals(score2, "1514285714288")
