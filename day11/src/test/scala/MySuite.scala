// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class MySuite extends munit.FunSuite:
  test("Day11 : main test on final data"):
    val (score1, score2) = Solver.solve
    assertEquals(score1, "90294")
    assertEquals(score2, "18170818354")

  test("Day11 : main test on initial data"):
    val (score1, score2) = Solver.solveTest
    assertEquals(score1, "10605")
    assertEquals(score2, "2713310158")
