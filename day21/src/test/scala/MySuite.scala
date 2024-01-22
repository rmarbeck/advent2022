// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class MySuite extends munit.FunSuite:
  test("Day21 : main test on final data"):
    val (score1, score2) = Solver.solve
    assertEquals(score1, "22382838633806")
    assertEquals(score2, "3099532691300")

  test("Day21 : main test on initial data"):
    val (score1, score2) = Solver.solveTest
    assertEquals(score1, "152")
    assertEquals(score2, "301")
