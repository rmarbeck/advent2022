// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class MySuite extends munit.FunSuite:
  test("main test on final data"):
    val (score1, score2) = Solver.solve
    assertEquals(score1, "1206825")
    assertEquals(score2, "9608311")

  test("main test on initial data"):
    val (score1, score2) = Solver.solveTest
    assertEquals(score1, "95437")
    assertEquals(score2, "24933642")