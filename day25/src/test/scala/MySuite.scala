// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class MySuite extends munit.FunSuite:
  test("Day25 : main test on final data"):
    val (score1, score2) = Solver.solve
    assertEquals(score1, "2=020-===0-1===2=020")
    assertEquals(score2, "Finished")

  test("Day25 : main test on initial data"):
    val (score1, score2) = Solver.solveTest
    assertEquals(score1, "2=-1=0")
    assertEquals(score2, "Finished")
