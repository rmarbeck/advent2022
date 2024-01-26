import Direction.Up

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

  test("Day22 : Unit tests"):
    val tCanvas = CubeCanvas(NoneRotation, NoneRotation, NoneRotation, NoneRotation, NoneRotation)
    val testDataCanvas = CubeCanvas(NoneRotation, NoneRotation, OneHalf, OneFourth, OneHalf)
    val realDataCanvas = CubeCanvas(NoneRotation, NoneRotation, ThreeFourth, OneHalf, NoneRotation)

    assert(tCanvas.move(One, Up) == (Four, Up), "1")
    assert(tCanvas.move(Two, Right) == (Six, Up), "2")
    assert(tCanvas.move(Two, Left) == (Five, Up), "3")
    assert(tCanvas.move(Three, Right) == (Six, Left), "4")
    assert(tCanvas.move(Three, Left) == (Five, Right), "5")

    assert(testDataCanvas.move(One, Up) == (Four, Down), "10")
    assert(testDataCanvas.move(Two, Right) == (Six, Down), "12")
    assert(testDataCanvas.move(Three, Down) == (Four, Up), "13")
    assert(testDataCanvas.move(Three, Right) == (Six, Right), "14")
    assert(testDataCanvas.move(Three, Left) == (Five, Up), s"15 ${testDataCanvas.move(Three, Left)}")
    assert(testDataCanvas.move(Five, Up) == (One, Right), "16")
    assert(testDataCanvas.move(Five, Down) == (Three, Right), "17")

    assert(realDataCanvas.move(One, Up) == (Four, Right), s"20 ${realDataCanvas.move(One, Up)}")
    assert(realDataCanvas.move(Four, Left) == (One, Down), s"20b ${realDataCanvas.move(One, Up)}")
    assert(realDataCanvas.move(Six, Up) == (Four, Up), s"21 ${realDataCanvas.move(Six, Up)}")
    assert(realDataCanvas.move(Two, Right) == (Six, Up), "22")
    assert(realDataCanvas.move(Three, Down) == (Four, Left), "23")
    assert(realDataCanvas.move(Three, Right) == (Six, Left), "24")
    assert(realDataCanvas.move(Two, Left) == (Five, Down), "25")