import wholeThing.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.*

class boardSpec extends AnyFlatSpec with Matchers:

  var grid = Vector[Vector[Square]]()
  for i <- 0 to 8 do
    var pom = Vector[Square]()
    for j <- 0 to 8 do
      pom = pom :+ Square((i,j))
    grid = grid :+ pom

  val areas = Vector(
    Area(Vector(grid(0)(0), grid(0)(1)), 3),
    Area(Vector(grid(0)(2), grid(0)(3), grid(0)(4)), 15),
    Area(Vector(grid(0)(5), grid(1)(4), grid(1)(5), grid(2)(4)), 22),
    Area(Vector(grid(0)(6), grid(1)(6)), 4),
    Area(Vector(grid(0)(7), grid(1)(7)), 16),
    Area(Vector(grid(0)(8), grid(1)(8), grid(2)(8), grid(3)(8)), 15),
    Area(Vector(grid(1)(0), grid(1)(1), grid(2)(0), grid(2)(1)), 21),
    Area(Vector(grid(1)(2), grid(1)(3)), 17),
    Area(Vector(grid(2)(2), grid(2)(3), grid(3)(3)), 9),
    Area(Vector(grid(2)(5), grid(3)(5), grid(4)(5)), 8),
    Area(Vector(grid(2)(6), grid(2)(7), grid(3)(6)), 20),
    Area(Vector(grid(3)(0), grid(4)(0)), 6),
    Area(Vector(grid(3)(1), grid(3)(2)), 14),
    Area(Vector(grid(3)(4), grid(4)(4), grid(5)(4)), 17),
    Area(Vector(grid(3)(7), grid(4)(7), grid(4)(6)), 17),
    Area(Vector(grid(4)(1), grid(4)(2), grid(5)(1)), 13),
    Area(Vector(grid(4)(3), grid(5)(3), grid(6)(3)), 20),
    Area(Vector(grid(4)(8), grid(5)(8)), 12),
    Area(Vector(grid(5)(0), grid(6)(0), grid(7)(0), grid(8)(0)), 27),
    Area(Vector(grid(5)(2), grid(6)(2), grid(6)(1)), 6),
    Area(Vector(grid(5)(5), grid(6)(5), grid(6)(6)), 20),
    Area(Vector(grid(5)(6), grid(5)(7)), 20),
    Area(Vector(grid(6)(4), grid(7)(4), grid(7)(3), grid(8)(3)), 10),
    Area(Vector(grid(6)(7), grid(6)(8), grid(7)(7), grid(7)(8)), 14),
    Area(Vector(grid(7)(1), grid(8)(1)), 8),
    Area(Vector(grid(7)(2), grid(8)(2)), 16),
    Area(Vector(grid(7)(5), grid(7)(6)), 15),
    Area(Vector(grid(8)(4), grid(8)(5), grid(8)(6)), 13),
    Area(Vector(grid(8)(7), grid(8)(8)), 17)
  )

  var rows = Vector[Vector[Square]]()
  for i <- grid.indices do //rows and colums
    rows = rows :+ grid(i)
    var pom = Vector[Square]()
    for j <- grid.indices do
      pom = pom :+ grid(j)(i)
    rows = rows :+ pom

  var pom = Vector[Vector[Square]]()
  for i <- grid.indices do
    pom = pom :+ grid(i)
    if (i+1) % 3 == 0 then
      var pom2 = Vector[Vector[Square]]()
      var pom3 = Vector[Vector[Square]]()
      var pom4 = Vector[Vector[Square]]()
      for j <- pom do
        val a = j.splitAt(3)(0)
        val b = j.splitAt(3)(1)
        pom2 = pom2 :+ a
        pom3 = pom3 :+ b.splitAt(3)(0)
        pom4 = pom4 :+ b.splitAt(3)(1)
      for j <- 0 to 2 do
        rows = rows :+ pom2.flatten
        rows = rows :+ pom3.flatten
        rows = rows :+ pom4.flatten
      pom = Vector[Vector[Square]]()

  var nines = Vector[OneToNine]()
  for i <- rows do
    nines = nines :+ OneToNine(i)
  for nine <- nines do
    nine.assignToSquare()
    


  private val board = Board(grid, areas, nines)

  "getNeighbours" should "return correct neighbours" in {
    val test = Seq((grid(1)(2), Vector[Square](grid(0)(2), grid(1)(1), grid(2)(2), grid(1)(3))),
      (grid(0)(0), Vector[Square](grid(1)(0), grid(0)(1))),
      (grid(6)(0), Vector[Square](grid(5)(0), grid(7)(0), grid(6)(1))))

    for (square, answer) <- test do
      withClue("For input: " + square){
        board.getNeighbours(square) should equal (answer)
      }
  }

  "isNeighbour" should "return correct booleans" in {
    val testRight =
      Seq(
        (grid(1)(2), Vector[Square](grid(0)(2), grid(1)(1), grid(2)(2), grid(1)(3))),
        (grid(0)(0), Vector[Square](grid(1)(0), grid(0)(1))),
        (grid(6)(0), Vector[Square](grid(5)(0), grid(7)(0), grid(6)(1)))
      )

    val testWrong =
      Seq(
        (grid(1)(2), Vector[Square](grid(7)(5), grid(4)(3), grid(2)(3), grid(0)(1))),
        (grid(0)(0), Vector[Square](grid(1)(1), grid(7)(1))),
        (grid(6)(0), Vector[Square](grid(6)(2), grid(7)(1), grid(0)(0)))
      )

    for (square, answer) <- testRight do
      withClue("For input: " + square + "and right answers"){
        for one <- answer do
          board.isNeighour(square, one) should equal (true)
      }

    for (square, answer) <- testWrong do
      withClue("For input: " + square + "and wrong answers"){
        for one <- answer do
          board.isNeighour(square, one) should equal (false)
      }
  }
  
    "squareGetNeighbouringAreas" should "return empty vectors when squares aren't assigned to areas" in {
    val test = Seq(
      (grid(1)(2), 
       Vector[Vector[Square]](
         Vector(grid(1)(0), grid(1)(1), grid(2)(0), grid(2)(1)), 
         Vector(grid(0)(2), grid(0)(3), grid(0)(4)),
         Vector(grid(2)(2), grid(2)(3), grid(3)(3))
       )),
      
      (grid(0)(0), 
       Vector[Vector[Square]](
         Vector(grid(1)(0), grid(1)(1), grid(2)(0), grid(2)(1))
       )),
        
      (grid(5)(5), 
       Vector[Vector[Square]](
         Vector(grid(3)(4), grid(4)(4), grid(5)(4)),
         Vector(grid(2)(5), grid(3)(5), grid(4)(5)),
         Vector(grid(5)(6), grid(5)(7))
       )
      )
    )
    for (square, answer) <- test do
      for squares <- answer  do
        board.squareGetNeighourAreas(square) should equal (Vector[Area]())
  } 
  


  "squareGetNeighbouringAreas" should "return correct areas" in {
    for area <- areas do
      for square <- area.squares do
        square.setArea(area)
    val test = Seq(
      (grid(1)(2), 
       Vector[Vector[Square]](
         Vector(grid(1)(0), grid(1)(1), grid(2)(0), grid(2)(1)), 
         Vector(grid(0)(2), grid(0)(3), grid(0)(4)),
         Vector(grid(2)(2), grid(2)(3), grid(3)(3))
       )),
      
      (grid(0)(0), 
       Vector[Vector[Square]](
         Vector(grid(1)(0), grid(1)(1), grid(2)(0), grid(2)(1))
       )),
        
      (grid(5)(5), 
       Vector[Vector[Square]](
         Vector(grid(3)(4), grid(4)(4), grid(5)(4)),
         Vector(grid(2)(5), grid(3)(5), grid(4)(5)),
         Vector(grid(5)(6), grid(5)(7))
       )
      )
    )

    for (square, answer) <- test do
      withClue("For input: " + square){
        board.squareGetNeighourAreas(square).length should equal (answer.length)
        for squares <- answer  do
          board.squareGetNeighourAreas(square).map(x => x.squares) should contain (squares)
      }
  } 


  "areaGetNeighbouringAreas" should "return correct areas" in {
    for area <- areas do
      for square <- area.squares do
        square.setArea(area)
    val test = Seq(
      (grid(5)(0).getArea.get,
       Vector[Vector[Square]](
         Vector(grid(3)(0), grid(4)(0)),
         Vector(grid(4)(1), grid(4)(2), grid(5)(1)),
         Vector(grid(5)(2), grid(6)(2), grid(6)(1)),
         Vector(grid(7)(1), grid(8)(1))
       )),

      (grid(6)(4).getArea.get,
       Vector[Vector[Square]](
         Vector(grid(7)(2), grid(8)(2)),
         Vector(grid(4)(3), grid(5)(3), grid(6)(3)),
         Vector(grid(3)(4), grid(4)(4), grid(5)(4)),
         Vector(grid(5)(5), grid(6)(5), grid(6)(6)),
         Vector(grid(7)(5), grid(7)(6)),
         Vector(grid(8)(4), grid(8)(5), grid(8)(6))
       )),

      (grid(1)(2).getArea.get,
       Vector[Vector[Square]](
         Vector(grid(1)(0), grid(1)(1), grid(2)(0), grid(2)(1)),
         Vector(grid(0)(2), grid(0)(3), grid(0)(4)),
         Vector(grid(0)(5), grid(1)(4), grid(1)(5), grid(2)(4)),
         Vector(grid(2)(2), grid(2)(3), grid(3)(3))
       )
      )
    )

    for (area, answer) <- test do
      withClue("For input: " + areas){
        board.areaGetNeighbourAreas(area).length should equal (answer.length)
        for squares <- answer  do
          board.areaGetNeighbourAreas(area).map(x => x.squares) should contain (squares)
      }
  }