import wholeThing.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.*
import scala.collection.immutable

class gameSpec extends AnyFlatSpec with Matchers:

  val game = Game()

  game.loadBoard("./example_boards/example_board.json")

  "loadBoard" should "load a board correctly" in {
    var grid = Vector[Vector[Square]]()
    for i <- 0 to 8 do
      var pom = Vector[Square]()
        for j <- 0 to 8 do
          pom = pom :+ Square((i,j))
      grid = grid :+ pom

    grid(0)(1).setNumber(1)
    grid(2)(3).setNumber(4)

    val answerAreas = Vector(
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


    withClue("Output board should have the same lenght as the answer"){
      game.board.areas.length should equal (answerAreas.length)
    }
    withClue("Output board should have the same filled squares as the answer"){
      for i <- game.board.squares.indices do
        for j <- game.board.squares(i).indices do
          if i == 0 && j == 1 then
            game.board.squares(i)(j).getNumber should equal (Some(1))
          else if i == 2 && j == 3 then
            game.board.squares(i)(j).getNumber should equal (Some(4))
          else
            game.board.squares(i)(j).getNumber should equal (None)
    }
    for area <- answerAreas do
      withClue("Output board should contain the same areas as answer"){
      game.board.areas.map(_.squares.map(_.pos)) should contain (area.squares.map(_.pos))
      }

  }

  "addValue" should "work properly" in {
    game.addValue(0, 0, 9)
    game.board.squares(0)(0).getNumber should equal (Some(9))
  }

  "removeValue" should "work properly" in {
    game.removeValue(0, 0)
    game.board.squares(0)(0).getNumber should equal (None)
  }

  "possibleFits" should "return correct numbers avaible for selected square" in {
    game.addValue(0, 3, 9)
    game.addValue(0, 7, 8)
    game.addValue(2, 2, 3)
    game.addValue(2, 1, 4)
    game.addValue(4, 0, 2)
    game.addValue(5, 0, 1)
    var fits = game.possibleFits(0,0)
    fits should equal (Vector(5,6,7))
    game.removeValue(2,2)
    fits = game.possibleFits(0,0)
    fits should equal (Vector(3,5,6,7))

  }

  "possibleCombinationsArea" should "return correct combinations 1" in {
    val emptyAnswer = Vector(
          Vector(3, 8, 9),
          Vector(4, 7, 9),
          Vector(5, 6, 9),
          Vector(5, 7, 8)
      )

    val output = game.possibleCombinationsArea(5,5)
     withClue("for an empty area combinations lengths should be equal"){
        output.length should equal (emptyAnswer.length)
     }
    for vec <- emptyAnswer do
      withClue("output should contain same vectors as answer"){
        output should contain (vec)
      }


  }
  "possibleCombinationsArea" should "return correct combinations 2" in {
     val addedNine = Vector(
          Vector(3, 8),
          Vector(4, 7),
          Vector(5, 6)
      )

     game.addValue(6, 5, 9)
     val output2 = game.possibleCombinationsArea(5,5)
     withClue("for an empty area combinations lengths should be equal"){
        output2.length should equal (addedNine.length)
     }
     for vec <- addedNine do
       withClue("output should contain same vectors as answer"){
        output2 should contain (vec)
       }
  }
  "possibleCombinationsArea" should "return correct combinations 3" in {
    game.addValue(6, 6, 3)
    game.addValue(6, 5, 9)
    val output3 = game.possibleCombinationsArea(5,5)
     withClue("for an empty area combinations lengths should be equal"){
        output3.length should equal (1)
     }
     withClue("output should contain same vectors as answer"){
        output3(0) should equal (Vector(8))
      }
  }

  "colorAreas" should "color areas so that no neighouring areas are the same color" in {
    game.colorAreas()
    for area <- game.board.areas do
      game.board.areaGetNeighbourAreas(area).map(x => x.getColor) should not contain (area.getColor)
  }
