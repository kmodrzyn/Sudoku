import wholeThing.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.*

import scala.collection.immutable

class areaSpec extends AnyFlatSpec with Matchers:

  "isInArea" should "return correct booleans" in {

    val squares = Vector[Square](
    Square((0,0)), Square((0, 1)), Square((0, 2)),
    Square((4,3)), Square((2, 7)), Square((8, 8)), Square((0,3))
    )

    val area = Area(Vector(squares(0), squares(1), squares(2)), 8)

    val testRight = Vector[Square](squares(0), squares(1), squares(2))
    val testWrong = Vector[Square](squares(3), squares(4), squares(5), squares(6))
    for square <- testRight do
      withClue("For input: " + square){
        area.isInArea(square) should equal (true)
      }
    for square <- testWrong do
      withClue("For input: " + square){
        area.isInArea(square) should equal (false)
      }
  }

  "possibleCombinations" should "return combinations of the correct length" in {
    val answers = Vector(
      (Area(Vector(Square((0,0)), Square((0, 1)), Square((0, 2))), 8),
      Vector(
          Vector(1, 2, 5),
          Vector(3, 4, 1)
      )),
      (Area(Vector(Square((0,0)), Square((0, 1)), Square((0, 2)), Square((0, 3))), 10),
      Vector(
          Vector(1, 2, 3, 4)
      )),
      (Area(Vector(Square((0,0)), Square((0, 1)), Square((0, 2)), Square((0, 3))), 20),
      Vector(
          Vector(1, 2, 8, 9),
          Vector(1, 3, 7, 9),
          Vector(1, 4, 6, 9),
          Vector(1, 5, 6, 8),
          Vector(2, 3, 6, 9),
          Vector(2, 3, 7, 8),
          Vector(2, 4, 5, 9),
          Vector(2, 4, 6, 8),
          Vector(2, 5, 6, 7),
          Vector(3, 4, 5, 8),
          Vector(3, 4, 6, 7)
      )),
      (Area(Vector(Square((0,0)), Square((0, 1)), Square((0, 2))), 15),
      Vector(
          Vector(1, 5, 9),
          Vector(1, 6, 8),
          Vector(2, 4, 9),
          Vector(2, 5, 8),
          Vector(2, 6, 7),
          Vector(3, 4, 8),
          Vector(3, 5, 7),
          Vector(4, 5, 6),
      ))
    )
    for (area, answer) <- answers do
      val output = area.possibleCombinations()
      withClue("Output's size should be bigger than 0"){
        output.length should not be equal (0)
      }
      for i <- output do
        withClue("For combination: " + i + s" from ${area}"){
          i.length should equal (answer(0).length)
        }

  }

  "possibleCombinations" should "return correct combinations summing up to the right int" in {
    val answers = Vector(
      (Area(Vector(Square((0,0)), Square((0, 1)), Square((0, 2))), 8),
      Vector(
          Vector(1, 2, 5),
          Vector(1, 3, 4)
      )),
      (Area(Vector(Square((0,0)), Square((0, 1)), Square((0, 2)), Square((0, 3))), 10),
      Vector(
          Vector(1, 2, 3, 4)
      )),
      (Area(Vector(Square((0,0)), Square((0, 1)), Square((0, 2)), Square((0, 3))), 20),
      Vector(
          Vector(1, 2, 8, 9),
          Vector(1, 3, 7, 9),
          Vector(1, 4, 6, 9),
          Vector(1, 5, 6, 8),
          Vector(2, 3, 6, 9),
          Vector(2, 3, 7, 8),
          Vector(2, 4, 5, 9),
          Vector(2, 4, 6, 8),
          Vector(2, 5, 6, 7),
          Vector(3, 4, 5, 8),
          Vector(3, 4, 6, 7)
      )),
      (Area(Vector(Square((0,0)), Square((0, 1)), Square((0, 2))), 15),
      Vector(
          Vector(1, 5, 9),
          Vector(1, 6, 8),
          Vector(2, 4, 9),
          Vector(2, 5, 8),
          Vector(2, 6, 7),
          Vector(3, 4, 8),
          Vector(3, 5, 7),
          Vector(4, 5, 6),
      ))
    )
    for (area, answer) <- answers do
      val output = area.possibleCombinations()
      for i <- answer do
        withClue("For combination: " + i + s" from ${area}"){
          output should contain (i)
        }

  }
