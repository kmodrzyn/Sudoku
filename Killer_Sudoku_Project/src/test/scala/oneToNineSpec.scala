import wholeThing.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.*

import scala.collection.immutable

class oneToNineSpec extends AnyFlatSpec with Matchers:
  val squares = Vector(Square((0,0)), Square((0,1)), Square((0,2)), Square((1,0)), Square((1,1)), Square((1,2)),
  Square((2,0)), Square((2,1)), Square((2,2)))

  "Vector 'left'" should "have correct contents after series of commands" in{
    val nine = OneToNine(squares)
    nine.assignToSquare()
    squares(0).setNumber(7)
    withClue(s"when to ${squares(0)} 7 is added"){
    nine.numbersLeft should equal (Vector(1,2,3,4,5,6,8,9))
    }
    squares(1).setNumber(3)
    withClue(s"when to ${squares(1)} 3 is added"){
    nine.numbersLeft should equal (Vector(1,2,4,5,6,8,9))
    }
    squares(2).setNumber(2)
    withClue(s"when to ${squares(2)} 2 is added"){
    nine.numbersLeft should equal (Vector(1,4,5,6,8,9))
    }
    squares(3).setNumber(9)
    withClue(s"when to ${squares(3)} 9 is added"){
    nine.numbersLeft should equal (Vector(1,4,5,6,8))
    }
    squares(1).delete()
    withClue(s"when from ${squares(1)} number is removed"){
    nine.numbersLeft should equal (Vector(1,3,4,5,6,8))
    }
    squares(0).delete()
    withClue(s"when from ${squares(0)} number is removed"){
    nine.numbersLeft should equal (Vector(1,3,4,5,6,7,8))
    }
    squares(0).setNumber(1)
    withClue(s"when to ${squares(0)} 1 is added"){
    nine.numbersLeft should equal (Vector(3,4,5,6,7,8))
    }
    squares(1).setNumber(3)
    withClue(s"when to ${squares(1)} 3 is added"){
    nine.numbersLeft should equal (Vector(4,5,6,7,8))
    }
    squares(4).setNumber(4)
    withClue(s"when to ${squares(4)} 4 is added"){
    nine.numbersLeft should equal (Vector(5,6,7,8))
    }
    squares(5).setNumber(5)
    withClue(s"when to ${squares(5)} 5 is added"){
    nine.numbersLeft should equal (Vector(6,7,8))
    }
    squares(6).setNumber(6)
    withClue(s"when to ${squares(6)} 6 is added"){
    nine.numbersLeft should equal (Vector(7,8))
    }
    squares(7).setNumber(7)
    withClue(s"when to ${squares(7)} 7 is added"){
    nine.numbersLeft should equal (Vector(8))
    }
    squares(8).setNumber(8)
    withClue(s"when to ${squares(8)} 8 is added"){
    nine.numbersLeft should equal (Vector())
    }
    withClue(s"when vector is empty: "){
    nine.isCompleted should equal (true)
    }
  }