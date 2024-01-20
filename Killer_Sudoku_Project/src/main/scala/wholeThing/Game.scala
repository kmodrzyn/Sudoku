package wholeThing

import io.circe.*
import io.circe.generic.auto.*
import io.circe.parser.*
import io.circe.syntax.*

import java.io.*
import scala.collection.mutable.Buffer
import scala.io.Source


class Game:

  case class Nested(sum: Int, squares: Vector[String])
  case class Areas(areas: Vector[Nested])
  case class Data(filledSquares: Vector[(String, Int)], areas: Vector[Nested])


  /** START set up grid of Squares */
  var grid = Vector[Vector[Square]]()
    for i <- 0 to 8 do
    var pom = Vector[Square]()
      for j <- 0 to 8 do
        pom = pom :+ Square((i,j))
    grid = grid :+ pom
  /** END set up grid of Squares */

  /** START set up OneToNine class */
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
  /** END set up OneToNine class */

  var board = Board(grid, Vector[Area](), nines)

  var errorReason = "Could not load the board"

  def loadBoard(fileName: String): Unit =
    for i <- this.board.squares do
      for j <- i do
        j.clearArea()
        j.delete()
    var values = Vector[((Int, Int),Int)]()
    var areas = Vector[Area]()
    val file = Source.fromFile(fileName)
    try
      val jsonFile = file.getLines
      var json = ""
      for line <- jsonFile do
        json = json + line
      val jObject: Json = parse(json).getOrElse(null)
      val extractedData = jObject.as[Data].getOrElse(Data(Vector(), Vector()))
      for i <- extractedData.filledSquares do
        if i(0).size == 2 then
          val pos1 = i(0)(0).asDigit
          val pos2 = i(0)(1).asDigit
          if pos1 < 9 && pos1 >= 0 && pos2 < 9 && pos2 >= 0 then
            values = values :+ ((pos1, pos2), i(1))
          else
           throw StreamCorruptedException("wrong position on board")
        else
          throw StreamCorruptedException("too many arrgument in a position")
      for i <- values do
        grid(i(0)(0))(i(0)(1)).setNumber(i(1))
      for i <- extractedData.areas do
        var pom = Vector[Square]()
        if i.squares.length <= 4 then
          for j <- i.squares do
            if j.length == 2 then
              val pos1 = j(0).asDigit
              val pos2 = j(1).asDigit
              if pos1 < 9 && pos1 >= 0 && pos2 < 9 && pos2 >= 0 then
                pom = pom :+ grid(pos1)(pos2)
              else
               throw StreamCorruptedException("wrong position on board")
            else
              throw StreamCorruptedException("too many arrgument in a position")
        else
          throw StreamCorruptedException("too big area")
        areas = areas :+ Area(pom, i.sum)
      /** Assign areas to squares */
      for area <- areas do
        for square <- area.squares do
          square.setArea(area)
      if this.board.squares.forall(row => row.forall(_.getArea.isDefined)) then
        this.board = Board(grid, areas, nines)
      else
        for i <- this.board.squares do
          for j <- i do
            j.delete()
        this.board = Board(grid, Vector[Area](), nines)

    catch
      case x: StreamCorruptedException =>
        println("Corrupted file, couldn't load a board")
        errorReason = "Corrupted file, couldn't load a board"
        this.board = Board(grid, Vector[Area](), nines)
      case x: FileNotFoundException =>
        println("Couldn't find desired file")
        errorReason = "Couldn't find desired file"
        this.board = Board(grid, Vector[Area](), nines)
      case x: IOException =>
        println("IO Exception occured")
        errorReason = "IO Exception occured"
        this.board = Board(grid, Vector[Area](), nines)
      case _: Throwable =>
        println("Some other kind of throwable has occured")
        errorReason = "Corrupted file, couldn't load a board for unknown reason"
        this.board = Board(grid, Vector[Area](), nines)
    finally
      file.close()

  def addValue(i: Int, j: Int, value: Int) = this.board.squares(i)(j).setNumber(value)

  def removeValue(i: Int, j: Int) = this.board.squares(i)(j).delete()

  def possibleFits(i: Int, j: Int) =
    val nine = this.board.squares(i)(j).getNine
    var possibleNumbers = 1.to(9).toVector
    for i <- nine do
      possibleNumbers = possibleNumbers.filter(x => i.numbersLeft.contains(x))
    possibleNumbers

  def possibleCombinationsArea(i: Int, j: Int): Vector[Vector[Int]] = 
    val area = this.board.squares(i)(j).getArea
    if area.isDefined then
      area.get.possibleCombinations()
    else
      Vector[Vector[Int]]()

  def colorAreas() =
    var edges = Vector[Vector[Int]]()
    for i <- this.board.areas.indices do
      val pom = this.board.areaGetNeighbourAreas(this.board.areas(i))
      var index = Vector[Int]()
      for j <- pom do
        index = index :+ this.board.areas.indexOf(j)
      edges = edges :+ index
    val colors = Vector[Color](Red, Blue, Yellow, Green)
    // as board is a map, 4 colors should be enough 
    var result = Buffer.fill(this.board.areas.size)(-1)
    var notAvailable = Buffer.fill(this.board.areas.size)(false)

    result(0) = 0

    for i <- 1 until this.board.areas.size do
      for j <- edges(i) do
        if result(j) != -1 then
          notAvailable(result(j)) = true

      var col = 0
      for k <- colors.indices do
        if !notAvailable(k) && col == 0 then
          col = k

      result(i) = col
      for j <- edges(i) do
        if result(j) != -1 then
          notAvailable(result(j)) = false

    for i <- result.indices do
      this.board.areas(i).setColor(colors(result(i)))


  def isWon = this.board.nines.forall(_.isCompleted) && this.board.areas.forall(_.isCompleted)

  def saveBoard(path: String) =
    var filledSquares = Vector[(String, Int)]()
    var formatArea = Vector[Nested]()
    for i <- grid do
      for square <- i do
        if square.getNumber.isDefined then
          filledSquares = filledSquares :+ (s"${square.pos(0)}${square.pos(1)}", square.getNumber.get)
    for area <- this.board.areas do
      var pom = Vector[String]()
      for square <- area.squares do
        pom = pom :+ s"${square.pos(0)}${square.pos(1)}"
      formatArea = formatArea :+ Nested(area.sum, pom)
    val data =  Data(filledSquares, formatArea)
    val fileWriter = FileWriter( File(path))
    fileWriter.write(data.asJson.toString)
    fileWriter.close()

end Game
