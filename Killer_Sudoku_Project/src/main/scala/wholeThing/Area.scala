package wholeThing

import scala.collection.mutable.Buffer

class Area(val squares: Vector[Square], val sum: Int):

  private var color : Option[Color] = Option.empty[Color]

  private var displayed = false

  def isDisplayed: Boolean = this.displayed

  def display() = this.displayed = true
  
  def isCompleted: Boolean =
    var partialSum = 0
    if this.squares.forall(_.getNumber.isDefined) then
      for i <- squares do
        partialSum = partialSum + i.getNumber.get
      if partialSum == sum then true else false
    else
      false
  
  def getColor = this.color
  
  def setColor(colour: Color) = this.color = Some(colour)
  
  def isInArea(square: Square): Boolean = this.squares.contains(square)
  
  private def sums(array: Buffer[Int], sum: Int, curSum: Int, begin: Int, result: Buffer[Buffer[Int]]): Unit =
    if sum == curSum then
      val pom = array
      result.append(pom.clone())

    for i <- begin until sum do
      val temp = curSum + i
      if temp <= sum then
        array.append(i)
        sums(array, sum, temp, i, result)
        array.remove(array.length - 1)

  def possibleCombinations() =
    var alreadyFilled = Vector[Int]()
    var size = 0
    var partialSum = this.sum
    for i <- this.squares do
      if i.getNumber.isEmpty then
        size += 1
      else
        alreadyFilled = alreadyFilled :+ i.getNumber.get
        partialSum = partialSum - i.getNumber.get
    val array = Buffer[Int]()
    val result = Buffer[Buffer[Int]]()
    sums(array, partialSum, 0, 1, result)
    result.append(Buffer(partialSum))
    var resultTwo = Vector[Vector[Int]]()
    for i <- result do
      val l = i.toSet
      if l.size == size && l.sum == partialSum && l.forall(x => x < 10) && !resultTwo.contains(l) && l.forall(x => !alreadyFilled.contains(x)) then
        resultTwo = resultTwo :+ i.toVector.sorted
    resultTwo.sortBy(_.head)
        
  override def toString: String = s"Area: ${squares}"

end Area

