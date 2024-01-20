package wholeThing


class OneToNine(val squares: Vector[Square]):

  private var left = 1.to(9).toVector
  private var inNine = Vector[Int]()

  def addNumber(square: Square) =
    if square.getNumber.isDefined then
      inNine = inNine :+ square.getNumber.get
      left = 1.to(9).filterNot(x => inNine.contains(x)).toVector
      
  def removeNumber(square: Square) =
    if square.getNumber.isDefined then
      val toRemove = inNine.indexOf(square.getNumber.get)
      inNine = inNine.take(toRemove) ++ inNine.drop(toRemove + 1)
      left = 1.to(9).filterNot(x => inNine.contains(x)).toVector

  def isCompleted: Boolean = left.isEmpty
  
  def numbersLeft: Vector[Int] = this.left
  
  def assignToSquare() = this.squares.foreach(_.addNine(this))

  override def toString: String = s"Row/column/3x3: ${squares}"

end OneToNine



