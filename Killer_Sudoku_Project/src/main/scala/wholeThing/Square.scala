package wholeThing


class Square(private val position: (Int, Int)):
  private var number: Option[Int] = Option.empty[Int]
  private var area: Option[Area] = Option.empty[Area]
  private var nine: Vector[OneToNine] = Vector()

  def getNumber: Option[Int] = this.number

  def setNumber(value: Int) =
    this.number = Some(value)
    this.nine.foreach(x => x.addNumber(this))
  
  def pos = this.position

  def delete() =
    this.nine.foreach(x => x.removeNumber(this))
    this.number = None

  def getArea: Option[Area] = this.area

  def setArea(newArea: Area) = this.area = Some(newArea)
  
  def clearArea() = this.area = None
  
  def getNine: Vector[OneToNine] = this.nine

  def addNine(oneToNine: OneToNine) = this.nine = this.nine :+ oneToNine

  override def toString: String = s"Square at position ${position} "

end Square



