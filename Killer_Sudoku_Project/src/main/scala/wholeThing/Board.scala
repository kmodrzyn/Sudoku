package wholeThing


class Board(val squares: Vector[Vector[Square]], val areas: Vector[Area], val nines: Vector[OneToNine]):
  def getNeighbours(square: Square): Vector[Square] =
    val positions = Vector(
      (square.pos(0)-1, square.pos(1)),
      (square.pos(0), square.pos(1)-1),
      (square.pos(0)+1, square.pos(1)),
      (square.pos(0), square.pos(1)+1)
    )
    var neighbours = Vector[Square]()
    for i <- positions do
      if i(0) >= 0 && i(0) < 9 && i(1) >= 0 && i(1) < 9 then
        neighbours = neighbours :+ squares(i(0))(i(1))
    neighbours
    
  def isNeighour(first: Square, second: Square) = this.getNeighbours(first).contains(second)

  def squareGetNeighourAreas(square: Square): Vector[Area] =
    val neighbours = this.getNeighbours(square)
    var areas = Vector[Area]()
    for i <- neighbours do
      if i.getArea.isDefined then
        val area = i.getArea.get
        if square.getArea != Some(area) then
          areas = areas :+ area
    areas

  def areaGetNeighbourAreas(area: Area): Vector[Area] =
    var fromSquares = Vector[Area]()
    for square <- area.squares do
      fromSquares = fromSquares ++ this.squareGetNeighourAreas(square)
    fromSquares.distinct

end Board
