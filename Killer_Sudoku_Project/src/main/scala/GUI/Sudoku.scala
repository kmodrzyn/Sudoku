package GUI

import scalafx.beans.property.StringProperty
import scala.collection.mutable.Buffer
import javafx.scene.control.ContentDisplay
import wholeThing.Game
import scalafx.scene.control.Button
import scalafx.scene.text.TextAlignment.*
import scalafx.Includes.*
import scalafx.application.JFXApp3
import scalafx.geometry.{Insets, Pos}
import scalafx.scene.Scene
import scalafx.scene.text.*
import scalafx.scene.input.MouseEvent
import scalafx.scene.control.Label
import scalafx.scene.paint.Color.*
import scalafx.scene.layout.{Background, BackgroundFill, Border, BorderPane, BorderStroke, BorderStrokeStyle, BorderWidths, ColumnConstraints, CornerRadii, GridPane, HBox, RowConstraints, VBox}
import scalafx.stage.FileChooser
import scalafx.beans.property
import scalafx.scene.control.Alert
import scalafx.scene.control.Alert.AlertType

object Main extends JFXApp3:

  val game = Game()

  def start(): Unit =

    stage = new JFXApp3.PrimaryStage:
      title = "Killer Sudoku"
      width = 1200
      height = 700

    val root = new BorderPane()
    val scene = Scene(parent = root)
    stage.scene = scene

    val border = new Border(new BorderStroke(Black, Black, Black, Black, BorderStrokeStyle.Solid,
    BorderStrokeStyle.Solid, BorderStrokeStyle.Solid, BorderStrokeStyle.Solid, CornerRadii.Empty, new BorderWidths(1), Insets.Empty))

    val combinations = new Label("")
    combinations.padding = Insets(100)
    combinations.prefWidth = 400
    combinations.prefHeight = 550
    combinations.border = border
    combinations.font = Font(24)
    combinations.background = Background(Array(new BackgroundFill(("#ADD8E6"), CornerRadii.Empty, Insets.Empty)))

    def showCombinations(i: Int, j: Int) =
      val comb = game.possibleCombinationsArea(i, j)
      var res = ""
      for i <- comb do
        var pom = ""
        for j <- i do
          pom = pom + j + " "
        res = res + pom + "\n"
      res

    var selected = new Label()
    val grid = new GridPane()
    var boxes = Vector[Vector[VBox]]()
    var numbers = Vector[Vector[Label]]()
    var areas = Vector[Vector[Label]]()
    grid.padding = Insets(20)
    for i <- 0 to 8 do
      var pomBox = Vector[VBox]()
      var pomLabel = Vector[Label]()
      var pomArea = Vector[Label]()
      for j <- 0 to 8 do
        val numb = new Label("")
        val area = new Label("")
        area.font = new Font(8)
        numb.font = new Font(16)
        numb.padding = Insets(0,0,0,8)
        val tile = new VBox(numb, area) 
        tile.prefHeight = 30
        tile.prefWidth = 40
        tile.onMouseClicked = (event: MouseEvent) =>
          selected = numb
          if game.board.areas.nonEmpty then
            combinations.text = showCombinations(i, j)
          if numb.text.toString != " " then
            numb.text = " "
            game.removeValue(i, j)
            for i <- game.board.squares do
              for square <- i do
                if square.getNine.forall(x => !(x.isCompleted)) then
                  val pos = square.pos
                  boxes(pos(0))(pos(1)).background = Background(
                    Array(
                      new BackgroundFill(
                        (game.board.squares(pos(0))(pos(1)).getArea.get.getColor.get.hex),
                        CornerRadii.Empty,
                        Insets.Empty)
                    )
                  )
        tile.padding = Insets(5)
        tile.border = border
        grid.add(tile, j, i)
        pomBox = pomBox :+ tile
        pomLabel = pomLabel :+ numb
        pomArea = pomArea :+ area
      boxes = boxes :+ pomBox
      numbers = numbers :+ pomLabel
      areas = areas :+ pomArea

    val saveButton = new Button("SAVE")
    saveButton.prefWidth = 75
    saveButton.prefHeight = 30
    saveButton.onMouseClicked = (e: MouseEvent) =>
      val fileChooser = new FileChooser
      val selectedFile = fileChooser.showSaveDialog(stage)
      if selectedFile != null then
        game.saveBoard(selectedFile.getPath + ".json")

    val loadButton = new Button("LOAD")
    loadButton.prefWidth = 75
    loadButton.prefHeight = 30
    loadButton.onMouseClicked = (e: MouseEvent) =>
      val fileChooser = new FileChooser
      val selectedFile = fileChooser.showOpenDialog(stage)
      if selectedFile != null then
        game.loadBoard(selectedFile.getPath)
        if game.board.areas.isEmpty || !game.board.squares.forall(row => row.forall(_.getArea.isDefined)) then
          new Alert(AlertType.Warning) {
            initOwner(stage)
            title = "Loading error"
            headerText = "Loading error"
            contentText = game.errorReason
          }.showAndWait()
          for i <- 0 to 8 do
            for j <- 0 to 8 do
              numbers(i)(j).text = " "
              boxes(i)(j).background = Background(Array(new BackgroundFill(("#FFFFFF"), CornerRadii.Empty, Insets.Empty)))
              areas(i)(j).text = " "
        else
          game.colorAreas()
          for i <- 0 to 8 do
            for j <- 0 to 8 do
              numbers(i)(j).text = game.board.squares(i)(j).getNumber.getOrElse(" ").toString
              if !game.board.squares(i)(j).getArea.get.isDisplayed then
                areas(i)(j).text = game.board.squares(i)(j).getArea.get.sum.toString
                game.board.squares(i)(j).getArea.get.display()
              else
                areas(i)(j).text = " "
              if game.board.squares(i)(j).getNine.forall(x => !(x.isCompleted)) then
                boxes(i)(j).background =
                  Background(
                    Array(
                      new BackgroundFill(
                        (game.board.squares(i)(j).getArea.get.getColor.get.hex),
                        CornerRadii.Empty,
                        Insets.Empty)
                    )
                  )
              else
                boxes(i)(j).background =
                  Background(
                    Array(
                      new BackgroundFill(
                        (LightGrey),
                        CornerRadii.Empty,
                        Insets.Empty)
                    )
                  )

    val topPart = new HBox(loadButton, saveButton)
    topPart.prefHeight = 60

    val bottomPart = new HBox()

    val emptyBox = new VBox()
    emptyBox.prefWidth = 150
    emptyBox.prefHeight = 550

    val rightPart = new VBox(combinations)
    rightPart.padding = Insets(10)

    val buttonBox = HBox(30)
    val buttons = Buffer[Button]()
    for i <- 1 to 9 do
      val but = new Button(s"${i}")
      but.border = border
      but.background = Background(Array(new BackgroundFill(("#FAF9F6"), CornerRadii.Empty, Insets.Empty)))
      but.onMouseClicked = (event: MouseEvent) =>
        selected.text = i.toString
        for k <- 0 to 8 do
          for j <- 0 to 8 do
            if selected == numbers(k)(j) && game.board.areas.nonEmpty then
              game.addValue(k, j, i)
              for nine <- game.board.squares(k)(j).getNine do
                if nine.isCompleted then
                  for square <- nine.squares do
                    val pos = square.pos
                    boxes(pos(0))(pos(1)).background = Background(
                      Array(
                        new BackgroundFill(
                          (LightGrey),
                          CornerRadii.Empty,
                          Insets.Empty)
                      )
                    )
              combinations.text = showCombinations(k, j)
              if game.isWon then
                new Alert(AlertType.Warning) {
                initOwner(stage)
                title = "Board completed"
                headerText = "Congratulations, you have solved this sudoku board"
              }.showAndWait()
        selected = new Label()
      buttons.append(but)
    buttonBox.children = buttons
    buttonBox.padding = Insets(20)

    for i <- 0 to 8 do
      for j <- 0 to 8 do
        boxes(i)(j).onMouseEntered = (event: MouseEvent) =>
          if game.board.areas.nonEmpty then
            val butToHighlight = game.possibleFits(i, j)
            for k <- butToHighlight do
              buttons(k - 1).background = Background(Array(new BackgroundFill(("#FDFD96"), CornerRadii.Empty, Insets.Empty)))
        boxes(i)(j).onMouseExited = (event: MouseEvent) =>
          buttons.foreach(_.background = Background(Array(new BackgroundFill(("#FAF9F6"), CornerRadii.Empty, Insets.Empty))))

    val center = VBox(40)
    center.children = Vector(grid, buttonBox)

    root.top = topPart
    root.left = emptyBox
    root.center = center
    root.right = rightPart
    root.bottom = bottomPart
