package com.aeq

import java.io.File


object PuzzleSolver {

  object Color extends Enumeration {
    type Color = Value
    val White = Value("W")
    val Black = Value("B")
    val Undefined = Value("U")
  }

  import Color._

  case class Box (rowIndex:Int, columnIndex:Int, value: Int, color: Color.Value=Undefined) {
    def changeValue(desiredColor: Color.Value): Box = Box(rowIndex, columnIndex, value, desiredColor)
  }

  case class Board(boxes: List[List[Box]]) {

    val height: Int = boxes.length
    val width: Int = if (boxes.nonEmpty) boxes.head.length else 0

    def getBoxes: List[List[Box]] = boxes

    /**
      * Returns adjacent cells both from Row and Column of current cell
      * @param rowIndex
      * @param columnIndex
      * @return
      */
    def getNeighbors(rowIndex: Int, columnIndex: Int): List[Box] = {
      val neighborCoords = List((rowIndex + 1, columnIndex), (rowIndex - 1, columnIndex), (rowIndex, columnIndex - 1), (rowIndex, columnIndex + 1))
      neighborCoords.foldLeft(List[Box]())((currList, currCoords) => {
        if (!(currCoords._1 >= height || currCoords._1 < 0 || currCoords._2 < 0 || currCoords._2 >= width)) {
          getBox(currCoords._1, currCoords._2) :: currList
        }
        else currList
      })
    }

    /**
      * @param updatedCell
      * @return All undefined cells in the current row of a given cell
      */
    def getOtherRowCells(updatedCell: Box) = {
      getBoxes(updatedCell.rowIndex).filter(c => c.columnIndex != updatedCell.columnIndex && c.color==Undefined)
    }

    def getOtherColumnCells(upatedCell: Box) = {
      getBoxes.flatten.filter(c => c.rowIndex != upatedCell.rowIndex && c.columnIndex == upatedCell.columnIndex  &&  c.color==Undefined)
    }

    def getBox(x: Int, y: Int): Box = boxes(x)(y)


   /* def changeColorAndUpdateBoard(cell: Box, desiredColor: Color.Value): Board = {
      val line = boxes(cell.rowIndex)
      val box = line(cell.columnIndex)
      if(desiredColor==Black && !isBlackConnected(board = this, cell.rowIndex, cell.columnIndex)) {
        val b = Board(boxes.updated(cell.rowIndex, line.updated(cell.columnIndex, box.changeValue(desiredColor))))
        markBlackAdjacentAsWhite(b, cell.rowIndex, cell.columnIndex)
      } else {
        var b = Board(boxes.updated(cell.rowIndex, line.updated(cell.columnIndex, box.changeValue(White))))
        val currentBox = b.getBox(cell.rowIndex, cell.columnIndex)
        val re = b.getBoxes(cell.rowIndex).filter(cell => cell.columnIndex != cell.columnIndex && cell.color==Undefined && cell.value==currentBox.value)
        val ce =   b.getBoxes.flatten.filter(cell => cell.rowIndex!= cell.rowIndex &&  cell.color==Undefined && cell.columnIndex == cell.columnIndex && cell.value==currentBox.value)
        if((re++ce).nonEmpty) (re++ce).foreach{c =>
          b = changeColorAndUpdateBoard(c, Black)
        }
        b
      }

    }
  }*/

    def changeColorAndUpdateBoard(cell: Box, desiredColor: Color.Value): Board = {
      val line = boxes(cell.rowIndex)
      val box = line(cell.columnIndex)
      if(desiredColor==Black && !isBlackConnected(board = this, cell.rowIndex, cell.columnIndex)) {
        val b = Board(boxes.updated(cell.rowIndex, line.updated(cell.columnIndex, box.changeValue(desiredColor))))
        markBlackAdjacentAsWhite(b, cell.rowIndex, cell.columnIndex)
      } else {
        var b = Board(boxes.updated(cell.rowIndex, line.updated(cell.columnIndex, box.changeValue(White))))

        val re = b.getOtherRowCells(cell).filter(_.value==cell.value)
        val ce =  b.getOtherColumnCells(cell).filter(_.value==cell.value)
        if((re++ce).nonEmpty) (re++ce).foreach (c => b = changeColorAndUpdateBoard(c, Black))

        b
      }

    }
  }





  def isBlackConnected(board: Board,x: Int, y:Int): Boolean = {
    board.getNeighbors(x, y).map(_.color).contains(Black)
  }

  def markBlackAdjacentAsWhite(b:Board, x:Int, y:Int) : Board = {
    var board = b
    b.getNeighbors(x, y).filter(_.color==Undefined).foreach(c => board = board.changeColorAndUpdateBoard(c,White))
    board
  }

  def markSameValuesAsBlack(board:Board, x:Int, y:Int) :Board = {
    var b = board

    val currentBox = b.getBox(x, y)
    val re = b.getOtherRowCells(currentBox).filter(_.value==currentBox.value)
    val ce =   b.getBoxes.flatten.filter(cell => cell.rowIndex!= x &&  cell.color==Undefined && cell.columnIndex == y && cell.value==currentBox.value)
    if((re++ce).nonEmpty) (re++ce).foreach {
      cell => {
        b = b.changeColorAndUpdateBoard(cell, Black)
      }
    }

    b
  }


  def checkForSandwiches(b: Board): Board = {

    var board = b

        b.getBoxes.flatten.foreach{cell =>
          val neibours = board.getNeighbors(cell.rowIndex, cell.columnIndex)

          val rn = neibours.filter(_.rowIndex==cell.rowIndex)
          if(rn.map(_.value).diff(rn.map(_.value).distinct).nonEmpty)
            board = board.changeColorAndUpdateBoard(cell, Color.White)

          val cn = neibours.filter(_.columnIndex==cell.columnIndex)
          if(cn.map(_.value).diff(cn.map(_.value).distinct).nonEmpty)
            board = board.changeColorAndUpdateBoard(cell, Color.White)
        }


    board
  }

  def checkUniqueness(b:Board) :Board = {
    var board = b

    b.getBoxes.flatten.filter(_.color==White).foreach(b => {
      val rows =  board.getBoxes(b.rowIndex).filter(c => c.columnIndex != b.columnIndex && c.value == b.value)
      val columns = board.getBoxes.flatten.filter(c => c.columnIndex==b.columnIndex && c.rowIndex !=b.rowIndex && c.value==b.value)
      (rows++columns).foreach { c =>
        board = board.changeColorAndUpdateBoard(c, Color.Black)
      }
    })

    board
  }

  def checkCornerHeuristic(b:Board) : Board = {
    var board = b
    List((0,0), (0,b.height-1), (b.width-1, 0), (b.height-1, b.width-1)).foreach{index =>
      val nei = b.getNeighbors(index._1, index._2)

      if((nei :+ b.getBox(index._1, index._2)).map(_.value).distinct.size == 1) {
         board = board.changeColorAndUpdateBoard(board.getBox(index._1, index._2), Black)
      }
    }

    board
  }

  def checkOddManOut(b:Board) : Board = {
    var board = b
    b.getBoxes.flatten.filter(_.color==Undefined).foreach { c =>

        (checkDuplicate(board,c.rowIndex, c.columnIndex+1, c.value, c.columnIndex, isRowCheck = true) ++     //Check If two sub-sequent ceels are equalient in current row
        checkDuplicate(board,c.rowIndex+1, c.columnIndex, c.value, c.rowIndex, isRowCheck = false)).foreach {   //Check If two sub-sequent ceels are equalient in current column
          box => board = board.changeColorAndUpdateBoard(box, Black)
        }

    }

    board
  }

  def checkDuplicate(board: Board, x: Int, y: Int, value : Int, roworcolIndex:Int, isRowCheck:Boolean) : List[Box] = {
    if( (isRowCheck && y<board.width-1) || (!isRowCheck && x<board.height-1)) {
      val ra = board.getBox(x, y)
      if(ra.value==value)
        board.getBoxes(x).filter(oe => oe.value==value && !List(roworcolIndex, getRowOrCol(isRowCheck, oe)).contains(oe.columnIndex))
      else
        Nil
    } else
      Nil
  }

  def checkWhiteConnectedness(b: Board): Board = {
    var board = b

    b.getBoxes.flatten.filter(_.color==Undefined).foreach { c=>
      board.getNeighbors(c.rowIndex, c.columnIndex).filter(cell => cell.color==White).foreach(wn => {
        val neighbors = board.getNeighbors(wn.rowIndex, wn.columnIndex).filterNot(_==c)
        if(neighbors.map(_.color).forall(_ == Black))
          board = board.changeColorAndUpdateBoard(c, White)
      })
    }

    board
  }



  def getRowOrCol(isRow: Boolean, c: Box): Int = {
    if(isRow) c.columnIndex else c.rowIndex
  }


  def solvePuzzle(f:String):Unit = {
    val lines = scala.io.Source.fromFile(new File(f)).getLines().toList
    val numLines = lines.map(_.split(" ").toList.map(_.toInt))

    var board =  loadBoard(numLines)

    board = checkForSandwiches(board)
    println("Sandwi")
    showBoard(board)
    board = checkUniqueness(board)
    println("Unique")
    showBoard(board)
    board = checkCornerHeuristic(board)
    println("Corner")
    showBoard(board)
    board = checkOddManOut(board)
    println("OddManout")
    showBoard(board)
    board = checkWhiteConnectedness(board)
    println("White Connected")
    showBoard(board)
    board = markWhite(board)
    println("Mark relevant white")
    showBoard(board)

  }

  def showBoard(board:Board) :Unit = {
    board.getBoxes.foreach(row => {
      row.foreach(e => print(s"${e.color}\t"))
      println()
    } )
  }


  def loadBoard(boardData: List[List[Int]], x: Int = 0, currBoard: Board = Board(Nil)): Board = {

    def loadLine(lineData: List[Int], y:Int = 0, currLine: List[Box] = Nil): List[Box] = {
      if (lineData.isEmpty) currLine
      else {
        val newLine = Box(x, y, lineData.head) :: currLine
        loadLine(lineData.tail, y + 1, newLine)
      }
    }

    if (boardData.isEmpty)
      Board(currBoard.getBoxes.reverse)
    else {
      val line = loadLine(boardData.head).reverse
      loadBoard(boardData.tail, x + 1,  Board(line :: currBoard.getBoxes))
    }
  }


  def markWhite(b: Board): Board = {
    var board = b
    board.getBoxes.flatten.filter(_.color==Undefined).foreach { currentCell =>
      val noRowDuplicate = !board.getBoxes.flatten.filter(rowCell => rowCell.rowIndex==currentCell.rowIndex && rowCell.columnIndex != currentCell.columnIndex && rowCell.color != Black).map(_.value).contains(currentCell.value)
      val noColumnDuplicate = !board.getBoxes.flatten.filter(rowCell => rowCell.columnIndex==currentCell.columnIndex && rowCell.rowIndex != currentCell.rowIndex && rowCell.color != Black).map(_.value).contains(currentCell.value)
      if(noRowDuplicate && noColumnDuplicate) board = board.changeColorAndUpdateBoard(currentCell, White)
      if(!noRowDuplicate && !noColumnDuplicate) board = board.changeColorAndUpdateBoard(currentCell, Black)
    }
    println("--------------")
    showBoard(board)
    if(board.getBoxes.flatten.exists(_.color == Undefined)) board = markWhite(board)

    board
  }


  def main(args: Array[String]): Unit = {
    val inputPath = args(0)

    solvePuzzle(inputPath)

  }


}
