package com.aeq

import java.io.File
import java.io.PrintWriter

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


  //Returns adjacent cells both from Row and Column of current cell
  def getNeighbors(rowIndex: Int, columnIndex: Int): List[Box] = {
    val neighborCoords = List((rowIndex + 1, columnIndex), (rowIndex - 1, columnIndex), (rowIndex, columnIndex - 1), (rowIndex, columnIndex + 1))
    neighborCoords.foldLeft(List[Box]())((currList, currCoords) => {
      if (!(currCoords._1 >= height || currCoords._1 < 0 || currCoords._2 < 0 || currCoords._2 >= width)) {
        getBox(currCoords._1, currCoords._2) :: currList
      }
      else currList
    })
  }

  //Get All undefined cells in the current row of a given cell
  def getOtherRowCells(updatedCell: Box) = {
    getBoxes(updatedCell.rowIndex).filter(c => c.columnIndex != updatedCell.columnIndex && c.color==Undefined)
  }

  def getOtherColumnCells(upatedCell: Box) = {
    getBoxes.flatten.filter(c => c.rowIndex != upatedCell.rowIndex && c.columnIndex == upatedCell.columnIndex  &&  c.color==Undefined)
  }

  def getBox(x: Int, y: Int): Box = boxes(x)(y)


  /**
    * Updates the given cell with dessiredColor and
    * When Color is white marks all other cells with same value in the row/colum to be Black
    * When Color is Black marks all it's adjacent to be White
    * @param cell
    * @param desiredColor
    * @return
    */
  def changeColorAndUpdateBoard(cell: Box, desiredColor: Color.Value): Board = {

    def updateColor(color: Color) : Board = {
      val line = boxes(cell.rowIndex)
      val box = line(cell.columnIndex)
      Board(boxes.updated(cell.rowIndex, line.updated(cell.columnIndex, box.changeValue(color))))
    }

    if(desiredColor==Black && !isBlackConnected(board = this, cell.rowIndex, cell.columnIndex)) {
      markBlackAdjacentAsWhite(updateColor(desiredColor), cell.rowIndex, cell.columnIndex)
    } else {
      markSameValuesAsBlack(updateColor(desiredColor), cell)
    }

  }

  def markUniqueCellsWhite(): Board = {

    getBoxes.flatten.filter(_.color==Undefined).foreach { currentCell =>
      val noRowDuplicate = !getBoxes.flatten.filter(rowCell => rowCell.rowIndex==currentCell.rowIndex && rowCell.columnIndex != currentCell.columnIndex && rowCell.color != Black).map(_.value).contains(currentCell.value)
      val noColumnDuplicate = !getBoxes.flatten.filter(rowCell => rowCell.columnIndex==currentCell.columnIndex && rowCell.rowIndex != currentCell.rowIndex && rowCell.color != Black).map(_.value).contains(currentCell.value)
      if(noRowDuplicate && noColumnDuplicate)
        changeColorAndUpdateBoard(currentCell, White)
    }
    if(getBoxes.flatten.exists(_.color == Undefined))
      markUniqueCellsWhite()
    else
      this
  }

  def isBlackConnected(board: Board,x: Int, y:Int): Boolean = {
    board.getNeighbors(x, y).map(_.color).contains(Black)
  }

  def markBlackAdjacentAsWhite(b:Board, x:Int, y:Int) : Board = {
    var board = b
    b.getNeighbors(x, y).filter(_.color==Undefined).foreach(c => board = board.changeColorAndUpdateBoard(c,White))
    board
  }

  def markSameValuesAsBlack(board:Board, cell:Box) :Board = {
    var b = board
    val otherCells = b.getOtherRowCells(cell).filter(_.value==cell.value) ++ b.getOtherColumnCells(cell).filter(_.value==cell.value)
    otherCells.foreach (c => b = b.changeColorAndUpdateBoard(c, Black))
    b
  }

}

object PuzzleSolver {


  def main(args: Array[String]): Unit = {
    val inputPath = args(0)
    val output = args(1)
    println(output)

    val board = solvePuzzle(inputPath)
    saveOuputToFile(output, board)
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

    if (boardData.isEmpty) {
      Board(currBoard.getBoxes.reverse)
    } else {
      val line = loadLine(boardData.head).reverse
      loadBoard(boardData.tail, x + 1, Board(line :: currBoard.getBoxes))
    }
  }

  def solvePuzzle(inpputFileName:String): Board = {
    val lines = scala.io.Source.fromFile(new File(inpputFileName)).getLines().toList
    val numLines = lines.map(_.split(" ").toList.map(_.toInt))

    var board =  loadBoard(numLines)

    board = checkForSandwiches(board)
    println("Sandwi")
    showBoard(board)
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
    board = board.markUniqueCellsWhite()
    println("Mark relevant white")
    showBoard(board)


    board

  }

  def saveOuputToFile(ouputFileName:String, board: Board) = {
    val writer = new PrintWriter(new File(ouputFileName))
    board.getBoxes.foreach(row => {
      row.foreach { e =>
        writer.print(s"${e.color}")
        if(e.columnIndex<board.width-1) writer.print("\t")   //Avoid Tab for the last element
      }
      writer.println() //New line for every row
    } )
    writer.close()
  }





}
