import scala.collection.mutable

case class Cell(x: Int, y: Int) {
  def neighbors: Set[Cell] = {
    val neighborCells = for {
      neighborX <- x - 1 to x + 1
      neighborY <- y - 1 to y + 1
      if !(neighborX == x && neighborY == y)
    } yield Cell(neighborX, neighborY)

    neighborCells.toSet
  }
}

class Grid(val liveCells: Set[Cell]) {

  def isAlive(c: Cell): Boolean = {
    liveCells.contains(c)
  }

  def countLiveNeighbors(c: Cell): Int = {
    c.neighbors.count(isAlive)
  }

  override def equals(obj: Any): Boolean = {
    obj match {
      case that: Grid => this.liveCells == that.liveCells
      case _ => false
    }
  }
}

object Grid {

  def fromString(encodedGrid: String): Grid = {
    val liveCells = mutable.Set[Cell]()

    for ((row, y) <- encodedGrid.split("\n").zipWithIndex) {
      for ((cell, x) <- row.zipWithIndex) {
        if (cell == '@') {
          liveCells += Cell(x, y)
        }
      }
    }

    new Grid(liveCells.toSet)
  }
}

