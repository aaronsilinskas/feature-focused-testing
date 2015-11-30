import scala.collection.mutable

case class Coordinate(x: Int, y: Int) {
  def neighbors: Set[Coordinate] = {
    val neighborCoordinates = for {
      neighborX <- x - 1 to x + 1
      neighborY <- y - 1 to y + 1
      if !(neighborX == x && neighborY == y)
    } yield Coordinate(neighborX, neighborY)

    neighborCoordinates.toSet
  }
}

class Grid(val liveCells: Set[Coordinate]) {

  def isAlive(c: Coordinate): Boolean = {
    liveCells.contains(c)
  }

  def countLiveNeighbors(c: Coordinate): Int = {
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
    val liveCells = mutable.Set[Coordinate]()

    for ((row, y) <- encodedGrid.split("\n").zipWithIndex) {
      for ((cell, x) <- row.zipWithIndex) {
        if (cell == '@') {
          liveCells += Coordinate(x, y)
        }
      }
    }

    new Grid(liveCells.toSet)
  }
}

