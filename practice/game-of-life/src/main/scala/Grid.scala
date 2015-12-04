
class Grid(val liveCells: Set[Cell]) {

  def isAlive(cell: Cell): Boolean = {
    liveCells.contains(cell)
  }

  def countLiveNeighbors(cell: Cell): Int = {
    cell.neighbors.count(isAlive)
  }

  override def equals(obj: Any): Boolean = {
    obj match {
      case that: Grid => this.liveCells == that.liveCells
      case _ => false
    }
  }
}