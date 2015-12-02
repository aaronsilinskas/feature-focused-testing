import scala.collection.mutable

class Game(rules: Rules) {

  def step(grid: Grid): Grid = {
    val survivingCells = mutable.Set[Cell]()

    for (cell <- grid.liveCells) {
      if (rules.isLiveCellSurviving(cell, grid.countLiveNeighbors(cell))) {
        survivingCells += cell
      }

      val revivedDeadNeighbors = for {
        neighbor <- cell.neighbors
        if !grid.isAlive(neighbor)
        if rules.isDeadCellReviving(neighbor, grid.countLiveNeighbors(neighbor))
      } yield neighbor
      survivingCells ++= revivedDeadNeighbors
    }

    new Grid(survivingCells.toSet)
  }
}

trait Rules {
  def isLiveCellSurviving(cell: Cell, liveNeighbors: Int): Boolean

  def isDeadCellReviving(cell: Cell, liveNeighbors: Int): Boolean
}