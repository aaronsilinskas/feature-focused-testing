import scala.collection.mutable

class Game(rules: Rules) {

  def step(grid: Grid): Grid = {
    val survivingCells = mutable.Set[Coordinate]()

    for (coordinate <- grid.liveCells) {
      if (rules.isLiveCellSurviving(coordinate, grid.countLiveNeighbors(coordinate))) {
        survivingCells += coordinate
      }

      val revivedDeadNeighbors = for {
        neighbor <- coordinate.neighbors
        if !grid.isAlive(neighbor)
        if rules.isDeadCellReviving(neighbor, grid.countLiveNeighbors(neighbor))
      } yield neighbor
      survivingCells ++= revivedDeadNeighbors
    }

    new Grid(survivingCells.toSet)
  }
}

trait Rules {
  def isLiveCellSurviving(coordinate: Coordinate, liveNeighbors: Int): Boolean

  def isDeadCellReviving(coordinate: Coordinate, liveNeighbors: Int): Boolean
}