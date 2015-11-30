
class DefaultRules extends Rules {

  override def isLiveCellSurviving(coordinate: Coordinate, liveNeighbors: Int): Boolean = {
    liveNeighbors == 2 || liveNeighbors == 3
  }

  override def isDeadCellReviving(coordinate: Coordinate, liveNeighbors: Int): Boolean = {
    liveNeighbors == 3
  }

}

