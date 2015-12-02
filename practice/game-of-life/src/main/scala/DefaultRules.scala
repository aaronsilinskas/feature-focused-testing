
class DefaultRules extends Rules {

  override def isLiveCellSurviving(cell: Cell, liveNeighbors: Int): Boolean = {
    liveNeighbors == 2 || liveNeighbors == 3
  }

  override def isDeadCellReviving(cell: Cell, liveNeighbors: Int): Boolean = {
    liveNeighbors == 3
  }

}

