
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