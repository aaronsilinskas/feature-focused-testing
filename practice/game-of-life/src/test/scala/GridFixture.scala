import scala.annotation.tailrec
import scala.util.Random

trait GridFixture {

  def randomCell(): Cell = {
    new Cell(Random.nextInt(), Random.nextInt())
  }

  @tailrec
  final def randomCells(count: Int = 1000, excluding: Set[Cell] = Set.empty, including: Set[Cell] = Set.empty): Set[Cell] = {
    if (count == 0) {
      including
    } else {
      val c = randomCell()

      if (excluding.contains(c) || including.contains(c)) {
        randomCells(count, excluding, including)
      } else {
        randomCells(count - 1, excluding, including + c)
      }
    }
  }

  def getNeighborCells(cell: Cell): Set[Cell] = {
    Set(
      Cell(cell.x - 1, cell.y - 1), Cell(cell.x, cell.y - 1), Cell(cell.x + 1, cell.y - 1),
      Cell(cell.x - 1, cell.y), Cell(cell.x + 1, cell.y),
      Cell(cell.x - 1, cell.y + 1), Cell(cell.x, cell.y + 1), Cell(cell.x + 1, cell.y + 1)
    )
  }

  def randomNeighborhood(seed: Cell, recursiveDepth: Int, liveNeighborsPerSeed: Int = 3): Set[Cell] = {
    if (recursiveDepth == 0) {
      Set(seed)
    } else {
      val allNeighbors = getNeighborCells(seed)
      val randomSubsetOfNeighbors = Random.shuffle(allNeighbors).take(liveNeighborsPerSeed)
      randomSubsetOfNeighbors.flatMap { neighbor =>
        randomNeighborhood(neighbor, recursiveDepth - 1)
      }
    }
  }

}
