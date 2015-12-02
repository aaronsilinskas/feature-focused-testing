import scala.annotation.tailrec
import scala.util.Random

object GridTesting {

  def randomCell(): Cell = {
    new Cell(Random.nextInt(), Random.nextInt())
  }

  @tailrec
  def randomCells(count: Int = 1000, excluding: Set[Cell] = Set.empty, including: Set[Cell] = Set.empty): Set[Cell] = {
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

  def getNeighborCells(c: Cell): Set[Cell] = {
    Set(
      Cell(c.x - 1, c.y - 1), Cell(c.x, c.y - 1), Cell(c.x + 1, c.y - 1),
      Cell(c.x - 1, c.y), Cell(c.x + 1, c.y),
      Cell(c.x - 1, c.y + 1), Cell(c.x, c.y + 1), Cell(c.x + 1, c.y + 1)
    )
  }

  def randomNeighborhood(seed: Cell, recursiveDepth: Int): Set[Cell] = {
    if (recursiveDepth == 0) {
      Set(seed)
    } else {
      val allNeighbors = getNeighborCells(seed)
      val randomSubsetOfNeighbors = Random.shuffle(allNeighbors).take(3)
      randomSubsetOfNeighbors.flatMap { neighbor =>
        randomNeighborhood(neighbor, recursiveDepth - 1)
      }
    }
  }

}
