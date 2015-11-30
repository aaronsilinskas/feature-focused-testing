import scala.annotation.tailrec
import scala.util.Random

object GridTesting {

  def randomCoordinate(): Coordinate = {
    new Coordinate(Random.nextInt(), Random.nextInt())
  }

  @tailrec
  def randomCoordinates(count: Int = 1000, excluding: Set[Coordinate] = Set.empty, including: Set[Coordinate] = Set.empty): Set[Coordinate] = {
    if (count == 0) {
      including
    } else {
      val c = randomCoordinate()

      if (excluding.contains(c) || including.contains(c)) {
        randomCoordinates(count, excluding, including)
      } else {
        randomCoordinates(count - 1, excluding, including + c)
      }
    }
  }

  def getNeighborCoordinates(c: Coordinate): Set[Coordinate] = {
    Set(
      Coordinate(c.x - 1, c.y - 1), Coordinate(c.x, c.y - 1), Coordinate(c.x + 1, c.y - 1),
      Coordinate(c.x - 1, c.y), Coordinate(c.x + 1, c.y),
      Coordinate(c.x - 1, c.y + 1), Coordinate(c.x, c.y + 1), Coordinate(c.x + 1, c.y + 1)
    )
  }

  def randomNeighborhood(seed: Coordinate, recursiveDepth: Int): Set[Coordinate] = {
    if (recursiveDepth == 0) {
      Set(seed)
    } else {
      val allNeighbors = getNeighborCoordinates(seed)
      val randomSubsetOfNeighbors = Random.shuffle(allNeighbors).take(3)
      randomSubsetOfNeighbors.flatMap { neighbor =>
        randomNeighborhood(neighbor, recursiveDepth - 1)
      }
    }
  }

}
