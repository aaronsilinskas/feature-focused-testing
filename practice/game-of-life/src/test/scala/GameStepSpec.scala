import GridTesting._
import org.scalatest.{FeatureSpec, GivenWhenThen, Matchers}

import scala.collection.mutable

class GameStepSpec extends FeatureSpec with Matchers with GivenWhenThen {

  feature("Each game step applies the rules to all applicable cells.") {

    scenario("A step in the game applies rules to all living cells.") {
      Given("a grid with live cells")
      val livingCells = randomCoordinates()
      val grid = new Grid(livingCells)

      val rules = new Rules() {
        val liveCellsWithRulesApplied = mutable.Set[Coordinate]()

        override def isLiveCellSurviving(coordinate: Coordinate, liveNeighbors: Int): Boolean = {
          liveCellsWithRulesApplied += coordinate
          true
        }

        override def isDeadCellReviving(coordinate: Coordinate, liveNeighbors: Int): Boolean = {
          false
        }
      }
      val game = new Game(rules)

      When("a game step occurs")
      game.step(grid)

      Then("the rules are applied")
      rules.liveCellsWithRulesApplied shouldBe livingCells
    }

    scenario("A step in the game applies rules to all dead cells with at least one live neighbor.") {
      Given("a grid with live cells")
      val livingCells = randomCoordinates()
      val deadCellsAroundLiveCells = getDeadNeighbors(livingCells)
      val grid = new Grid(livingCells)

      val rules = new Rules() {
        val deadCellsWithRulesApplied = mutable.Set[Coordinate]()

        override def isLiveCellSurviving(coordinate: Coordinate, liveNeighbors: Int): Boolean = {
          true
        }

        override def isDeadCellReviving(coordinate: Coordinate, liveNeighbors: Int): Boolean = {
          deadCellsWithRulesApplied += coordinate
          false
        }
      }
      val game = new Game(rules)

      When("a game step occurs")
      game.step(grid)

      Then("the rules are applied to all dead cells with at least one live neighbor")
      rules.deadCellsWithRulesApplied shouldBe deadCellsAroundLiveCells
    }

    scenario("A step in the game applies all rules simultaneous.") {
      Given("a grid with live cells grouped so that they have neighbors")
      val randomGroupingOfLiveCells = randomNeighborhood(randomCoordinate(), 5)
      val deadNeighbors = getDeadNeighbors(randomGroupingOfLiveCells)

      val grid = new Grid(randomGroupingOfLiveCells)
      val liveNeighborCountsBeforeRules = countNeighbors(grid, randomGroupingOfLiveCells)
      val deadNeighborCountsBeforeRules = countNeighbors(grid, deadNeighbors)

      And("a rule that all live cells die and their dead neighbors become alive")
      val rules = new InvertCellsRules with NeighborCountTracking
      val game = new Game(rules)

      When("a game step occurs")
      val nextGrid = game.step(grid)

      Then("live neighbor counts reflect the original counts and not any cell deaths or births")
      rules.liveNeighborCounts shouldBe liveNeighborCountsBeforeRules
      rules.deadNeighborCounts shouldBe deadNeighborCountsBeforeRules

      And("all live cells are now dead and dead neighbors are now alive")
      nextGrid.liveCells.intersect(randomGroupingOfLiveCells) shouldBe empty
      nextGrid.liveCells shouldBe deadNeighbors
    }

  }

  private class InvertCellsRules extends Rules {

    override def isLiveCellSurviving(coordinate: Coordinate, liveNeighbors: Int): Boolean = {
      false
    }

    override def isDeadCellReviving(coordinate: Coordinate, liveNeighbors: Int): Boolean = {
      true
    }
  }

  private trait NeighborCountTracking extends Rules {
    val liveNeighborCounts = mutable.Map[Coordinate, Int]()
    val deadNeighborCounts = mutable.Map[Coordinate, Int]()

    abstract override def isLiveCellSurviving(coordinate: Coordinate, liveNeighbors: Int): Boolean = {
      liveNeighborCounts += (coordinate -> liveNeighbors)
      super.isLiveCellSurviving(coordinate, liveNeighbors)
    }

    abstract override def isDeadCellReviving(coordinate: Coordinate, liveNeighbors: Int): Boolean = {
      deadNeighborCounts += (coordinate -> liveNeighbors)
      super.isDeadCellReviving(coordinate, liveNeighbors)
    }
  }

  private def countNeighbors(grid: Grid, coordinates: Set[Coordinate]): Map[Coordinate, Int] = {
    coordinates.map { coordinate =>
      coordinate -> grid.countLiveNeighbors(coordinate)
    }.toMap
  }

  private def getDeadNeighbors(liveCoordinates: Set[Coordinate]): Set[Coordinate] = {
    for {
      liveCoordinate <- liveCoordinates
      neighbor <- liveCoordinate.neighbors
      if !liveCoordinates.contains(neighbor)
    } yield neighbor
  }
}
