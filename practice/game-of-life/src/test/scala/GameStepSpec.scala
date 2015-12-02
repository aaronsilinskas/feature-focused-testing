import GridTesting._
import org.scalatest.{FeatureSpec, GivenWhenThen, Matchers}

import scala.collection.mutable

class GameStepSpec extends FeatureSpec with Matchers with GivenWhenThen {

  feature("Each game step applies the rules to all applicable cells.") {

    scenario("A step in the game applies rules to all living cells.") {
      Given("a grid with live cells")
      val livingCells = randomCells()
      val grid = new Grid(livingCells)

      val rules = new Rules() {
        val liveCellsWithRulesApplied = mutable.Set[Cell]()

        override def isLiveCellSurviving(cell: Cell, liveNeighbors: Int): Boolean = {
          liveCellsWithRulesApplied += cell
          true
        }

        override def isDeadCellReviving(cell: Cell, liveNeighbors: Int): Boolean = {
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
      val livingCells = randomCells()
      val deadCellsAroundLiveCells = getDeadNeighbors(livingCells)
      val grid = new Grid(livingCells)

      val rules = new Rules() {
        val deadCellsWithRulesApplied = mutable.Set[Cell]()

        override def isLiveCellSurviving(cell: Cell, liveNeighbors: Int): Boolean = {
          true
        }

        override def isDeadCellReviving(cell: Cell, liveNeighbors: Int): Boolean = {
          deadCellsWithRulesApplied += cell
          false
        }
      }
      val game = new Game(rules)

      When("a game step occurs")
      game.step(grid)

      Then("the rules are applied to all dead cells with at least one live neighbor")
      rules.deadCellsWithRulesApplied shouldBe deadCellsAroundLiveCells
    }

    scenario("A step in the game applies all rules simultaneously.") {
      Given("a grid with live cells grouped so that they have neighbors")
      val randomGroupingOfLiveCells = randomNeighborhood(randomCell(), 5)

      val grid = new Grid(randomGroupingOfLiveCells)
      val liveCellNeighborCountsBeforeRules = countLiveNeighbors(grid, randomGroupingOfLiveCells)

      val deadCellsWhereRulesApply = getDeadNeighbors(randomGroupingOfLiveCells)
      val deadCellNeighborCountsBeforeRules = countLiveNeighbors(grid, deadCellsWhereRulesApply)

      And("a rule that all live cells die and their dead neighbors become alive")
      val rules = new InvertCellsRules with NeighborCountTracking
      val game = new Game(rules)

      When("a game step occurs")
      val nextGrid = game.step(grid)

      Then("live neighbor counts reflect the original counts and not any cell deaths or births")
      rules.liveNeighborCounts shouldBe liveCellNeighborCountsBeforeRules
      rules.deadNeighborCounts shouldBe deadCellNeighborCountsBeforeRules

      And("all live cells are now dead and dead neighbors are now alive")
      nextGrid.liveCells.intersect(randomGroupingOfLiveCells) shouldBe empty
      nextGrid.liveCells shouldBe deadCellsWhereRulesApply
    }

  }

  private class InvertCellsRules extends Rules {

    override def isLiveCellSurviving(cell: Cell, liveNeighbors: Int): Boolean = {
      false
    }

    override def isDeadCellReviving(cell: Cell, liveNeighbors: Int): Boolean = {
      true
    }
  }

  private trait NeighborCountTracking extends Rules {
    val liveNeighborCounts = mutable.Map[Cell, Int]()
    val deadNeighborCounts = mutable.Map[Cell, Int]()

    abstract override def isLiveCellSurviving(cell: Cell, liveNeighbors: Int): Boolean = {
      liveNeighborCounts += (cell -> liveNeighbors)
      super.isLiveCellSurviving(cell, liveNeighbors)
    }

    abstract override def isDeadCellReviving(cell: Cell, liveNeighbors: Int): Boolean = {
      deadNeighborCounts += (cell -> liveNeighbors)
      super.isDeadCellReviving(cell, liveNeighbors)
    }
  }

  private def countLiveNeighbors(grid: Grid, cells: Set[Cell]): Map[Cell, Int] = {
    cells.map { cell =>
      cell -> grid.countLiveNeighbors(cell)
    }.toMap
  }

  private def getDeadNeighbors(liveCells: Set[Cell]): Set[Cell] = {
    for {
      liveCell <- liveCells
      neighbor <- liveCell.neighbors
      if !liveCells.contains(neighbor)
    } yield neighbor
  }
}
