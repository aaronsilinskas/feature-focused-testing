
import org.scalatest._

class RulesSpec extends FeatureSpec with Matchers with GridFixture {

  val rules = new DefaultRules()
  val anyCell = randomCell()

  feature("The rules to determine whether or not a cell will survive") {

    scenario("Live cell with fewer than two live neighbors dies.") {
      rules.isLiveCellSurviving(anyCell, liveNeighbors = 0) shouldBe false
      rules.isLiveCellSurviving(anyCell, liveNeighbors = 1) shouldBe false
    }

    scenario("Live cell two or three live neighbors lives.") {
      rules.isLiveCellSurviving(anyCell, liveNeighbors = 2) shouldBe true
      rules.isLiveCellSurviving(anyCell, liveNeighbors = 3) shouldBe true
    }

    scenario("Live cell with more than three live neighbors dies.") {
      for (liveNeighbors <- 4 to 9) {
        rules.isLiveCellSurviving(anyCell, liveNeighbors) shouldBe false
      }
    }

    scenario("Dead cell with exactly three live neighbors becomes alive.") {
      rules.isDeadCellReviving(anyCell, liveNeighbors = 3) shouldBe true
    }

    scenario("Dead cell without three live neighbors stays dead.") {
      for (liveNeighbors <- 0 to 9 if liveNeighbors != 3) {
        rules.isDeadCellReviving(anyCell, liveNeighbors) shouldBe false
      }
    }
  }
}
