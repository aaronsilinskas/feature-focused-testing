
import GridTesting._
import org.scalatest._

class RulesSpec extends FeatureSpec with ShouldMatchers {

  val rules = new DefaultRules()
  val anyCoordinate = randomCoordinate()

  feature("The rules to determine whether or not a cell will survive") {

    scenario("Live cell with fewer than two live neighbors dies.") {
      rules.isLiveCellSurviving(anyCoordinate, liveNeighbors = 0) shouldBe false
      rules.isLiveCellSurviving(anyCoordinate, liveNeighbors = 1) shouldBe false
    }

    scenario("Live cell two or three live neighbors lives.") {
      rules.isLiveCellSurviving(anyCoordinate, liveNeighbors = 2) shouldBe true
      rules.isLiveCellSurviving(anyCoordinate, liveNeighbors = 3) shouldBe true
    }

    scenario("Live cell with more than three live neighbors dies.") {
      for (liveNeighbors <- 4 to 9) {
        rules.isLiveCellSurviving(anyCoordinate, liveNeighbors) shouldBe false
      }
    }

    scenario("Dead cell with exactly three live neighbors becomes alive.") {
      rules.isDeadCellReviving(anyCoordinate, liveNeighbors = 3) shouldBe true
    }

    scenario("Dead cell without three live neighbors stays dead.") {
      for (liveNeighbors <- 0 to 9 if liveNeighbors != 3) {
        rules.isDeadCellReviving(anyCoordinate, liveNeighbors) shouldBe false
      }
    }
  }
}
