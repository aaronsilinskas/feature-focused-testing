import org.scalatest._

import scala.annotation.tailrec
import scala.collection.mutable

class ExamplePatternsSpec extends FeatureSpec with Matchers {

  feature("Conway's Game of Life - Example patterns using default rules") {

    scenario("Stable 2x2 block pattern.") {
      val gridSnapshots =
        """
          |__@@__ __@@__ __@@__
          |__@@__ __@@__ __@@__
        """.stripMargin('|')

      snapshotsShouldMatchGameSteps(gridSnapshots)
    }

    scenario("Blinker pattern.") {
      val gridSnapshots =
        """
          |______ ___@__ ______ ___@__
          |__@@@_ ___@__ __@@@_ ___@__
          |______ ___@__ ______ ___@__
        """.stripMargin('|')

      snapshotsShouldMatchGameSteps(gridSnapshots)
    }

    scenario("4 steps to Beehive pattern.") {
      val gridSnapshots =
        """
          |______ ___@__ __@@__ __@@__ __@@__
          |__@@@_ __@@__ __@@__ _@__@_ _@__@_
          |__@___ __@___ __@@__ __@@__ __@@__
        """.stripMargin('|')

      snapshotsShouldMatchGameSteps(gridSnapshots)
    }

    scenario("3 steps to Beehive pattern.") {
      val gridSnapshots =
        """
          |__@___ ______ __@___ __@___
          |__@___ _@@@__ _@_@__ _@_@__
          |__@___ _@@@__ _@_@__ _@_@__
          |__@___ ______ __@___ __@___
        """.stripMargin('|')

      snapshotsShouldMatchGameSteps(gridSnapshots)
    }

    scenario("Beacon pattern.") {
      val gridSnapshots =
        """
          |_@@___ _@@___ _@@___ _@@___
          |_@____ _@@___ _@____ _@@___
          |____@_ ___@@_ ____@_ ___@@_
          |___@@_ ___@@_ ___@@_ ___@@_
        """.stripMargin('|')

      snapshotsShouldMatchGameSteps(gridSnapshots)
    }

    scenario("Toad pattern.") {
      val gridSnapshots =
        """
          |______ ___@__ ______ ___@__
          |__@@@_ _@__@_ __@@@_ _@__@_
          |_@@@__ _@__@_ _@@@__ _@__@_
          |______ __@___ ______ __@___
        """.stripMargin('|')

      snapshotsShouldMatchGameSteps(gridSnapshots)
    }

    scenario("5 steps then death pattern.") {
      val gridSnapshots =
        """
          |_@____ __@@__ __@@__ ___@__ ______ ______
          |__@@@_ __@@__ ____@_ ____@_ ___@@_ ______
          |______ ___@__ __@@__ ___@__ ______ ______
        """.stripMargin('|')

      snapshotsShouldMatchGameSteps(gridSnapshots)
    }
  }

  private def snapshotsShouldMatchGameSteps(gridSnapshots: String): Unit = {
    val initialAndExpectedGrids = gridSnapshotsFromString(gridSnapshots)

    val gridAndSteps = takeSteps(initialAndExpectedGrids.head, initialAndExpectedGrids.size - 1)

    initialAndExpectedGrids shouldBe gridAndSteps
  }

  def gridSnapshotsFromString(encodedGrids: String): Seq[Grid] = {
    val separatedGrids = new mutable.ArrayBuffer[String]()
    for (encodedLine <- encodedGrids.split("\n") if !encodedLine.isEmpty) {
      for ((row, gridIndex) <- encodedLine.split(" ").zipWithIndex) {
        val rowAndDelimiter = row + "\n"
        if (separatedGrids.isDefinedAt(gridIndex)) {
          separatedGrids.update(gridIndex, separatedGrids(gridIndex) + rowAndDelimiter)
        } else {
          separatedGrids.insert(gridIndex, rowAndDelimiter)
        }
      }
    }
    separatedGrids map Grid.fromString
  }

  private def takeSteps(initialStep: Grid, stepsToTake: Int): Seq[Grid] = {
    val game = new Game(new DefaultRules())

    @tailrec
    def stepUntilDone(previousSteps: Seq[Grid], stepsLeft: Int): Seq[Grid] = {
      if (stepsLeft == 0) {
        previousSteps
      } else {
        val nextStepAppended = previousSteps :+ game.step(previousSteps.last)
        stepUntilDone(nextStepAppended, stepsLeft - 1)
      }
    }

    stepUntilDone(Seq(initialStep), stepsToTake)
  }
}
