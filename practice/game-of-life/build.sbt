lazy val commonSettings = Seq(
  organization := "com.mindwidgets",
  version := "0.1.0",
  scalaVersion := "2.11.7",
  coverageEnabled := true
)

lazy val root = (project in file(".")).
  settings(commonSettings: _*).
  settings(
    name := "game-of-life",
    libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test"
  )
