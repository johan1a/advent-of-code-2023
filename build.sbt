ThisBuild / organization := "se.johan1a"
ThisBuild / scalaVersion := "3.3.1"

lazy val root = (project in file(".")).
  settings(
    name := "advent-of-code-2023",
    libraryDependencies += "org.scalameta" %% "munit" % "1.0.0-M10" % Test
  )

(Test / test) := (Test / test).dependsOn(Test / scalafmtCheck).value

scalacOptions ++= Seq(
  "-Xfatal-warnings",
  "-Wunused:all",
  "-deprecation"
)
