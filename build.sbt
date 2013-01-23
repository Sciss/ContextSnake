name := "ContextSnake"

version := "0.0.1"

organization := "de.sciss"

scalaVersion := "2.10.0"

libraryDependencies += "org.scalatest" %% "scalatest" % "1.9.1" % "test"

retrieveManaged := true

scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature", "-language:higherKinds")

scalacOptions ++= Seq("-Xelide-below", (annotation.elidable.SEVERE).toString)

initialCommands in console := "import de.sciss.contextsnake._"
