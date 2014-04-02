name                := "ContextSnake"

version             := "0.1.2-SNAPSHOT"

organization        := "de.sciss"

scalaVersion        := "2.10.4"

description         := "A library for moving around in variable length Markov chains"

homepage            := Some(url("https://github.com/Sciss/" + name.value))

licenses            := Seq("GPL v2+" -> url("http://www.gnu.org/licenses/gpl-2.0.txt"))

libraryDependencies += "org.scalatest" %% "scalatest" % "2.1.2" % "test"

// retrieveManaged     := true

scalacOptions      ++= Seq("-deprecation", "-unchecked", "-feature", "-language:higherKinds")

scalacOptions      ++= Seq("-Xelide-below", (annotation.elidable.SEVERE).toString)

initialCommands in console := "import de.sciss.contextsnake._"

// ---- publishing ----

publishMavenStyle := true

publishTo :=
  Some(if (version.value endsWith "-SNAPSHOT")
    "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
  else
    "Sonatype Releases"  at "https://oss.sonatype.org/service/local/staging/deploy/maven2"
  )

publishArtifact in Test := false

pomIncludeRepository := { _ => false }

pomExtra := { val n = name.value
<scm>
  <url>git@github.com:Sciss/{n}.git</url>
  <connection>scm:git:git@github.com:Sciss/{n}.git</connection>
</scm>
<developers>
  <developer>
    <id>sciss</id>
    <name>Hanns Holger Rutz</name>
    <url>http://www.sciss.de</url>
  </developer>
</developers>
}
