def baseName = "ContextSnake"

def baseNameL = baseName.toLowerCase

def projectVersion = "0.1.2-SNAPSHOT"

def baseDescription = "A library for moving around in variable length Markov chains"

def lucreDataVersion = "2.2.2+"

def scalaTestVersion = "2.1.2"

lazy val commonSettings = Project.defaultSettings ++ Seq(
  version         := projectVersion,
  organization    := "de.sciss",
  scalaVersion    := "2.10.4",
  crossScalaVersions := Seq("2.11.0-RC3", "2.10.4"),
  homepage        := Some(url("https://github.com/Sciss/" + baseName)),
  licenses        := Seq("GPL v2+" -> url("http://www.gnu.org/licenses/gpl-2.0.txt")),
  initialCommands in console := """import de.sciss.contextsnake._""",
  // retrieveManaged := true,
  scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature"),
  scalacOptions ++= Seq("-Xelide-below", annotation.elidable.SEVERE.toString),
  // ---- publishing ----
  publishMavenStyle := true,
  publishTo := {
    Some(if (version.value endsWith "-SNAPSHOT")
      "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
    else
      "Sonatype Releases"  at "https://oss.sonatype.org/service/local/staging/deploy/maven2"
    )
  },
  publishArtifact in Test := false,
  pomIncludeRepository := { _ => false },
  pomExtra := { val n = baseName
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
)

lazy val subSettings = commonSettings ++ Seq(
  libraryDependencies += "org.scalatest" %% "scalatest" % scalaTestVersion % "test"
)

lazy val root = Project(
  id        = "root",
  base      = file("."),
  aggregate = Seq(mutable, txn),
  settings  = commonSettings ++ Seq(
    description := baseDescription,
    packagedArtifacts := Map.empty           // prevent publishing anything!
  )
)

lazy val mutable = Project(
  id            = s"$baseNameL-mutable",
  base          = file("mutable"),
  settings      = subSettings ++ Seq(
    name         := s"$baseName-mutable",
    description  := s"$baseDescription - using mutable data structures"
  )
)

lazy val txn = Project(
  id            = s"$baseNameL-txn",
  base          = file("txn"),
  settings      = subSettings ++ Seq(
    name         := s"$baseName-txn",
    description := s"$baseDescription - using transactional data structures",
    libraryDependencies += "de.sciss" %% "lucredata-core" % lucreDataVersion
  )
)

