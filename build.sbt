def baseName          = "ContextSnake"

def baseNameL         = baseName.toLowerCase

def projectVersion    = "0.2.0-SNAPSHOT"

def baseDescription   = "A library for moving around in variable length Markov chains"

def lucreDataVersion  = "2.2.3"

def lucreSTMVersion   = "2.0.4"

def kollFlitzVersion  = "0.2.0"

def numbersVersion    = "0.1.1"

def swingPlusVersion  = "0.1.2"

def scalaTestVersion  = "2.1.7"

lazy val commonSettings = Project.defaultSettings ++ Seq(
  version         := projectVersion,
  organization    := "de.sciss",
  scalaVersion    := "2.11.1",
  crossScalaVersions := Seq("2.11.1", "2.10.4"),
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
    libraryDependencies ++= Seq(
      "de.sciss" %% "lucredata-core" % lucreDataVersion,
      "de.sciss" %% "lucrestm-bdb"   % lucreSTMVersion  % "test",
      "de.sciss" %% "kollflitz"      % kollFlitzVersion % "test",
      "de.sciss" %% "numbers"        % numbersVersion   % "test",
      "de.sciss" %% "swingplus"      % swingPlusVersion % "test"
    ),
    libraryDependencies += { val sv = scalaVersion.value
      val swing = if (sv startsWith "2.10")
        "org.scala-lang" % "scala-swing" % sv
      else
        "org.scala-lang.modules" %% "scala-swing" % "1.0.1"
      swing % "test"
    }
  )
)

