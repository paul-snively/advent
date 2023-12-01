name := "advent"

organization := "org.snively.paul"

version := "1.0.0"

scalaVersion := "2.13.12"

Compile / run / fork := true
Test / fork          := true

connectInput := true
outputStrategy := Some(StdoutOutput)

lazy val root = (project in file("."))
  .settings(
    libraryDependencies ++= List(
      "org.typelevel" %% "cats-parse" % "1.0.0",
      "co.fs2"        %% "fs2-io"     % "3.9.3"
    )
  )

libraryDependencies += {
  val version = scalaBinaryVersion.value match {
    case "2.10" => "1.0.3"
    case "2.11" => "1.6.7"
    case _ ⇒ "2.5.11"
  }
  "com.lihaoyi" % "ammonite" % version % "test" cross CrossVersion.full
}

Test / sourceGenerators += Def.task {
  val file = (Test / sourceManaged).value / "amm.scala"
  IO.write(file, """object amm extends App { ammonite.AmmoniteMain.main(args) }""")
  Seq(file)
}.taskValue

// Optional, required for the `source` command to work
(Test / fullClasspath) ++= {
  (Test / updateClassifiers).value
    .configurations
    .find(_.configuration.name == Test.name)
    .get
    .modules
    .flatMap(_.artifacts)
    .collect{case (a, f) if a.classifier == Some("sources") => f}
}
