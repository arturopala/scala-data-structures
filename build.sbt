import scalariform.formatter.preferences._

organization := "me.arturopala"

name := "scala-data-structures"

version := "0.1.0-SNAPSHOT"

resolvers += Resolver.mavenLocal

scalaVersion := "2.11.7"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "2.2.5" % Test,
  "org.scalacheck" %% "scalacheck" % "1.12.5" % Test
).map(_.withSources())

com.typesafe.sbt.SbtScalariform.scalariformSettings

ScalariformKeys.preferences := PreferencesImporterExporter.loadPreferences(baseDirectory.value / "project" / "formatterPreferences.properties" toString)

fork := true

connectInput in run := true

outputStrategy := Some(StdoutOutput)