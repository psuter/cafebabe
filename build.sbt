name := "Cafebabe"

version := "1.2.1"

scalaVersion := "3.5.2"

crossScalaVersions := Seq("2.11.12", "2.12.10", "3.5.2")

scalacOptions ++= Seq(
    "-deprecation",
    "-unchecked"
)

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.10" % "test"

