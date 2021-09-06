name := "Cafebabe"

version := "1.2"

scalaVersion := "2.13.6"

crossScalaVersions := Seq("2.11.12", "2.12.10")

scalacOptions ++= Seq(
    "-deprecation",
    "-unchecked"
)

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % "test"

