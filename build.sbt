enablePlugins(GitVersioning)

git.useGitDescribe := true

// Published to Maven Central under the namespace of the lab, as ch.epfl.lara:cafebabe_3
organization := "ch.epfl.lara"

name := "cafebabe"

// The standard Scala 3.9 (LTS), so that the published library works with Scala 3.9 and later,
// including the Scala 3 compiler of the lab that Inox and Stainless use
scalaVersion := "3.9.0"

scalacOptions ++= Seq(
    "-deprecation",
    "-unchecked",
    "-feature"
)

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.19" % "test"

// Publishing to Maven Central (see .github/workflows/release.yml). The version comes from the
// latest tag, through `git describe` (sbt-git): v1.3.0 gives 1.3.0, and -SNAPSHOT is appended when
// the working tree has uncommitted changes. `sbt publishSigned` signs the artifacts and stages
// them locally, `sbt sonaRelease` uploads them to the Central Portal and releases them (`sbt
// sonaUpload` only uploads them, to inspect them in the Portal first). Both read the Portal token
// from SONATYPE_USERNAME and SONATYPE_PASSWORD.
description := "A Scala library to generate JVM class files"

homepage := Some(uri("https://github.com/epfl-lara/cafebabe"))

licenses := Seq("BSD-2-Clause-Views" -> uri("https://github.com/epfl-lara/cafebabe/blob/main/LICENSE"))

scmInfo := Some(ScmInfo(
  uri("https://github.com/epfl-lara/cafebabe"),
  "scm:git:git@github.com:epfl-lara/cafebabe.git",
))

developers := List(
  Developer("psuter", "Philippe Suter", "", uri("https://github.com/psuter")),
  Developer("epfl-lara", "EPFL Lab for Automated Reasoning and Analysis", "lara.maven@epfl.ch", uri("https://lara.epfl.ch")),
)

publishMavenStyle := true

Test / publishArtifact := false

pomIncludeRepository := { _ => false }

publishTo := localStaging.value

// The key that signs the artifacts, when the keyring holds several. Not cached: sbt would
// otherwise keep the value of a previous run, since it can't see the environment variable.
pgpSigningKey := Def.uncached(sys.env.get("PGP_FINGERPRINT"))
