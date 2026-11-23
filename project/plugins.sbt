// Takes the version of the build from the latest tag (`git describe`)
addSbtPlugin("com.github.sbt" % "sbt-git" % "3.0.0")

// Signs the artifacts published to Maven Central (`publishSigned`)
addSbtPlugin("com.github.sbt" % "sbt-pgp" % "2.4.0")
