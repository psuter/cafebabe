About
-----

Cafebabe is a tiny Scala library to generate JVM class files. It was originally
developed for the Compiler Construction course taught at EPFL. See
http://lara.epfl.ch/w/Teaching for more info.

The wiki contains some documentation.

Setup
-----

Releases for Scala 3 are published on Maven Central, under `ch.epfl.lara`. They are compiled
with Scala 3.9, and work with Scala 3.9 and later. Add the following to your project settings in
`build.sbt`, with the latest version:

```scala
libraryDependencies += "ch.epfl.lara" %% "cafebabe" % "<version>"
```

The last release for Scala 2.10, 2.11 and 2.12 is `1.2`, under the same coordinates.

Releasing
---------

Releases are published to Maven Central by the
[Release workflow](.github/workflows/release.yml). Run it from the Actions tab ("Run workflow" on
`main`), or with `gh workflow run release.yml -f version=v1.3.0`, giving the version with a
leading `v`. It tags the current commit (the build takes its version from the tag, through
`git describe`), tests, signs and publishes that version, then pushes the tag and creates the
GitHub release, with generated notes and the JAR attached. A version can't be published twice.

To check a release before it goes out, choose the mode `upload`: it only uploads the artifacts to
the [Central Portal](https://central.sonatype.com/publishing/deployments), to publish or drop
them there, and pushes no tag.

Locally, `sbt publishSigned` signs the artifacts and stages them in `target/sona-staging`.
