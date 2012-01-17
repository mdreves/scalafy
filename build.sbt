// See https://github.com/harrah/xsbt/wiki/Getting-Started-Welcome

name := "scalafy"

version := "0.1"

scalaVersion := "2.9.1"

scalacOptions ++= Seq("-unchecked", "-deprecation")

// Notes:
//   * "groupId" % "artifactId" % "revision" % "configuration"
//   * Main configurations are "test" and "compile"
//   * If no "configuration" used then "compile" assumed.
//   * Using %% fills in scala version tag  (specs2_2.9.0 vs specs2)
libraryDependencies ++= Seq(
  "log4j" % "log4j" % "1.2.16",
  "org.slf4j" % "slf4j-log4j12" % "1.6.4",
  "junit" % "junit" % "4.10" % "test",
  "org.specs2" %% "specs2" % "1.6.1" % "test",
  "org.specs2" %% "scalaz-core" % "6.0.1" % "test",
  "org.scala-tools.testing" %% "scalacheck" % "1.9" % "test"
)

// Notes:
//   * Standard Maven repo and scala-tools.org repos are added by default
//resolvers ++= Seq(
// "Scala-Tools Snapshots Repo" at "http://scala-tools.org/repo-snapshots",
// "Local Repo" at "file://"+Path.userHome.absolutePath+"/.m2/repository"
//)
