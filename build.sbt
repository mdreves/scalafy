name := "scalafy"

organization := "org.scalafy"

version := "0.1.0"

libraryDependencies ++= Seq(
  "org.specs2" %% "specs2" % "1.6.1" % "test",
  "org.specs2" %% "scalaz-core" % "6.0.1" % "test"
)

// Workaround for bug: https://github.com/harrah/xsbt/issues/85
unmanagedClasspath in Compile += Attributed.blank(new java.io.File("doesnotexist"))
