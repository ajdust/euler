name := "EulerScala"

version := "1.0"


scalaVersion := "2.11.6"
scalaVersion in ThisBuild := "2.11.6"


libraryDependencies ++= Seq(
  "org.scalactic" %% "scalactic" % "3.0.4",
  "org.scalatest" %% "scalatest" % "3.0.4" % "test",
  "com.fasterxml.jackson.module" %% "jackson-module-scala" % "2.7.2"
)
