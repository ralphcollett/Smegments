name := "smegment"

version := "0.1"

scalaVersion := "2.12.7"

libraryDependencies ++= Seq(
  "com.simple-http" % "simple-http" % "1.3"
)

val circeVersion = "0.10.0"

libraryDependencies ++= Seq(
  "io.circe" %% "circe-core",
  "io.circe" %% "circe-generic",
  "io.circe" %% "circe-parser"
).map(_ % circeVersion)
