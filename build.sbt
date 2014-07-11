name := "lamaVersion"

version := "1.0"

scalaVersion := "2.11.1"

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.0" % "test"

libraryDependencies ++= Seq(
    "joda-time" % "joda-time" % "2.3",
    "org.joda" % "joda-convert" % "1.2"
)

scalacOptions ++= Seq("-deprecation", "-feature")

mainClass := Some("com.lamaVersion.core.Manager")