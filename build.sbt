name := "porter2"

version := "1.0.0"

scalaVersion := "2.11.7"

lazy val scalatest = "org.scalatest" %% "scalatest" % "3.0.5"
lazy val scalacheck = "org.scalacheck" %% "scalacheck" % "1.13.4"
libraryDependencies += scalacheck % Test
libraryDependencies += scalatest % Test
