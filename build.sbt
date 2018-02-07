name := "ScalaZ80"

version := "0.1"

scalaVersion := "2.12.4"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.5"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"

scalacOptions ++= Seq(
  "-feature",
  "-language:existentials",
  "-language:higherKinds",
  "-language:implicitConversions"
)