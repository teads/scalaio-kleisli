import pl.project13.scala.sbt.JmhPlugin

scalaVersion := "2.12.3"

libraryDependencies += "org.typelevel" %% "cats-core" % "1.1.0"

enablePlugins(JmhPlugin)
