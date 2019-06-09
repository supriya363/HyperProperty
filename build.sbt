name := "ScalaParser"
 
version := "1.0"
 
scalaVersion := "2.12.3"
 
sbtVersion := "1.2.8"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.1" withSources()

EclipseKeys.createSrc := EclipseCreateSrc.Default + EclipseCreateSrc.ManagedClasses

EclipseKeys.withSource := true
