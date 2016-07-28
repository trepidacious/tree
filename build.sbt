enablePlugins(ScalaJSPlugin)

name := "tree"

version := "0.1-SNAPSHOT"

scalaVersion := "2.11.8"

libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.9.0"

libraryDependencies += "com.lihaoyi" %%% "scalatags" % "0.5.5"

libraryDependencies += "com.github.japgolly.fork.monocle" %%% "monocle-core" % "1.2.0"

libraryDependencies += "com.github.japgolly.fork.monocle" %%% "monocle-generic" % "1.2.0"

libraryDependencies += "com.github.japgolly.fork.monocle" %%% "monocle-state" % "1.2.0"

libraryDependencies += "com.github.japgolly.fork.monocle" %%% "monocle-macro" % "1.2.0"

addCompilerPlugin(compilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full))

libraryDependencies += "com.lihaoyi" %%% "upickle" % "0.4.1"

scalaJSUseRhino in Global := false

scalacOptions ++= Seq(
  "-feature",
  "-deprecation",
  "-encoding", "UTF-8",
  "-unchecked",
  "-Xfatal-warnings",
  "-Xlint"
)
