enablePlugins(ScalaJSPlugin)

name := "tree"

version := "0.1-SNAPSHOT"

scalaVersion := "2.11.8"

//libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.9.0"

//libraryDependencies += "com.lihaoyi" %%% "scalatags" % "0.5.5"

libraryDependencies += "com.github.japgolly.fork.monocle" %%% "monocle-core" % "1.2.0"

libraryDependencies += "com.github.japgolly.fork.monocle" %%% "monocle-generic" % "1.2.0"

libraryDependencies += "com.github.japgolly.fork.monocle" %%% "monocle-state" % "1.2.0"

libraryDependencies += "com.github.japgolly.fork.monocle" %%% "monocle-macro" % "1.2.0"

addCompilerPlugin(compilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full))

libraryDependencies += "com.lihaoyi" %%% "upickle" % "0.4.1"

// core = essentials only. No bells or whistles.
libraryDependencies += "com.github.japgolly.scalajs-react" %%% "core" % "0.11.1"

libraryDependencies += "com.github.japgolly.scalajs-react" %%% "extra" % "0.11.1"

// React JS itself (Note the filenames, adjust as needed, eg. to remove addons.)
jsDependencies ++= Seq(

  "org.webjars.bower" % "react" % "15.2.1"
    /        "react-with-addons.js"
    minified "react-with-addons.min.js"
    commonJSName "React",

  "org.webjars.bower" % "react" % "15.2.1"
    /         "react-dom.js"
    minified  "react-dom.min.js"
    dependsOn "react-with-addons.js"
    commonJSName "ReactDOM",

  "org.webjars.bower" % "react" % "15.2.1"
    /         "react-dom-server.js"
    minified  "react-dom-server.min.js"
    dependsOn "react-dom.js"
    commonJSName "ReactDOMServer")


scalaJSUseRhino in Global := false

scalacOptions ++= Seq(
  "-feature",
  "-deprecation",
  "-encoding", "UTF-8",
  "-unchecked",
  "-Xfatal-warnings",
  "-Xlint"
)
