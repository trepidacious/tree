name := "tree root project"

version in ThisBuild := "0.1-SNAPSHOT"

organization in ThisBuild := "org.rebeam"

scalaVersion in ThisBuild := "2.11.8"

scalacOptions in ThisBuild ++= Seq(
  "-feature",
  "-deprecation",
  "-encoding", "UTF-8",
  "-unchecked",
  "-Xfatal-warnings",
  "-Xlint"
)

//SLF4J simple logger, y u log to System.err by default, even for info?
javaOptions in ThisBuild := Seq("-Dorg.slf4j.simpleLogger.logFile=System.out")

lazy val http4sVersion = "0.14.2a"

lazy val monocleVersion = "1.2.2"     // or "1.3.0-SNAPSHOT"

val circeVersion = "0.5.1"

val scalajsReactVersion = "0.11.2"

val reactVersion = "15.3.2"

lazy val root = project.in(file(".")).
  aggregate(treeJS, treeJVM).
  settings(
    publish := {},
    publishLocal := {}
  )

lazy val tree = crossProject.in(file(".")).
  settings(
    name := "tree",
    version := "0.1-SNAPSHOT",
    libraryDependencies ++= Seq(
      "com.github.julien-truffaut"  %%%  "monocle-core"    % monocleVersion,
      "com.github.julien-truffaut"  %%%  "monocle-generic" % monocleVersion,
      "com.github.julien-truffaut"  %%%  "monocle-macro"   % monocleVersion,
      "com.github.julien-truffaut"  %%%  "monocle-state"   % monocleVersion,
      "com.github.julien-truffaut"  %%%  "monocle-refined" % monocleVersion,
      "com.github.julien-truffaut"  %%%  "monocle-law"     % monocleVersion % "test",
      "org.rebeam"                  %%%  "lenses-codec"    % "0.1-SNAPSHOT",
      "io.circe"                    %%%  "circe-core"      % circeVersion,
      "io.circe"                    %%%  "circe-generic"   % circeVersion,
      "io.circe"                    %%%  "circe-parser"    % circeVersion
    ),
    //For @Lenses
    addCompilerPlugin("org.scalamacros" %% "paradise" % "2.1.0" cross CrossVersion.full)

  ).jvmSettings(
  // Add JVM-specific settings here
  libraryDependencies ++= Seq(
    "org.http4s"  %% "http4s-blaze-server"  % http4sVersion,
    "org.http4s"  %% "http4s-dsl"           % http4sVersion,
    //  "org.http4s"  %% "http4s-argonaut"      % http4sVersion,

    "org.log4s"   %% "log4s"                % "1.2.1",

    "org.slf4j"   % "slf4j-simple"          % "1.7.21"
  )

).jsSettings(
  // Add JS-specific settings here
  libraryDependencies ++= Seq(
    "com.github.japgolly.scalajs-react" %%% "core" % scalajsReactVersion,
    "com.github.japgolly.scalajs-react" %%% "extra" % scalajsReactVersion,
    "com.payalabs" %%% "scalajs-react-mdl" % "0.2.0-SNAPSHOT"
  ),

  // React JS itself (Note the filenames, adjust as needed, eg. to remove addons.)
  jsDependencies ++= Seq(

    "org.webjars.bower" % "react" % reactVersion
      /        "react-with-addons.js"
      minified "react-with-addons.min.js"
      commonJSName "React",

    "org.webjars.bower" % "react" % reactVersion
      /         "react-dom.js"
      minified  "react-dom.min.js"
      dependsOn "react-with-addons.js"
      commonJSName "ReactDOM",

    "org.webjars.bower" % "react" % reactVersion
      /         "react-dom-server.js"
      minified  "react-dom-server.min.js"
      dependsOn "react-dom.js"
      commonJSName "ReactDOMServer"
  )
)

lazy val treeJVM = tree.jvm
lazy val treeJS = tree.js

