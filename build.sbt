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

lazy val http4sVersion = "0.14.2a"

val circeVersion = "0.5.1"

val scalajsReactVersion = "0.11.3"

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
      "org.rebeam"                  %%%  "lenses-codec"     % "0.1-SNAPSHOT",

      "io.circe"                    %%%  "circe-core"       % circeVersion,
      "io.circe"                    %%%  "circe-generic"    % circeVersion,
      "io.circe"                    %%%  "circe-parser"     % circeVersion,

      "org.scalactic"               %%% "scalactic"         % "3.0.0"             % "test",
      "org.scalatest"               %%% "scalatest"         % "3.0.0"             % "test"
    ),

    //For @Lenses
    addCompilerPlugin("org.scalamacros" %% "paradise" % "2.1.0" cross CrossVersion.full)

  ).jvmSettings(
    // Add JVM-specific settings here
    libraryDependencies ++= Seq(
      "org.http4s"  %% "http4s-blaze-server"  % http4sVersion,
      "org.http4s"  %% "http4s-dsl"           % http4sVersion
    )

  ).jsSettings(
    // Add JS-specific settings here
    libraryDependencies ++= Seq(
      "com.github.japgolly.scalajs-react" %%% "core" % scalajsReactVersion,
      "com.github.japgolly.scalajs-react" %%% "extra" % scalajsReactVersion
    )
  )

lazy val treeJVM = tree.jvm
lazy val treeJS = tree.js

