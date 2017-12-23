name := "tree root project"

version in ThisBuild := "0.1-SNAPSHOT"

organization in ThisBuild := "org.rebeam"

scalaVersion in ThisBuild := "2.11.12"

scalacOptions in ThisBuild ++= Seq(
  "-feature",
  "-deprecation",
  "-encoding", "UTF-8",
  "-unchecked",
  "-Xfatal-warnings",
  "-Xlint",
  "-Xcheckinit"
)

testOptions in Test += Tests.Argument(TestFrameworks.ScalaCheck, "-verbosity", "2")

resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots")
)

lazy val http4sVersion              = "0.15.5a"
lazy val circeVersion               = "0.7.0"
lazy val catsVersion                = "0.9.0"
lazy val scalajsReactVersion        = "0.11.3"
lazy val shapelessVersion           = "2.3.2"

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
      "org.rebeam"                  %%% "lenses-codec"    % "0.1-SNAPSHOT",

      "io.circe"                    %%% "circe-core"      % circeVersion,
      "io.circe"                    %%% "circe-generic"   % circeVersion,
      "io.circe"                    %%% "circe-parser"    % circeVersion,

      "org.typelevel"               %%% "cats"            % catsVersion,
      
      "com.chuusai"                 %%% "shapeless"       % shapelessVersion,

      "org.scalactic"               %%% "scalactic"       % "3.0.0"             % "test",
      "org.scalatest"               %%% "scalatest"       % "3.0.0"             % "test",
      "org.scalacheck"              %%% "scalacheck"      % "1.13.4"            % "test"
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

