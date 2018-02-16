name := "tree root project"

version in ThisBuild := "0.1-SNAPSHOT"

organization in ThisBuild := "org.rebeam"

// Plain Scala
//scalaVersion in ThisBuild := "2.12.4"

//Typelevel Scala, also see .jsSettings below
scalaOrganization in ThisBuild := "org.typelevel"
scalaVersion in ThisBuild := "2.12.4-bin-typelevel-4"

scalacOptions in ThisBuild ++= Seq(
  "-feature",
  "-deprecation",
  "-encoding", "UTF-8",
  "-unchecked",
  "-Xfatal-warnings",
  "-Xlint",
  "-Xcheckinit",
  "-Xlint:-unused",
  "-Ywarn-unused:imports",
  "-Ypartial-unification",
  "-language:existentials",
  "-language:higherKinds",
  "-Yno-adapted-args",
  "-Ywarn-dead-code",
  "-Ywarn-numeric-widen",
  "-Ywarn-value-discard",
  "-Xfuture"
  //"-Yno-predef" ?
)

testOptions in Test += Tests.Argument(TestFrameworks.ScalaCheck, "-verbosity", "2")

resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots")
)

javaOptions in ThisBuild := Seq("-Dorg.slf4j.simpleLogger.logFile=System.out")

//addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.4")

lazy val http4sVersion              = "0.18.0"  //done
lazy val circeVersion               = "0.9.1"   //done
lazy val catsVersion                = "1.0.1"   //done
lazy val scalajsReactVersion        = "1.1.1"   //done
lazy val shapelessVersion           = "2.3.3"   //done
lazy val monocleVersion             = "1.5.0-cats"   // or just 1.5.0 ?
lazy val scalacticVersion           = "3.0.0"   //Needed?
lazy val scalatestVersion           = "3.0.5"   //done
lazy val scalacheckVersion          = "1.13.5"  //done
lazy val slf4jVersion               = "1.7.25"

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

      "io.circe"                    %%% "circe-core"        % circeVersion,
      "io.circe"                    %%% "circe-generic"     % circeVersion,
      "io.circe"                    %%% "circe-parser"      % circeVersion,

      "org.typelevel"               %%% "cats-free"         % catsVersion,
      
      "com.chuusai"                 %%% "shapeless"         % shapelessVersion,

      "com.github.julien-truffaut"  %%% "monocle-core"      % monocleVersion,
      "com.github.julien-truffaut"  %%% "monocle-generic"   % monocleVersion,
      "com.github.julien-truffaut"  %%% "monocle-macro"     % monocleVersion,
      "com.github.julien-truffaut"  %%% "monocle-state"     % monocleVersion,
      "com.github.julien-truffaut"  %%% "monocle-refined"   % monocleVersion,
      "com.github.julien-truffaut"  %%% "monocle-law"       % monocleVersion      % "test",

      "org.scalactic"               %%% "scalactic"         % scalacticVersion    % "test",
      "org.scalatest"               %%% "scalatest"         % scalatestVersion    % "test",
      "org.scalacheck"              %%% "scalacheck"        % scalacheckVersion   % "test"

    ),

    //For @Lenses and Circe
    addCompilerPlugin("org.scalamacros" %% "paradise" % "2.1.0" cross CrossVersion.patch)

  ).jvmSettings(
    // Add JVM-specific settings here
    libraryDependencies ++= Seq(
      "org.http4s"  %% "http4s-blaze-server"  % http4sVersion,
      "org.http4s"  %% "http4s-dsl"           % http4sVersion,

      "org.slf4j"   % "slf4j-simple"          % slf4jVersion
    )

  ).jsSettings(
    // Add JS-specific settings here
    libraryDependencies ++= Seq(
      "com.github.japgolly.scalajs-react" %%% "core" % scalajsReactVersion,
      "com.github.japgolly.scalajs-react" %%% "extra" % scalajsReactVersion
    ),

    //Typelevel scala, see https://github.com/scala-js/scala-js/pull/2954#issuecomment-302743801
    // Remove the dependency on the scalajs-compiler
    libraryDependencies := libraryDependencies.value.filterNot(_.name == "scalajs-compiler"),
    // And add a custom one
    addCompilerPlugin("org.scala-js" % "scalajs-compiler" % scalaJSVersion cross CrossVersion.patch)

)

lazy val treeJVM = tree.jvm
lazy val treeJS = tree.js

