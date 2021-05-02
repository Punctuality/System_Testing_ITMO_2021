//import sbt.Keys.mainClass

ThisBuild / scalaVersion := "2.13.5"
ThisBuild / version := "0.0.1"

ThisBuild / scalacOptions ++= Seq(
  "-deprecation",
  "-unchecked",
  "-language:existentials",
  "-language:higherKinds",
  "-language:postfixOps",
  "-language:implicitConversions",
  "-Xlint",
  "-Xlint:deprecation",
  //  "-Xfatal-warnings"
)

val silencerVersion = "1.7.3"
val scalaMacrosVersion = "2.1.1"
val kindProjectorVersion = "0.11.3"

lazy val commonSettings = Seq(
  compilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1"),
  compilerPlugin("org.typelevel" % "kind-projector" % kindProjectorVersion cross CrossVersion.full),
  compilerPlugin("com.github.ghik" % "silencer-plugin" % silencerVersion cross CrossVersion.full),
  "com.github.ghik" % "silencer-lib" % silencerVersion % Provided cross CrossVersion.full
)

lazy val `software-testing` = (project in file(".")).aggregate(
  lab1,
  lab2
)

lazy val lab1 = (project in file("lab1"))
  .settings(
    name := "lab1",
    libraryDependencies ++= commonSettings
  )

lazy val lab2 = (project in file("lab2"))
  .settings(
    name := "lab2",
    libraryDependencies ++= commonSettings
  ).dependsOn(lab1)

lazy val lab3 = (project in file("lab3"))
  .settings(
    name := "lab3",
    libraryDependencies ++= commonSettings
  )