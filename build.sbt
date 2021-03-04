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

lazy val `software-testing` = (project in file(".")).aggregate(
  lab1,
  lab2
)

lazy val lab1 = (project in file("lab1"))
  .settings(
    name := "lab1",
    libraryDependencies ++= Seq()
  )

lazy val lab2 = (project in file("lab2"))
  .settings(
    name := "lab2",
    libraryDependencies ++= Seq()
  )