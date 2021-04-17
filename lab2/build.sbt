libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % "2.3.0",
  "org.typelevel" %% "cats-effect" % "2.3.0",
  "org.typelevel" %% "cats-tagless-core" % "0.12",
  "org.typelevel" %% "cats-tagless-macros" % "0.12",

  "tf.tofu" % "tofu-core_2.13" % "0.10.0",
  "tf.tofu" % "tofu-logging_2.13" % "0.10.0",

  "com.beachape" % "enumeratum_2.13" % "1.6.1",

  "junit" % "junit" % "4.13" % Test,
  "org.mockito" % "mockito-core" % "3.9.0" % Test
)