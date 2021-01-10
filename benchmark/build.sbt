name := "jawn-benchmarks"

javaOptions in run += "-Xmx6G"

libraryDependencies ++= Seq(
  "io.argonaut" %% "argonaut" % "6.2.3",
  "org.json4s" %% "json4s-native" % "3.5.4",
  "org.json4s" %% "json4s-jackson" % "3.5.4",
  "com.typesafe.play" %% "play-json" % "2.6.9",
  "com.rojoma" %% "rojoma-json" % "2.4.3",
  "com.rojoma" %% "rojoma-json-v3" % "3.8.0",
  "io.spray" %% "spray-json" % "1.3.4",
  "org.parboiled" %% "parboiled" % "2.1.4",
  "com.fasterxml.jackson.core" % "jackson-annotations" % "2.9.6",
  "com.fasterxml.jackson.core" % "jackson-core" % "2.9.6",
  "com.fasterxml.jackson.core" % "jackson-databind" % "2.9.6",
  "com.google.code.gson" % "gson" % "2.8.5"
)

// enable forking in run
fork in run := true
