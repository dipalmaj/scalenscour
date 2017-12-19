name := "scalenscour"

version := "0.1"

scalaVersion := "2.12.4"

//// https://mvnrepository.com/artifact/org.scalatest/scalatest_2.12.0-M2
//libraryDependencies += "org.scalatest" % "scalatest_2.12.0-M2" % "2.2.5-M2" % "test"
//
//// https://mvnrepository.com/artifact/org.scalactic/scalactic_2.12
//libraryDependencies += "org.scalactic" % "scalactic_2.12" % "3.0.1"

libraryDependencies ++= Seq(
  "org.scala-js" %% "scalajs-test-interface" % "0.6.14",
  "org.scalatest" %% "scalatest" % "3.0.1", //version changed as these the only versions supported by 2.12
  "com.novocode" % "junit-interface" % "0.11",
  "org.scala-lang" % "scala-library" % scalaVersion.value
)