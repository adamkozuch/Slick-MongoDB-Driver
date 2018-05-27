name := "SlickMongoDBDriver"

version := "1.0"

scalaVersion := "2.12.4"

val circleVersion = "0.9.1"

fork in Test := true
javaOptions ++= Seq("-Xms512M", "-Xmx2048M", "-XX:+CMSClassUnloadingEnabled")
libraryDependencies ++= Seq(
  "com.typesafe.slick" %% "slick" % "3.2.3",
  "org.slf4j" % "slf4j-nop" % "1.6.4",
  "org.mongodb" %% "casbah" % "3.1.1",
  "com.novocode" % "junit-interface" % "0.10" % "test",
  "org.scalatest" %% "scalatest" % "3.0.5" % "test"
//  "org.mongodb.scala" %% "mongo-scala-driver" % "2.2.1"
)
