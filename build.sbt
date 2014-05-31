
name := "9DeadMenMorris"

version := "1.0"

scalacOptions ++= Seq("-optimise", "-deprecation", "-unchecked", "-Yinline-warnings")

libraryDependencies ++= Seq(
  "com.typesafe.slick" %% "slick" % "2.0.2",
  "org.slf4j" % "slf4j-nop" % "1.6.4",
  "com.h2database" % "h2" % "1.3.173"
)


