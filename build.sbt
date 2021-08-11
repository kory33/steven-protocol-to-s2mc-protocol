name := "steven-protocol-to-s2mc-protocol"

version := "0.1"

scalaVersion := "3.0.1"

idePackagePrefix := Some("com.github.kory33.s2mctest.protocolconversion")

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % "2.6.1",
  "org.typelevel" %% "cats-kernel" % "2.6.1",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "2.0.0"
)
