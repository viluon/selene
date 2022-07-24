name := "selene"

scalaVersion := "2.11.2"
scalaOrganization := "org.scala-lang.virtualized"

resolvers += Resolver.sonatypeRepo("snapshots")

libraryDependencies += "org.scala-lang.lms" %% "lms-core" % "1.0.0-SNAPSHOT"
libraryDependencies += "org.scala-lang.virtualized" % "scala-compiler" % scalaVersion.value
libraryDependencies += "org.scala-lang.virtualized" % "scala-library" % scalaVersion.value
libraryDependencies += "org.scala-lang.virtualized" % "scala-reflect" % scalaVersion.value
libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.2"

scalacOptions += "-Yvirtualize"
scalacOptions += "-deprecation"
