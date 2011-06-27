sbtPlugin := true

organization := "me.lessis"

name := "sbt gh issues"

version := "0.1.1-SNAPSHOT"

libraryDependencies ++= Seq(
   "net.databinder" %% "dispatch-lift-json" % "0.7.8"
)

scalacOptions += "-deprecation"

//publishTo := Resolver.file("lessis repo", new java.io.File("/var/www/repo"))
