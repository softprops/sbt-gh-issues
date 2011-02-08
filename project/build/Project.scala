import sbt._

class Project(info: ProjectInfo) extends PluginProject(info)
  with posterous.Publish {

  val dbjs = "net.databinder" %% "dispatch-lift-json" % "0.7.8"
  override def managedStyle = ManagedStyle.Maven
  lazy val publishTo = Resolver.file("lessis repo", new java.io.File("/var/www/repo"))
}
