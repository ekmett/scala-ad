import sbt._

class ad(info: ProjectInfo) extends DefaultProject(info) {
  val scalaToolsSnapshots = "Scala Tools Snapshots" at "http://scala-tools.org/repo-snapshots/"
  val scalazCore = "org.scalaz" %% "scalaz-core" % "6.0-SNAPSHOT"
}

