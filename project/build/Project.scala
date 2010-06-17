import sbt._

class Project(info: ProjectInfo) extends DefaultProject(info) {
  val scalaToolsSnapshots = "Scala-Tools Maven2 Snapshots Repository" at "http://scala-tools.org/repo-snapshots"
  val mavenLocal = "Local Maven Repository" at "file://"+Path.userHome+"/.m2/repository"
}
