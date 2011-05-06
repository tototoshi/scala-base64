import sbt._

class MyProject(info: ProjectInfo) extends DefaultProject(info){
  val scalaTest = "org.scalatest" % "scalatest" % "1.3"
  val commonslang = "commons-io" % "commons-io" % "2.0.1"
}
