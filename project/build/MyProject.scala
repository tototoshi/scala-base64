import sbt._

class MyProject(info: ProjectInfo) extends DefaultProject(info){
  val scalaTest = "org.scalatest" % "scalatest" % "1.2"
}
