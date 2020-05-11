import sbt._
import Keys._

object Dependencies {

  object Versions {
    val cats = "2.2.0-M1"
    val `cats-effect` = "2.1.3"
  }

  val cats = Seq(
    "org.typelevel" %% "cats-core" % Versions.cats,
  )

  val catsEffect = Seq(
    "org.typelevel" %% "cats-effect" % Versions.`cats-effect`
  )
}
