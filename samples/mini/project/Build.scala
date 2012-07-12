import sbt._
import Keys._
import PlayProject._

object ApplicationBuild extends Build {

    val appName         = "mini"
    val appVersion      = "1.0-SNAPSHOT"

    val appDependencies = Seq(
      "EPFL" %% "play-js-validation" % "0.2-SNAPSHOT"
    )

    val main = PlayProject(appName, appVersion, appDependencies, mainLang = SCALA).settings(
      resolvers += Resolver.url("ivy-local", url("file://" + Path.userHome + "/.ivy2/local"))(Resolver.ivyStylePatterns),
      scalacOptions ++= Seq("-deprecation", "-unchecked", "-Xexperimental", "-Yvirtualize")
    )

}
