import mill._, scalalib._, scalafmt._

object adventofcode extends ScalaModule with ScalafmtModule {
  def scalaVersion = "2.13.7"

  // try to limit bad algorithms from hanging the system
  def forkArgs = Seq(
    "-Xms64m",
    "-Xmx1g"
  )

  def ivyDeps =
    Agg(
      ivy"org.typelevel::cats-effect:3.3.0",
      ivy"com.lihaoyi::os-lib:0.7.8",
      ivy"com.lihaoyi::upickle:1.4.2",
      ivy"commons-codec:commons-codec:1.15"
    )

  object test extends Tests {
    def ivyDeps = Agg(
      ivy"com.disneystreaming::weaver-cats:0.7.7"
    )
    def testFramework = "weaver.framework.CatsEffect"
  }

  def scalacOptions =
    Seq(
      "-deprecation",
      "-encoding",
      "utf-8",
      "-explaintypes",
      "-feature",
      "-language:reflectiveCalls",
      "-unchecked",
      "-Yrangepos",
      "-Xcheckinit",
      "-Xfatal-warnings",
      "-Xlint:constant",
      "-Xlint:delayedinit-select",
      "-Xlint:doc-detached",
      "-Xlint:inaccessible",
      "-Xlint:infer-any",
      "-Xlint:missing-interpolator",
      "-Xlint:nullary-unit",
      "-Xlint:option-implicit",
      "-Xlint:package-object-classes",
      "-Xlint:poly-implicit-overload",
      "-Xlint:private-shadow",
      "-Xlint:stars-align",
      "-Xlint:type-parameter-shadow",
      "-Ywarn-dead-code",
      "-Ywarn-extra-implicit",
      "-Ywarn-numeric-widen",
      "-Ywarn-unused:implicits",
      "-Ywarn-unused:imports",
      "-Ywarn-unused:locals",
      "-Ywarn-unused:params",
      "-Ywarn-unused:patvars",
      "-Ywarn-unused:privates",
      "-Ywarn-value-discard"
    )
}
