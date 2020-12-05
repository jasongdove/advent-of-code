import mill._, scalalib._

object adventofcode extends ScalaModule {
  def scalaVersion = "2.13.4"

  def ivyDeps =
    Agg(
      ivy"org.typelevel::cats-effect:2.1.3",
      ivy"com.lihaoyi::os-lib:0.7.0"
    )

  object test extends Tests {
    def ivyDeps = Agg(
      ivy"com.disneystreaming::weaver-framework:0.5.0",
      ivy"com.disneystreaming::weaver-scalacheck:0.5.0",
      ivy"com.disneystreaming::weaver-zio:0.5.0"
    )
    def testFrameworks = Seq("weaver.framework.TestFramework")
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
