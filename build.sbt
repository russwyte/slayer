val scala3Version = "3.8.3"
val zioVersion    = "2.1.26"

ThisBuild / scalaVersion         := scala3Version
ThisBuild / organization         := "io.github.russwyte"
ThisBuild / organizationName     := "russwyte"
ThisBuild / organizationHomepage := Some(url("https://github.com/russwyte"))
ThisBuild / licenses             := List("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0.txt"))
ThisBuild / homepage             := Some(url("https://github.com/russwyte/slayer"))
ThisBuild / scmInfo              := Some(
  ScmInfo(
    url("https://github.com/russwyte/slayer"),
    "scm:git@github.com:russwyte/slayer.git",
  )
)
ThisBuild / developers := List(
  Developer(
    id = "russwyte",
    name = "Russ White",
    email = "356303+russwyte@users.noreply.github.com",
    url = url("https://github.com/russwyte"),
  )
)
ThisBuild / versionScheme := Some("early-semver")

usePgpKeyHex("2F64727A87F1BCF42FD307DD8582C4F16659A7D6")

addCommandAlias(
  "coverageTest",
  "coverage; clean; core/test; core/coverageReport",
)
addCommandAlias("cov", "coverageTest")

lazy val commonSettings = Seq(
  scalacOptions ++= Seq(
    "-deprecation",
    "-Wunused:all",
    "-feature",
  ),
  scalafixDependencies += "com.github.vovapolu" %% "scaluzzi" % "0.1.23",
)

lazy val publishSettings = Seq(
  publishMavenStyle    := true,
  pomIncludeRepository := { _ => false },
  publishTo            := {
    val centralSnapshots = "https://central.sonatype.com/repository/maven-snapshots/"
    if (isSnapshot.value) Some("central-snapshots" at centralSnapshots)
    else localStaging.value
  },
)

lazy val root = project
  .in(file("."))
  .aggregate(core)
  .settings(
    name           := "slayer-root",
    publish / skip := true,
  )

lazy val core = project
  .in(file("core"))
  .settings(commonSettings)
  .settings(publishSettings)
  .settings(
    name        := "slayer",
    description := "Slayer — N-arity, multi-param-list, generics-aware ZIO service stubbing",
    libraryDependencies ++= Seq(
      "dev.zio"        %% "zio"             % zioVersion    % "provided",
      "org.scala-lang" %% "scala3-compiler" % scala3Version % "provided",
      "dev.zio"        %% "zio-test"        % zioVersion    % Test,
      "dev.zio"        %% "zio-test-sbt"    % zioVersion    % Test,
    ),
    testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"),
    coverageFailOnMinimum := true,
    coverageExcludedFiles := ".*\\bMacros",
  )
