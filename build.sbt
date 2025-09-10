ThisBuild / scalaVersion     := "3.7.2"
ThisBuild / version          := "0.1.0-SNAPSHOT"

lazy val root = (project in file("."))
  .settings(
    name := "Vernix",
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio" % "2.1.17",
      "dev.zio" %% "zio-http" % "3.0.1",
      "dev.zio" %% "zio-test" % "2.1.11" % Test,
      "dev.zio" %% "zio-interop-cats" % "23.1.0.5",
      "org.typelevel" %% "cats-core" % "2.9.0",
      "org.typelevel" %% "spire" % "0.18.0",
      "nl.vroste" %% "rezilience" % "0.10.4",
      "com.lihaoyi" %% "fastparse" % "3.1.1",
    ),
    testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")
  )
