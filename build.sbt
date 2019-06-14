organization in ThisBuild := "oddco"
version in ThisBuild := "0.1.0-SNAPSHOT"
scalacOptions in ThisBuild ++= Seq("-deprecation", "-feature", "-unchecked", "-language:higherKinds")
homepage in ThisBuild := Some(url("https://github.com/odd/keeping-order-while-hashed"))
scmInfo in ThisBuild := Some(
  ScmInfo(
    url("https://github.com/odd/keeping-order-while-hashed"),
    "scm:git:git@github.com:odd/keeping-order-while-hashed.git"
  ))
publishArtifact in ThisBuild := false
// The above is enough for Maven repos but it doesn't prevent publishing of ivy.xml files
publish in ThisBuild := ((): Unit)
publishLocal in ThisBuild := ((): Unit)
pomExtra in ThisBuild :=
    <developers>
      <developer><id>odd</id><name>Odd Möller</name></developer>
    </developers>

lazy val root = (project in file("."))
    .settings(name := "keeping-order-while-hashed")
    .aggregate(`collections-2-12`, `collections-2-13`, time, memory)

lazy val `collections-2-12` = (project in file("collections/2.12"))
    .settings(scalaVersion := "2.12.8")
    .settings(
      libraryDependencies += "com.lihaoyi" %% "utest" % "0.6.7" % Test,
      libraryDependencies += "io.github.stanch" %% "reftree" % "1.4.1-SNAPSHOT",
      libraryDependencies += "com.lihaoyi" % "ammonite" % "1.6.7" % "test" cross CrossVersion.full,
      testFrameworks += new TestFramework("utest.runner.Framework"),
      sourceGenerators in Test += Def.task {
        val file = (sourceManaged in Test).value / "amm.scala"
        IO.write(file, """object amm extends App { ammonite.Main.main(args) }""")
        Seq(file)
      }.taskValue,
      // Optional, required for the `source` command to work
      (fullClasspath in Test) ++= {
        (updateClassifiers in Test).value
            .configurations
            .find(_.configuration.name == Test.name)
            .get
            .modules
            .flatMap(_.artifacts)
            .collect{case (a, f) if a.classifier.contains("sources") => f}
      }
    )

lazy val `collections-2-13` = (project in file("collections/2.13"))
    .settings(scalaVersion := "2.13.0-RC3")
    .settings(libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % Test)

val memory = project.in(file("memory"))
    .settings(scalaVersion := "2.13.0-RC3")
    .dependsOn(`collections-2-13`)

val time = project.in(file("time"))
    .settings(scalaVersion := "2.13.0-RC3")
    .dependsOn(`collections-2-13`)
    .enablePlugins(JmhPlugin)
    .settings(mainClass in (Jmh, run) := Some("odd.orderkeeping.time.JmhRunner"))
