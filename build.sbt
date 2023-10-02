enablePlugins(JavaAppPackaging)

organization  := "org.renci.translator"

name          := "ctd-to-owl"

version       := "0.3.0"

licenses := Seq("BSD-3-Clause" -> url("https://opensource.org/licenses/BSD-3-Clause"))

scalaVersion  := "2.13.12"

scalacOptions := Seq("-unchecked", "-deprecation", "-encoding", "utf8")

scalacOptions in Test ++= Seq("-Yrangepos")

mainClass in Compile := Some("org.renci.translator.ctd.Main")

javaOptions += "-Xmx8G"

testFrameworks += new TestFramework("utest.runner.Framework")

fork in Test := true

libraryDependencies ++= {
  Seq(
    "org.scala-lang.modules"      %% "scala-xml"              % "2.2.0",
    "org.scalaz"                  %% "scalaz-core"            % "7.3.7",
    "net.sourceforge.owlapi"      %  "owlapi-distribution"    % "4.5.15",
    "org.phenoscape"              %% "scowl"                  % "1.4.1",
    "com.outr"                    %% "scribe-slf4j"           % "3.11.2",
    "com.lihaoyi"                 %% "utest"                  % "0.8.1" % Test
  )
}
