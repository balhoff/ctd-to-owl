enablePlugins(JavaAppPackaging)

organization  := "org.renci.translator"

name          := "ctd-to-owl"

version       := "0.2"

licenses := Seq("BSD-3-Clause" -> url("https://opensource.org/licenses/BSD-3-Clause"))

scalaVersion  := "2.13.1"

scalacOptions := Seq("-unchecked", "-deprecation", "-encoding", "utf8")

scalacOptions in Test ++= Seq("-Yrangepos")

mainClass in Compile := Some("org.renci.translator.ctd.Main")

javaOptions += "-Xmx8G"

testFrameworks += new TestFramework("utest.runner.Framework")

fork in Test := true

libraryDependencies ++= {
  Seq(
    "org.scala-lang.modules"      %% "scala-xml"              % "1.2.0",
    "org.scalaz"                  %% "scalaz-core"            % "7.2.30",
    "net.sourceforge.owlapi"      %  "owlapi-distribution"    % "4.5.15",
    "org.phenoscape"              %% "scowl"                  % "1.3.4",
    "com.outr"                    %% "scribe-slf4j"           % "2.7.10",
    "com.lihaoyi"                 %% "utest"                  % "0.7.4" % Test
  )
}
