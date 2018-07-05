enablePlugins(JavaAppPackaging)

organization  := "org.renci.translator"

name          := "ctd-to-owl"

version       := "0.1"

licenses := Seq("BSD-3-Clause" -> url("https://opensource.org/licenses/BSD-3-Clause"))

scalaVersion  := "2.12.6"

scalacOptions := Seq("-unchecked", "-deprecation", "-encoding", "utf8")

scalacOptions in Test ++= Seq("-Yrangepos")

mainClass in Compile := Some("org.renci.translator.Main")

javaOptions += "-Xmx8G"

testFrameworks += new TestFramework("utest.runner.Framework")

fork in Test := true

libraryDependencies ++= {
  Seq(
    "org.scalaz"                  %% "scalaz-core"            % "7.2.24",
    "net.sourceforge.owlapi"      %  "owlapi-distribution"    % "4.5.2",
    "org.phenoscape"              %% "scowl"                  % "1.3.1",
    "com.typesafe.scala-logging"  %% "scala-logging"          % "3.9.0",
    "ch.qos.logback"              %  "logback-classic"        % "1.2.3",
    "org.codehaus.groovy"         %  "groovy-all"             % "2.4.6",
    "com.lihaoyi"                 %% "utest"                  % "0.6.3" % Test
  )
}
