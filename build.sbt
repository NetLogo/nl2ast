name := "nl2ast"

scalaVersion := "3.7.2"

scalacOptions ++=
  "-deprecation -unchecked -feature -encoding us-ascii -Xfatal-warnings -Wunused:all".split(" ").toSeq

isSnapshot := true // Used by the publish-versioned plugin
publishTo  := { Some("Cloudsmith API" at "https://maven.cloudsmith.io/netlogo/ast2json/") }

Compile / scalaSource := baseDirectory.value / "src" / "main"

resolvers += "netlogoheadless" at "https://dl.cloudsmith.io/public/netlogo/netlogo/maven/"

libraryDependencies ++= Seq(
  "org.nlogo" % "netlogoheadless" % "7.0.0-RC1"
, "org.playframework" %% "play-json" % "3.0.5"
)
