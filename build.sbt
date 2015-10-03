val morsechaos = project in file(".")

        name := "morsechaos"
     version := "1.0-SNAPSHOT"
scalaVersion := "2.11.7"

       maxErrors := 5
triggeredMessage := Watched.clearWhenTriggered

scalacOptions ++= Seq("-encoding", "utf8")
scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked", "-Xlint")
scalacOptions  += "-language:higherKinds"
scalacOptions  += "-language:implicitConversions"
scalacOptions  += "-language:postfixOps"
scalacOptions  += "-Xfuture"

libraryDependencies += "jline" % "jline" % "2.13"

initialCommands in console += "\nimport morsechaos._"

fork in run := true
connectInput in run := true
cancelable in Global := true

sources in (Compile, doc) := Nil
publishArtifact in (Compile, packageDoc) := false

watchSources ++= (baseDirectory.value * "*.sbt").get
watchSources ++= (baseDirectory.value / "project" * "*.scala").get
