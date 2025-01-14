import sbt.Keys.libraryDependencies

scalaVersion := "3.3.4"
lazy val root = (project in file("."))
  .settings(
    name := "Trial_Projects", // ADD COMMA HERE
    libraryDependencies ++= {
      // Determine OS version of JavaFX binaries
      val osName = System.getProperty("os.name") match {
        case n if n.startsWith("Linux")   => "linux"
        case n if n.startsWith("Mac")     => "mac"
        case n if n.startsWith("Windows") => "win"
        case _ => throw new Exception("Unknown platform!")
      }
      Seq("base", "controls", "fxml", "graphics", "media", "swing", "web")
        .map(m => "org.openjfx" % s"javafx-$m" % "21.0.4" classifier osName)
    },
    // Install ScalaFX 21 for Scala 3, from Maven Repository

    libraryDependencies += "org.scalafx" %% "scalafx" % "21.0.0-R32"
  )