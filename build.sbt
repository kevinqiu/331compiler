lazy val root = (project in file(".")).
  settings(
    name := "lex",
    version := "0.1",
    libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.1.1"
  )
