name := "ants"

scalaVersion := "2.12.3"

scalacOptions ++= Seq(
  "-feature",
  "-deprecation",
  "-Ypartial-unification",
  "-language:higherKinds",
)

lazy val akkaVersion = "2.5.4"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor" % akkaVersion,
  "com.typesafe.akka" %% "akka-testkit" % akkaVersion,

  "com.typesafe.akka" %% "akka-typed" % akkaVersion,
  "com.typesafe.akka" %% "akka-typed-testkit" % akkaVersion,

  //  "org.atnos" %% "eff-monix" % "4.6.1",
  "org.typelevel" %% "cats-free" % "1.0.0-MF",
  "org.typelevel" %% "cats-effect" % "0.4",

  "org.scalatest" %% "scalatest" % "3.0.1" % "test",
)

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.4")
