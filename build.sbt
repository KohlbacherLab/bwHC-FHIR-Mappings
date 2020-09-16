


name := "fhir-mappings"
organization in ThisBuild := "de.bwhc"
scalaVersion in ThisBuild := "2.13.1"
version in ThisBuild := "1.0-SNAPSHOT"


//-----------------------------------------------------------------------------
// PROJECT
//-----------------------------------------------------------------------------

lazy val root = project.in(file("."))
  .settings(settings)
  .settings(
    libraryDependencies ++= Seq(
      "org.scalatest"    %% "scalatest"               % "3.0.8" % Test,
      "de.ekut.tbi"      %% "fhir-proof"              % "0.1-SNAPSHOT",
      "de.bwhc"          %% "utils"                   % "1.0-SNAPSHOT",
      "de.bwhc"          %% "hgnc-api"                % "1.0-SNAPSHOT",
      "de.bwhc"          %% "icd-catalogs-api"        % "1.0-SNAPSHOT",
      "de.bwhc"          %% "medication-catalog-api"  % "1.0-SNAPSHOT",
      "de.bwhc"          %% "data-entry-service-api"  % "1.0-SNAPSHOT"
   )
 )


//-----------------------------------------------------------------------------
// SETTINGS
//-----------------------------------------------------------------------------

lazy val settings = commonSettings

lazy val compilerOptions = Seq(
  "-encoding", "utf8",
  "-unchecked",
  "-Xfatal-warnings",
  "-feature",
//  "-language:existentials",
  "-language:higherKinds",
//  "-language:implicitConversions",
  "-language:postfixOps",
  "-deprecation"
)

lazy val commonSettings = Seq(
  scalacOptions ++= compilerOptions,
  resolvers ++= Seq(
    "Local Maven Repository" at "file://" + Path.userHome.absolutePath + "/.m2/repository",
    Resolver.sonatypeRepo("releases"),
    Resolver.sonatypeRepo("snapshots")
  )
)

