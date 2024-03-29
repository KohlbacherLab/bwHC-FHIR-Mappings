


name := "fhir-mappings"
ThisBuild / organization := "de.bwhc"
ThisBuild / scalaVersion := "2.13.8"
ThisBuild / version      := "1.0"


//-----------------------------------------------------------------------------
// PROJECT
//-----------------------------------------------------------------------------

lazy val root = project.in(file("."))
  .settings(settings)
  .settings(
    libraryDependencies ++= Seq(
      "de.ekut.tbi"       %% "fhir-proof"                        % "0.1-SNAPSHOT",
      "de.bwhc"           %% "mtb-dtos"                          % "1.0",
      "de.bwhc"           %% "mtb-dto-generators"                % "1.0" % Test,
      "ca.uhn.hapi.fhir"  %  "hapi-fhir-base"                    % "5.2.1" % Test,
      "ca.uhn.hapi.fhir"  %  "hapi-fhir-structures-r4"           % "5.2.1" % Test,
      "ca.uhn.hapi.fhir"  %  "hapi-fhir-validation-resources-r4" % "5.2.1" % Test,
      "org.scalatest"     %% "scalatest"                         % "3.1.1" % Test,
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
  resolvers ++=
    Seq("Local Maven Repository" at "file://" + Path.userHome.absolutePath + "/.m2/repository") ++
    Resolver.sonatypeOssRepos("releases") ++
    Resolver.sonatypeOssRepos("snapshots")
)
