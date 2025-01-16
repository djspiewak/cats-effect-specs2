/*
 * Copyright 2021 Daniel Spiewak
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

name := "cats-effect-testing"

ThisBuild / baseVersion := "0.5"
ThisBuild / strictSemVer := false

ThisBuild / organization := "com.codecommit"
ThisBuild / publishGithubUser := "djspiewak"
ThisBuild / publishFullName := "Daniel Spiewak"

ThisBuild / crossScalaVersions := Seq("3.3.4", "2.12.19", "2.13.16")

ThisBuild / githubWorkflowTargetBranches := Seq("series/0.x")

ThisBuild / homepage := Some(url("https://github.com/djspiewak/cats-effect-testing"))

ThisBuild / scmInfo := Some(
  ScmInfo(
    url("https://github.com/djspiewak/cats-effect-testing"),
    "git@github.com:djspiewak/cats-effect-testing.git"))

val CatsEffectVersion = "2.5.5"

lazy val root = project
  .in(file("."))
  .aggregate(specs2, utest, minitest, scalatest, `scalatest-scalacheck`)
  .enablePlugins(NoPublishPlugin)

lazy val specs2 = project
  .in(file("specs2"))
  .settings(
    name := "cats-effect-testing-specs2",

    libraryDependencies += ("org.specs2" %% "specs2-core" % "4.20.9").cross(CrossVersion.for3Use2_13),

    mimaPreviousArtifacts := {
      if (isDotty.value)
        Set()
      else
        mimaPreviousArtifacts.value
    })
  .settings(libraryDependencies += "org.typelevel" %% "cats-effect" % CatsEffectVersion)

lazy val `scalatest-scalacheck` = project
  .in(file("scalatest-scalacheck"))
  .settings(
    name := "cats-effect-testing-scalatest-scalacheck",

    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-effect" % CatsEffectVersion,
      "org.scalatestplus" %% "scalacheck-1-16" % "3.2.14.0",
      "org.scalacheck" %% "scalacheck" % "1.18.1"),

    mimaPreviousArtifacts := mimaPreviousArtifacts.value - ("com.codecommit" %% name.value % "0.3.0")
  )
  .dependsOn(scalatest)

lazy val scalatest = project
  .settings(
    name := "cats-effect-testing-scalatest",

    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-effect" % CatsEffectVersion,
      "org.scalatest"    %% "scalatest" % "3.2.19"),

    mimaPreviousArtifacts := mimaPreviousArtifacts.value -- Seq(
      "com.codecommit" %% name.value % "0.1.0",
      "com.codecommit" %% name.value % "0.2.0" ))

lazy val utest = project
  .in(file("utest"))
  .settings(
    name := "cats-effect-testing-utest",

    testFrameworks += new TestFramework("utest.runner.Framework"),

    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-effect" % CatsEffectVersion,
      "org.typelevel" %% "cats-effect-laws" % CatsEffectVersion,
      "com.lihaoyi" %% "utest" % "0.8.5"))

lazy val minitest = project
  .in(file("minitest"))
  .settings(
    name := "cats-effect-testing-minitest",
    testFrameworks += new TestFramework("minitest.runner.Framework"),

    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-effect" % CatsEffectVersion,
      "org.typelevel" %% "cats-effect-laws" % CatsEffectVersion,
      "io.monix" %% "minitest" % "2.9.6"),

    mimaPreviousArtifacts := mimaPreviousArtifacts.value - ("com.codecommit" %% name.value % "0.1.0"))
