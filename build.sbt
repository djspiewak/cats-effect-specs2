/*
 * Copyright 2019 Daniel Spiewak
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

ThisBuild / baseVersion := "0.1"

ThisBuild / organization := "com.codecommit"
ThisBuild / publishGithubUser := "djspiewak"
ThisBuild / publishFullName := "Daniel Spiewak"

ThisBuild / homepage := Some(url("https://github.com/djspiewak/cats-effect-testing"))

ThisBuild / scmInfo := Some(
  ScmInfo(
    url("https://github.com/djspiewak/cats-effect-testing"),
    "git@github.com:djspiewak/cats-effect-testing.git"))

val catsEffectVersion = "2.0.0-RC1"

lazy val root = project
  .in(file("."))
  .aggregate(specs2, utest)
  .settings(noPublishSettings)

lazy val specs2 = project
  .in(file("specs2"))
  .settings(
    name := "cats-effect-testing-specs2",

    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-effect" % catsEffectVersion,
      "org.specs2"    %% "specs2-core" % "4.6.0"))

lazy val utest = project
  .in(file("utest"))
  .settings(
    name := "cats-effect-testing-utest",
    testFrameworks += new TestFramework("utest.runner.Framework"),

    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-effect" % catsEffectVersion,
      "org.typelevel" %% "cats-effect-laws" % catsEffectVersion,
      "com.lihaoyi" %% "utest" % "0.7.1"))
