import sbt._
import Keys._

object SReflectionBuild extends Build {

	/* Dependencies */
	val junit = "junit" % "junit" % "4.4" % "test"
	val specs2 = "org.specs2" %% "specs2" % "1.13" % "test"

	/* Resolvers */
	val customResolvers = Seq(
		"snapshots" at "http://scala-tools.org/repo-snapshots",
		"releases" at "http://scala-tools.org/repo-releases",
		"Maven" at "http://repo1.maven.org/maven2/"
	)

	lazy val sReflection =
		Project(
			id = "sreflection",
			base = file("."),
			settings = Defaults.defaultSettings ++ Seq(
				libraryDependencies ++=
					Seq(junit, specs2),
				libraryDependencies <<= (scalaVersion, libraryDependencies) { (sv, deps) =>
					deps :+ "org.scala-lang" % "scala-compiler" % sv
					deps :+ "org.scala-lang" % "scalap" % sv
				},
				//      publishTo := Some(Resolver.file("file",  new File(Path.userHome.absolutePath+"/.m2/repository"))), 
				publishTo := Option(Resolver.ssh("fwbrasil.net repo", "fwbrasil.net", 8080) as ("maven") withPermissions ("0644")),
				organization := "net.fwbrasil",
				scalaVersion := "2.10.0",
				version := "0.2",
				resolvers ++= customResolvers
			)
		)

}