name := "BASIC GUI"

version := "0.0.1"

scalaVersion := "2.9.2"

unmanagedJars in Compile += Attributed.blank(file(System.getenv("JAVA_HOME") + "/jre/lib/jfxrt.jar"))

