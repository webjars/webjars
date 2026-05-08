package webjars.utils

import com.jamesward.zio_mavencentral.MavenCentral
import io.lemonlabs.uri.AbsoluteUrl

object PomTemplate:

  def apply(
    groupId: MavenCentral.GroupId,
    artifactId: MavenCentral.ArtifactId,
    releaseVersion: MavenCentral.Version,
    packageInfo: PackageInfo,
    sourceUrl: AbsoluteUrl,
    mavenDependencies: Set[(MavenCentral.GroupArtifact, String)],
    optionalMavenDependencies: Set[(MavenCentral.GroupArtifact, String)],
    licenses: Set[License]
  ): String =
    val licensesXml = licenses.map { license =>
      val nameXml = license.maybeName.map(name => s"                <name>$name</name>").getOrElse("")
      val urlXml = license.maybeUrl.map(url => s"                <url>$url</url>").getOrElse("")
      s"""            <license>
$nameXml
$urlXml
                <distribution>repo</distribution>
            </license>"""
    }.mkString("\n")

    val issuesXml = packageInfo.maybeIssuesUrl.map { issuesUrl =>
      s"""
    <issueManagement>
        <url>$issuesUrl</url>
    </issueManagement>"""
    }.getOrElse("")

    val depsXml = mavenDependencies.map { case (depGroupArtifact, version) =>
      s"""        <dependency>
            <groupId>${depGroupArtifact.groupId}</groupId>
            <artifactId>${depGroupArtifact.artifactId}</artifactId>
            <version>$version</version>
        </dependency>"""
    }.mkString("\n")

    val optionalDepsXml = optionalMavenDependencies.map { case (depGroupArtifact, version) =>
      s"""        <dependency>
            <groupId>${depGroupArtifact.groupId}</groupId>
            <artifactId>${depGroupArtifact.artifactId}</artifactId>
            <version>$version</version>
            <optional>true</optional>
        </dependency>"""
    }.mkString("\n")

    s"""<?xml version="1.0" encoding="UTF-8"?>
<project xmlns="http://maven.apache.org/POM/4.0.0" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://maven.apache.org/POM/4.0.0 http://maven.apache.org/maven-v4_0_0.xsd">
    <modelVersion>4.0.0</modelVersion>

    <packaging>jar</packaging>
    <groupId>$groupId</groupId>
    <artifactId>$artifactId</artifactId>
    <version>$releaseVersion</version>
    <name>${packageInfo.name}</name>
    <description>WebJar for ${packageInfo.name}</description>
    <url>https://www.webjars.org</url>

    <properties>
        <project.build.sourceEncoding>UTF-8</project.build.sourceEncoding>
    </properties>

    <scm>
        <url>$sourceUrl</url>
        <connection>${packageInfo.sourceConnectionUri}</connection>
        <developerConnection>${packageInfo.sourceConnectionUri}</developerConnection>
        <tag>v$releaseVersion</tag>
    </scm>

    <developers>
        <developer>
            <id>webjars</id>
            <url>https://www.webjars.org</url>
        </developer>
    </developers>

    <licenses>
$licensesXml
    </licenses>
$issuesXml
    <dependencies>
$depsXml
$optionalDepsXml
    </dependencies>

</project>"""
