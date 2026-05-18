package webjars.utils

import com.jamesward.zio_mavencentral.MavenCentral
import zio.http.URL

import scala.xml.{Elem, NodeSeq, PrettyPrinter}

object PomTemplate:

  private def licenseXml(license: License): Elem =
    val name = license.maybeName.toSeq.map(n => <name>{n}</name>)
    val url = license.maybeUrl.toSeq.map(u => <url>{u.encode}</url>)
    val distribution = Seq(<distribution>repo</distribution>)
    <license>{name ++ url ++ distribution}</license>

  private def dependencyXml(ga: MavenCentral.GroupArtifact, version: String, optional: Boolean): Elem =
    val core = Seq(
      <groupId>{ga.groupId.toString}</groupId>,
      <artifactId>{ga.artifactId.toString}</artifactId>,
      <version>{version}</version>,
    )
    val children = if optional then core :+ <optional>true</optional> else core
    <dependency>{children}</dependency>

  def apply(
    groupId: MavenCentral.GroupId,
    artifactId: MavenCentral.ArtifactId,
    releaseVersion: MavenCentral.Version,
    packageInfo: PackageInfo,
    sourceUrl: URL,
    mavenDependencies: Set[(MavenCentral.GroupArtifact, String)],
    optionalMavenDependencies: Set[(MavenCentral.GroupArtifact, String)],
    licenses: Set[License]
  ): String =
    val issueManagement: NodeSeq = packageInfo.maybeIssuesUrl match
      case Some(issuesUrl) => <issueManagement><url>{issuesUrl.encode}</url></issueManagement>
      case None            => NodeSeq.Empty

    val deps: NodeSeq =
      mavenDependencies.toSeq.map((ga, v) => dependencyXml(ga, v, optional = false)) ++
        optionalMavenDependencies.toSeq.map((ga, v) => dependencyXml(ga, v, optional = true))

    val pom =
      <project xmlns="http://maven.apache.org/POM/4.0.0" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://maven.apache.org/POM/4.0.0 http://maven.apache.org/maven-v4_0_0.xsd">
        <modelVersion>4.0.0</modelVersion>
        <packaging>jar</packaging>
        <groupId>{groupId.toString}</groupId>
        <artifactId>{artifactId.toString}</artifactId>
        <version>{releaseVersion.toString}</version>
        <name>{packageInfo.name}</name>
        <description>WebJar for {packageInfo.name}</description>
        <url>https://www.webjars.org</url>
        <properties>
          <project.build.sourceEncoding>UTF-8</project.build.sourceEncoding>
        </properties>
        <scm>
          <url>{sourceUrl.encode}</url>
          <connection>{packageInfo.sourceConnectionUri.encode}</connection>
          <developerConnection>{packageInfo.sourceConnectionUri.encode}</developerConnection>
          <tag>v{releaseVersion.toString}</tag>
        </scm>
        <developers>
          <developer>
            <id>webjars</id>
            <url>https://www.webjars.org</url>
          </developer>
        </developers>
        <licenses>{licenses.toSeq.map(licenseXml)}</licenses>
        {issueManagement}
        <dependencies>{deps}</dependencies>
      </project>

    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" +
      new PrettyPrinter(width = 120, step = 4).format(pom)
