# General Description and Overview

## Introduction

The [Whiteflag](https://www.whiteflagprotocol.org/) Java Library (WFJL)
is an implementation of the [Whiteflag Protocol](https://standard.whiteflagprotocol.org/)
in Java to support the development of Whiteflag-enabled applications.

The WFJL is to support all Whiteflag protocol features, but with minimal
dependencies. Therefore, the library is independent from any specific
blockchain, database solution, user interface, etc.

## Documentation

### Detailed API and Source Code References

* [WFJL Javadoc API Reference](javadoc)

### Source Code Description

* [Package Structure](md/packages.md)
* [High Level Overview](md/overview.md)
* [Exception Handling](md/errors.md)
* [Whiteflag Messages](md/messages.md)
* [Cryptography](md/crypto.md)

## Installation

To use the WFJL, its [GitHub package](https://github.com/WhiteflagProtocol/whiteflag-java/packages)
should be added to your project as a dependency using the following
"group:artifact:version" information, with the most recently published version:

`org.whiteflagprotocol.java:whiteflag-java:1.1.1`

For example, when using [Gradle](https://gradle.org/), the following should be
included in your `build.gradle` file:

```groovy
dependencies {
  implementation 'org.whiteflagprotocol.java:whiteflag-java:1.1.1'
}
```

or in your `pom.xml` when using [Maven](https://maven.apache.org/):

```xml
<dependency>
  <groupId>org.whiteflagprotocol.java</groupId>
  <artifactId>whiteflag-java</artifactId>
  <version>1.1.0</version>
</dependency> 
```

Note that you also need to add the GitHub WFJL package repository URL `https://maven.pkg.github.com/whiteflagprotocol/whiteflag-java`
to your project. Please see the GitHub documentation for detailed information
about installing packages from the GitHub repository with either
[Maven](https://docs.github.com/en/packages/guides/configuring-apache-maven-for-use-with-github-packages#installing-a-package)
or [Gradle](https://docs.github.com/en/packages/guides/configuring-gradle-for-use-with-github-packages#installing-a-package).

