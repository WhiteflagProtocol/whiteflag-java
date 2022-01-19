# Whiteflag Java Library

![GitHub latest release](https://img.shields.io/github/v/release/whiteflagprotocol/whiteflag-java?label=latest&logo=github&sort=semver)
![Ubuntu Test](https://github.com/WhiteflagProtocol/whiteflag-java/workflows/Ubuntu%20Test/badge.svg)
![Windows Test](https://github.com/WhiteflagProtocol/whiteflag-java/workflows/Windows%20Test/badge.svg)
![macOS Test](https://github.com/WhiteflagProtocol/whiteflag-java/workflows/macOS%20Test/badge.svg)

## Introduction

[Whiteflag](https://www.whiteflagprotocol.org) is a fully neutral and
secure communciations protocol based on blockchain technology. It enables
near real-time communication in armed conflicts and disasters to exchange
early warning and status information to create shared situational awareness.

The [Whiteflag Protocol specification](https://standard.whiteflagprotocol.org)
is an open standard.

The Whiteflag Java Library (WFJL) is an implementation of the Whiteflag
Protocol in [Java](https://www.java.com/) to support the development of
Whiteflag-enabled applications. The WFJL is to include all Whiteflag protocol
features, but with minimal dependencies. Therefore, the library is independent
from any specific blockchain, database solution, user interface, etc.

Version 1 of the WFJL corresponds with version 1 of the protocol, and more
specifically with `v1-draft.6` of the standard. The planned WFJL functionality
and corresponding `1.x` subversions are defined as [milestones](https://github.com/WhiteflagProtocol/whiteflag-java/milestones).

## Installation and Usage

### Defining package dependency

To use the WFJL, its [GitHub package](https://github.com/WhiteflagProtocol/whiteflag-java/packages)
should be added to your project as a dependency using the following
"group:artifact:version" information, with the most recently published version:

`org.whiteflagprotocol.java:whiteflag-java:1.0.0`

For example, when using [Gradle](https://gradle.org/), the following should be
included in your `build.gradle` file:

```groovy
dependencies {
  implementation 'org.whiteflagprotocol.java:whiteflag-java:1.0.0'
}
```

or in your `pom.xml` when using [Maven](https://maven.apache.org/):

```xml
<dependency>
  <groupId>org.whiteflagprotocol.java</groupId>
  <artifactId>whiteflag-java</artifactId>
  <version>1.0.0</version>
</dependency> 
```

Note that you also need to add the GitHub WFJL package repository URL `https://maven.pkg.github.com/whiteflagprotocol/whiteflag-java`
to your project. Please see the GitHub documentation for detailed information
about installing packages from the GitHub repository with either
[Maven](https://docs.github.com/en/packages/guides/configuring-apache-maven-for-use-with-github-packages#installing-a-package)
or [Gradle](https://docs.github.com/en/packages/guides/configuring-gradle-for-use-with-github-packages#installing-a-package).

### Example

After adding the dependency to your project, the WFJL classes can be used
in your Java code. Normally, you will only need the classes from the
`org.whiteflagprotocol.java` package. Your code may look like this:

```java
import org.whiteflagprotocol.java.WfMessage;
import org.whiteflagprotocol.java.WfException;

public class Example {
  /* Properties */
  private WfMessage message;

  /* Methods */
  public WfMessage createMessage(String messageType) throws WfException {
    message = WfMessage.type(messageType);
    return message;
  }
}
```

## Documentation

All detailed documentation of the WFJL programming interface is available at
[java.whiteflagprotocol.org](https://java.whiteflagprotocol.org/). The
documentation is also found in this repository in the `docs/` directory.

The repository structure and development guidelines for the source code are
described in `CONTRIBUTING.md`.

## License and Third Party Software

The WFJL software is dedicated to the public domain under the
[Creative Commons CC0-1.0 Universal Public Domain Dedication](http://creativecommons.org/publicdomain/zero/1.0/)
statement. See `LICENSE.md` for details.

The library may require third party software packages, which are not part of
this distribution and may be licenced differently. The third party software
dependencies of the WFJL itself are:

* the [JUnit test framework](https://junit.org/) for testing the software
* the [Bouncycastle Crypto API](https://bouncycastle.org/) used as the cryptographic provider for the Whiteflag Elliptic Curve Diffie-Hellman implementation
* the [Jackson JSON library](https://github.com/FasterXML/jackson) for reading and creating [JSON](https://en.wikipedia.org/wiki/JSON) formatted Whiteflag messages
