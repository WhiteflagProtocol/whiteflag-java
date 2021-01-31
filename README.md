# Whiteflag Java Library

![GitHub latest release](https://img.shields.io/github/v/release/whiteflagprotocol/whiteflag-java?label=latest&logo=github&sort=semver)
![Ubuntu Test](https://github.com/WhiteflagProtocol/whiteflag-java/workflows/Ubuntu%20Test/badge.svg)
![Windows Test](https://github.com/WhiteflagProtocol/whiteflag-java/workflows/Windows%20Test/badge.svg)
![macOS Test](https://github.com/WhiteflagProtocol/whiteflag-java/workflows/macOS%20Test/badge.svg)

## Introduction

Whiteflag is a fully neutral and secure communciations protocol based on
blockchain technology. It enables near real-time communication in armed
conflicts and disasters to exchange early warning and status information
to create shared situational awareness.

The [Whiteflag Protocol specification](https://github.com/WhiteflagProtocol/whiteflag-standard)
is an open standard.

The Whiteflag Java Library (WFJL) is an implementation of the Whiteflag
Protocol in [Java](https://www.java.com/) to support the development of
Whiteflag-enabled applications. The WFJL is to include all Whiteflag
protocol features, but with minimal dependencies. Therefore, the library
is independent from any specific blockchain, database solution,
user interface, etc.

Version 1 of the WFJL corresponds with version 1 of the protocol.
The planned WFJL functionality for versions `1.x` can be found in `SCOPE.md`.

## Installation and Usage

To use the WFJL, its [GitHub package](https://github.com/WhiteflagProtocol/whiteflag-java/packages)
should be added to your project as a dependency using the following
"group:artifact:version" information:

`org.whiteflagprotocol.java:whiteflag-java:0.3.0-dev`

For example, when using Gradle, the following should be included in your
`build.gradle` file:

```groovy
plugins {
  id 'maven'
}
repositories {
  maven {
    url = uri("https://maven.pkg.github.com/whiteflagprotocol/whiteflag-java")
    credentials {
      username = project.findProperty("gpr.user") ?: System.getenv("GPR_USER")
      password = project.findProperty("gpr.key") ?: System.getenv("GPR_TOKEN")
    }
  }
}
dependencies {
  implementation 'org.whiteflagprotocol.java:whiteflag-java:0.3.0-dev'
}
```

Please see the GitHub documentation for detailed information about
installing packages with either
[Maven](https://docs.github.com/en/packages/guides/configuring-apache-maven-for-use-with-github-packages#installing-a-package)
or [Gradle](https://docs.github.com/en/packages/guides/configuring-gradle-for-use-with-github-packages#installing-a-package).

After adding the dependency to your project, the WFJL classes can be used
in your Java code. Normally, you will only need the classes from the
`org.whiteflagprotocol.java` package. Your code may look like this:

```java
import org.whiteflagprotocol.java.WfMessage;
import org.whiteflagprotocol.java.WfException;

public class Example {
  private WfMessage message;

  public WfMessage createMessage(String messageType) {
    try {
      message = WfMessage.Creator.create(messageType);
    } catch(WfException e) {
      throw new IllegalArgumentException("Cannot create a Whiteflag message of type " + messageType);
    }
    return message;
  }
}
```

## Documentation

All detailed documentation of the WFJL programming interface is available at
[Github Pages](https://java.whiteflagprotocol.org/). The documentation is also
found in this repository in the `docs/` directory.

The repository structure and development guidelines for the source code are
described in `CONTRIBUTING.md`.

## License

The WFJL software is dedicated to the public domain under the
[Creative Commons CC0-1.0 Universal Public Domain Dedication](http://creativecommons.org/publicdomain/zero/1.0/)
statement. See `LICENSE.md` for details.

The library may require third party software packages, which are not
part of this distribution and may be licenced differently.

## Building and Testing

The dependencies of the WFJL itself, specified in the `build.gradle` file, are:

* the [JUnit test framework](https://junit.org/) for testing the software
* the [Jackson JSON library](https://github.com/FasterXML/jackson) for reading and creating [JSON](https://en.wikipedia.org/wiki/JSON) formatted Whiteflag messages

The [Gradle](https://gradle.org/) build tool is used to structure, test
and build the WFJL software.

A number of automated tests is implemented with the [JUnit](https://junit.org/)
test framework. To do a full test and run all the tests, use the following
command in the project root:

```shell
gradlew test
```
