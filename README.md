# Whiteflag Java Library

![GitHub latest release](https://img.shields.io/github/v/release/whiteflagprotocol/whiteflag-javalib?label=latest&logo=github&sort=semver)
![Ubuntu Test](https://github.com/WhiteflagProtocol/whiteflag-javalib/workflows/Ubuntu%20Test/badge.svg)
![Windows Test](https://github.com/WhiteflagProtocol/whiteflag-javalib/workflows/Windows%20Test/badge.svg)
![macOS Test](https://github.com/WhiteflagProtocol/whiteflag-javalib/workflows/macOS%20Test/badge.svg)

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

The planned WFJL functionality can be found in `SCOPE.md`.

## Documentation

More detailed documentation of the WFJL programming interface is available at
[Github Pages](https://whiteflagprotocol.github.io/whiteflag-javalib/)

This documentation is also found in the under the `docs/` directory.
The API documentation may also be generated manually by running
the following command in the project root:

```shell
gradlew javadoc
```

which creates the HTML documentation in `build/docs/javadoc/`.

The repository structure and development guidelines for the source code are
described in `CONTRIBUTING.md`.

## License

The WFJL software is dedicated to the public domain under the
[Creative Commons CC0-1.0 Universal Public Domain Dedication](http://creativecommons.org/publicdomain/zero/1.0/)
statement. See `LICENSE.md` for details.

The library may require third party software packages, which are not
part of this distribution and may be licenced differently.

## Installation and Usage

The WFJL software is not yet published in a central Java repository. To use
the library, integrate the source code manually into your software by cloning
the `src/` directory.

The dependencies, as specified in the `build.gradle` file, are:

* the [JUnit test framework]((https://junit.org/)) for testing the software
* the [Jackson JSON library](https://github.com/FasterXML/jackson) for reading and creating [JSON](https://en.wikipedia.org/wiki/JSON) formatted Whiteflag messages

## Building and Testing

The [Gradle](https://gradle.org/) build tool is used to structure, test
and build the WFJL software.

A number of automated tests is implemented with the [JUnit](https://junit.org/)
test framework. To do a full test and run all the tests, use the following
command in the project root:

```shell
gradlew test
```
