# Whiteflag Java Library

![GitHub latest release](https://img.shields.io/github/v/release/whiteflagprotocol/whiteflag-javalib?label=latest&logo=github&sort=semver)
![Ubuntu Test](https://github.com/WhiteflagProtocol/whiteflag-javalib/workflows/Ubuntu%20Test/badge.svg)
![Windows Test](https://github.com/WhiteflagProtocol/whiteflag-javalib/workflows/Windows%20Test/badge.svg)
![macOS Test](https://github.com/WhiteflagProtocol/whiteflag-javalib/workflows/macOS%20Test/badge.svg)

## Introduction

Whiteflag is a fully neutral and secure communciations means based on
blockchain technology. It enables near real-time communication in armed
conflicts and disasters to exchange early warning and status information
to create shared situational awareness.

The Whiteflag Java Library (WFJL) is to be an implementation of the Whiteflag
Protocol in Java to support the development of Whiteflag-enabled applications.
The WFJL is to support the all Whiteflag protocol features, but with minimal
dependencies. Therefore, the library is independent from any specific
blockchain, database solution, user interface, etc.

The planned WFJL functionality can be found in `SCOPE.md`.

## Documentation

More detailed documentation of the library, including the API specification,
will be made available soon. Meanwhile, the API documentation may be generated
manually into the by running the following command:

`./gradlew javadoc`

which creates the HTML documentation in `./build/docs/javadoc`.

The repository structure and development guidelines for the source code are
described in `CONTRIBUTING.md`.

## License

The Whiteflag Java Library software is dedicated to the public domain
under the [Creative Commons CC0-1.0 Universal Public Domain Dedication](http://creativecommons.org/publicdomain/zero/1.0/)
statement. See `LICENSE.md` for details.

The Whiteflag Java Library software may require third party software packages,
which are not part of this distribution and may be licenced differently.

## Installation and Usage

The software is not yet published in a central repository. To use the library,
integrate the source code manually into your software by cloning
the `./src/` directory.

The only dependecy, as specified in the `./build.gradle` file, is the
JUnit test framework for testing the software.
