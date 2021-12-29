# Contributing to the Whiteflag Java Library

The Whiteflag Java Library (WFJL) is a [Java](https://www.java.com/)
implementation of the Whiteflag Protocol to support the development
of Whiteflag-enabled applications.

## Issues and Requests

Please report bugs and file requests by creating an issue in the
[GitHub repository](https://github.com/WhiteflagProtocol/whiteflag-java/issues).

## Repository and Code Structure

The [Gradle](https://gradle.org/) build tool is used to structure, test
and build the WFJL software.

As any Java software, the WFJL is organised in packages. The repository
adheres to the standard Maven project structure with `src/main/java/org/whiteflagprotocol/java`
as the path to the main WFJL package. Please see `docs/md/packages.md`
for a description of the WFJL packages.

## Building and Testing

A number of automated tests is implemented with the [JUnit](https://junit.org/)
test framework. To do a full test use the following command
in the project root:

```shell
gradlew test
```

To test and build the software, use:

```shell
gradlew build
```

The compiled software, test reports and generated documentation are written
to the `build/` directory.

## Documentation

Detailed documentation of the WFJL programming interface is available at
[java.whiteflagprotocol.org](https://java.whiteflagprotocol.org/). The source of this
documentation is found in the `docs/` directory.

The API documentation is generated manually from the
[javadoc](https://www.oracle.com/java/technologies/javase/javadoc-tool.html)
comments in the code by running the following command in the project root:

```shell
gradlew docs
```

which creates the HTML documentation in `build/docs/javadoc/`
and copies it to `docs/javadoc/`.

Additional descriptions and background information is written in [Markdown](https://en.wikipedia.org/wiki/Markdown)
files, which can be found in `docs/md/`.

## Versioning

[Semantic versioning](https://semver.org/) is used for this project.
For available versions, see the [version tags](https://github.com/WhiteflagProtocol/whiteflag-java/tags)
on this repository.

Versions in development  use `-dev` as pre-release identifier,
e.g. `1.2.4-dev` indicates that this is a "work in progress" snapshot from
a development branch in between versions 1.2.3 and 1.2.4. Multiple pre-release
identifiers may be used way, e.g. `1.0.0-alpha.3-dev`.

## Git Branches

There are two main branches with infinite lifetime:

* `master` contains the released versions which are pulled from `develop`;
  all releases are tags on the master branch.
* `develop` is the branch in which all development work is brought together
  and merged for integration and testing of new versions, which are pulled
  into master for a new major or minor release (`x.y.0`)
  or new pre-release (`1.0.0-alpha.n`) upon completion.

In addition, a number of support branches with the following
naming conventions may be used:

* `hotfix-<version>` is a branch from `master` in which urgent bugs are fixed
  and then pulled into `master` for a bugfix release (with the `<version>`
  being `1.0.z` for example); a hotfix should also be merged into `develop`.
* `release-<version>` is a branch from `develop` used, as required, for
  integration and testing of a specific major or minor release (with the
  `<version>` being `x.y.0`); upon completion the release is pulled into
  `master` and should also be merged into `develop`.
* `dev/<feature>` is a branch from `develop` in which a specific feature is
  developed; such a branch may exist for a limited period of time for a very
  specific feature, or longer for larger work over multiple major and minor
  versions (e.g. `dev/crypto`, `dev/blockchain`, `dev/state`); a development
  branch may only be merged into `develop`.

## Coding Style

### Main Style Guide

The WFJL project intends to use the [Google Java Style Guide](https://google.github.io/styleguide/javaguide.html)
with the exceptions and additional coding guidance below.

All major code elements (classes and methods) should be documented using
[javadoc](https://www.oracle.com/java/technologies/javase/javadoc-tool.html).
A comment starting with a `/**` sequence is a javadoc comment. Non-JSDoc
comments must start with `/*`, except for end-line or temporary comments.

### Style Guide Exceptions

The project style has the following deviations from the
Google Java Style Guide:

* indentation of 4 spaces (no tabs!)
* lines may be 128 characters long

### Class organisation

Classes should be organised as follows:

1. Constant Properties
2. Variable Properties
3. Constructors
4. Static Factory Methods
5. Public Methods
6. Public Static Methods
7. Protected Methods
8. Protected Static Methods
9. Private Methods
10. Nested Classes

### Additional Coding Guidance

1. Look at the existing code as a guidance and example!
2. It is better to use multiple lines if a line is longer than 128 characters:
    * except for strings: do not break strings
    * put logical and concatination operators at the beginning of a new line
3. Comment your code, but avoid commenting the obvious:
    * modules, classes, and functions must be described using `javadoc`

Further guidance will follow.
