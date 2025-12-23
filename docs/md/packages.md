# WFJL Package Structure

| [WFJL Documentation Home](../index.md) | [WFJL Javadoc Reference](../javadoc) | [Whiteflag Specification](https://standard.whiteflagprotocol.org) |

## Overview

As any Java software, the WFJL is organised in packages.
Packages help to structure the software by grouping related classes. Packages
also provide access protection and a name space. The packages structure
corresponds with the source code directory structure under `src/main/java/`.

The WFJL software consists of the following Java packages:

| Package                           | Purpose                           |
|-----------------------------------|-----------------------------------|
|`org.whiteflagprotocol.java`       | Main Whiteflag protocol interface |
|`org.whiteflagprotocol.java.core`  | Whiteflag core functionality      |
|`org.whiteflagprotocol.java.crypto`| Whiteflag cryptographic functions |
|`org.whiteflagprotocol.java.util`  | Whiteflag utilities               |

## Description

The `org.whiteflagprotocol.java` package is the main package providing the
external programming interface for software using this library. It defines
the classes representing Whitelag messages and accounts.

The `org.whiteflagprotocol.java.core` package contains all implementation
independent core functionality as defined in the Whiteflag specification,
such as the Whiteflag message definitions. This seperates the core
protocol functionality from implementation-specific design decisions
as much as possible.

The `org.whiteflagprotocol.java.crypto` package contains all implementation
independent cryptogrpahic functionality as defined in the Whiteflag
specification, such as encryption, key derivation and negatiation, and
authentication. Seperating these critical security functions makes them better
inspectable, testable and maintainable.

The `org.whiteflagprotocol.java.util` package contains utilities used by
the main protocol classes, but that are not part of the external interface
and also not part of the protocol core, e.g. data conversions, such as
conversion of Whiteflag messages to and from JSON.
