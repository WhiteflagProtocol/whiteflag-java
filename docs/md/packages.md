# WFJL Package Structure

Go back to the [WFJL Documentation Home](../index.md) or visit
the detailed [WFJL Javadoc API Reference](../javadoc)

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
|`org.whiteflagprotocol.java.util`  | Whiteflag utilities               |

## Description

The `org.whiteflagprotocol.java` package is the main package providing the
external programming interface for software using this library. Normally,
this is the only package required to import in your code.

The `org.whiteflagprotocol.java.core` package contains all implementation
independent core functionality as defined in the Whiteflag specification,
such as the Whiteflag message definitions. This seperates the core
protocol functionality from implementation-specific design decisions
as much as possible.

The `org.whiteflagprotocol.java.util` package contains utilities used by
the main protocol classes, but that are not part of the external interface
and also not part of the protocol core, e.g. data conversions, such as
conversion of Whiteflag message to and from JSON.
