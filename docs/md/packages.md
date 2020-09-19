# Whiteflag Java Library Packages Structure

As any Java software, the Whiteflag Java Library is organised in packages.
Packages help to structure the software by grouping related classes. Packages
also provide access protection and a name space. The packages structure
corresponds with the source code directory structure under `src/main/java/`.

## Overview

The WFJL software consists of the following Java packages:

| Package                           | Purpose                           |
|-----------------------------------|-----------------------------------|
|`org.whiteflag.protocol`           | Main Whiteflag protocol interface |
|`org.whiteflag.protocol.core`      | Whiteflag core functionality      |

## Description

The `org.whiteflag.protocol` package is the main package providing the
external programming interface for software using this library.

The `org.whiteflag.protocol.core` package contains all implementation
independent core functionality as defined in the Whiteflag specification,
such as the Whiteflag message definitions. This seperates the core
protocol functionality from implementation-specific design decisions
as much as possible.
