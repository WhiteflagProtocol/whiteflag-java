# Whiteflag Java Library Packages

As any Java software, the Whiteflag Java Library is organised in packages.
Packages help to structure the software by grouping related classes. Packages
also provide access protection and a name space. The packages structure
corresponds with the source code directory structure under `src/main/java`.

The Whiteflag Java Library consists of the following packages:

| Package                           | Purpose                           |
|-----------------------------------|-----------------------------------|
|`org.whiteflag.protocol`           | Main Whiteflag protocol interface |
|`org.whiteflag.protocol.core`      | Whiteflag core specification      |

The `org.whiteflag.protocol` package is the main package providing the external
interface for software using this library.

The `org.whiteflag.protocol.core` package contains all implementation
independent functionality as defined in the Whiteflag specification, such as
the WHiteflag message definitions.
