# WFJL Error Handling

Go back to the [WFJL Documentation Home](../index.md) or visit
the detailed [WFJL Javadoc API Reference](../javadoc)

## Overview

In addition to the standard Java exception classes, the WFJL extends the
[Java Exception class](https://docs.oracle.com/javase/8/docs/api/java/lang/Exception.html)
for Whiteflag specific errors:

* `WfException`: used by the `org.whiteflagprotocol.java` to indicate errors to other software components while processing WHiteflag data
* `WfCoreException`: used internally by the `org.whiteflagprotocol.java.core` package for incompatibilities with the Whiteflag specification
* `WfCryptoException`: used internally by the `org.whiteflagprotocol.java.crypto` package to wrap cryptographic errors
* `WfUtilException`: used internally by the `org.whiteflagprotocol.java.util` package to wrap utility errors

Software using the library should typcially only expect the `WfException` class
to be thrown. The other error classes are primarily used internally.

### Error Class Diagram

The class diagram gives a rough overview of the error classes. It is not
intended as a complete or accurate reference Instead, please see the
[WFJL Javadoc API Reference](../javadoc) for all details.

![WFJL Error Class Diagram](../uml/errors.png)

## Detailed Implementation

### Error Classes

The core package defines the `WfCoreException` class, that is thrown by its
classes if any request is made that is not in accordance with the Whiteflag
specification, e.g. when a field value is not allowed or is not correctly
formatted for a specfic field.

The main package `org.whiteflagprotocol.java` that provides the external
interface, uses the `WfExcepetion` class. The `WfException` class has a
property indicating the type of error. The error types are defined by the
nested enumeration `WfException.ErrorType`.

### Error Types

The `WfException.ErrorType` enum defines the following error codes:

* `WF_GENERIC_ERROR`: generic Whiteflag error
* `WF_METADATA_ERROR`: incorrect or missing Whiteflag message metadata
* `WF_FORMAT_ERROR`: Whiteflag message format error
* `WF_REFERENCE_ERROR`: Whiteflag message reference error
* `WF_AUTH_ERROR`: Whiteflag message authentication error
* `WF_SIGN_ERROR`: Whiteflag signature error
* `WF_CRYPTO_ERROR`: Whiteflag cryptographic error

## Usage

### Example

An example where a Whiteflag exception is thrown during authentication
of an originator:

```java
throw new WfException("Invalid authentication data", null, WfException.ErrorType.WF_AUTH_ERROR);
```

Note the second argument, which is a `Throwable` indicating the underlying
cause. It may be `null` if unknown or not existent.
