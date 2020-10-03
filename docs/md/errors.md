# Whiteflag Java Library

## Error Handling

In addition to the standard Java exception classes, the Whiteflag Java Library
extends the Java exception class for specific Whiteflag protocol errors.

### Whiteflag Exceptions

The core package defines the `WfCoreException` class, that is thrown by its
classes if any request is made that is not in accordance with the Whiteflag
specification, e.g. when a field value is not allowed or is not correctly
formatted for a specfic field.

The main package `org.whiteflag.protocol` that provides the external interface,
uses the `WfExcepetion` class that extends the `WfCoreException` class. The
`WfException` class has a property indicating the type of error. The error
types are defined by the nested enumeration `WfException.ErrorType`.

#### Error types

The `WfException.ErrorType` enum defines the following error codes:

* `WF_PROTOCOL_ERROR`: generic Whiteflag protocol error
* `WF_METADATA_ERROR`: incorrect or missing Whiteflag message metadata
* `WF_FORMAT_ERROR`: Whiteflag message format error
* `WF_REFERENCE_ERROR`: Whiteflag message reference error
* `WF_AUTH_ERROR`: Whiteflag message authentication error
* `WF_SIGN_ERROR`: Whiteflag signature error
* `WF_ENCRYPTION_ERROR`: Whiteflag encryption error

### Usage of Whiteflag Exceptions

The `WfCoreException` class is only used internally in the library. Software
using the library should only expect the `WfException` class to be thrown, and
should only use that class themselves. For example:

```java
throw new WfException("The message originator could not be authenticated", WfException.ErrorType.WF_AUTH_ERROR);
```

### Error Class Diagram

![WFJL Error Class Diagram](../uml/errors.png)
