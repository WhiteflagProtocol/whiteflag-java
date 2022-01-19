# WFJL Whiteflag Messages

Go back to the [WFJL Documentation Home](../index.md) or visit
the detailed [WFJL Javadoc API Reference](../javadoc)

## Overview

This section gives a general overview of the external programming interface
of the WFJL of the main `org.whiteflagprotocol.java` package. This package
defines:

* the `WfMessage` class, representing a Whiteflag message with its operations
  (e.g. encoding, encryption, decoding, decryption).
  
* the `WfBlockchainAccount` abstract class, representing a (virtual) blockchain
  account to or from which Whiteflag messages are sent. This is an abstract
  class, because its actual implementation depends on the blockchain.

* the `WfBlockchainAddress` interface, defining how a blockchain address must
  be represented. Adresses are blockchain specific, but Whiteflag is
  blockchain agnostic. This interface defines how classes implementing
  blockchain functionality should provide Whiteflag with a blockchain address.

### Example

A simplified example for creating a new Whiteflag message of a type specified
by a string with the message code:

```java
import org.whiteflagprotocol.java.WfMessage;
import org.whiteflagprotocol.java.WfException;

public class Example {
  /* Properties */
  private WfMessage message;

  /* Methods */
  public WfMessage decode(String hexString) throws WfException {
    message = WfMessage.decode(hexString);
    return message;
  }
}
```

### Class Diagram

The class diagram gives a rough overview of the package. It is not intended as
a complete or an accurate reference for the classes. Please see the
[WFJL Javadoc API Reference](../javadoc) for all details.

![WFJL Message Class Diagram](../uml/overview.png)
