# Whiteflag Java Library

## Whiteflag messages

This section describes how Whiteflag message are implemented by the WFJL.

### Message classes

The WFJL implements the Whiteflag messages defined in the Whiteflag standard
with the `WfMessage` class of the `org.whiteflag.protocol` package. This class
extends the `WfMessageCore` class from the `org.whiteflag.protocol.core`
package with additional implementation-sepcific metadata and methods that allow
for further integration in larger software applications.

The full set of classes used to build and process Whiteflag messages is shown
in the class diagram below.

### Message core, segment and field classes

The `WfMessageCore` class only contains the core elements as specified in the
Whiteflag standard, i.e. the generic message header and the message type
specific body. Both are implemented as a `WfMessageSegment` class, which
comprises a number of message fields implemented as `WfMessageField` class.

### Message creator and definition classes

To create Whiteflag messages, creator classes are used: the
`WfMessageCreator` class creates `WfMessageCore` objects and is instantiated by
the nested `WfMessage.Creator` class to created extended `WfMessage` objects.

The `WfMessageDefinitions` class used by the `WfMessageCreator` class contains
all message types and field definitions i.a.w. the Whiteflag specification.

### Message Class Diagram

![WFJL Message Class Diagram](../uml/messages.png)
