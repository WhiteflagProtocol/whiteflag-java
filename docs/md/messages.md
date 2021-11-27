# WFJL Whiteflag Messages

Go back to the [WFJL Documentation Home](../index.md) or visit
the detailed [WFJL Javadoc API Reference](../javadoc)

## Overview

This section describes how Whiteflag messages are implemented internally
by the WFJL.

To use Whiteflag messages in software applications, only the
`org.whiteflagprotocol.java` package with the `WfMessage` class and its nested
creator class `WfMessage.Creator` are required.

A simple example for creating a new Whiteflag message of a type specified by
a string with the message code:

```java
import org.whiteflagprotocol.java.WfMessage;
import org.whiteflagprotocol.java.WfException;

public class Example {
  /* Properties */
  private WfMessage message;

  /* Methods */
  public WfMessage createMessage(String messageType) throws WfException {
    message = WfMessage.type(messageType);
    return message;
  }
}
```

## Detailed Implementation

### Message Class Diagram

![WFJL Message Class Diagram](../uml/messages.png)

### Message Classes

The WFJL implements the Whiteflag messages defined in the Whiteflag standard
with the `WfMessage` class of the `org.whiteflagprotocol.java` package. This class
extends the `WfMessageCore` class from the `org.whiteflagprotocol.java.core`
package with additional implementation-sepcific metadata and methods that allow
for further integration in larger software applications.

### Message Core, Segment and Field classes

The `WfMessageCore` class only contains the core elements as specified in the
Whiteflag standard, i.e. the generic message header and the message type
specific body. Both are implemented as a `WfMessageSegment` class, which
comprises a number of message fields implemented as `WfMessageField` class.

### Message Creator, Type and Definition classes

To create Whiteflag messages, the core package provides the `WfMessageCreator`
class to instantiate `WfMessageCore` objects. The `WfMessageCreator` class is
used by the static factory methods of the `WfMessage` class, which are the
external interface to other software to create Whiteflag messages.

The `WfMessageType` class contains all message types i.a.w. the Whiteflag
specification, and is used by the `WfMessageCreator` class to create the
messages. The `WfMessageDefintions` is a utility class with all field
definitions.

## Usage

### Message Creation and Alteration

The `WfMessage` class represents a Whiteflag message. The class cannot be
instantiated directly. Instead, one of its static factory methods must be used
to create a message. The available static factory methods to do this are:

* `WfMessage.type(String messageCode)`: creates a new Whiteflag message of the type specified by the message code with empty field values
* `WfMessage.copy(WfMessage)`: copies an existing Whiteflag message, without the metadata
* `WfMessage.clone(WfMessage)`: clones an existing Whiteflag message, including the metadata
* `WfMessage.deserialize(String)`: deserializes a string with a serialized message
* `WfMessage.deserializeJson(String)`: deserializes a string with a JSON representation of a message
* `WfMessage.decode(String)`: decodes a string with the hexadecimal representation of an encoded message
* `WfMessage.compile(String[])`: compiles a Whiteflag message from an array with a complete and ordered set of field values

Each of these methods returns a new `WfMessage` object. This object contains
the message header and body as `WfMessageSegment` objects that contain the
`WfMessageField` objects.

### Accessing Message Fields

Message fields may be accessed through the message header or body segments,
which provide getters and setter. The `WfMessage` class also provides an
interface to get or set a field directly. Thus, the following ways both work
to get a value from a header field:

* `WfMessage.get(String fieldname)`: returns the value of the specified message field
* `WfMessage.header.get(String fieldname)`: returns the value of the specified message body field

Or to set a field value in the message body:

* `WfMessage.set(String fieldname, String value)`: sets the value of the specified message field
* `WfMessage.body.set(String fieldname, String value)`: sets the value of the specified message body field

To ensure data integrity, a field value cannot change once set. The `set`
methods return `true` if the field value is set. If the return value is
`false`, then the field value could not be set, either because the provided
value is invalid, or the field had already been set.

See below for a number of methods to check message and field validity. Trying
to serialize or encode an invalid message will result in a `WfException`
of type `WF_FORMAT_ERROR` to be thrown.

To set all fields values in a segment, use the `setAll` method:

* `WfMessage.header.setAll(String[], int index)`: sets all fields from an array with field values
* `WfMessage.header.setAll(Map<String, String>)`: sets fields from a fieldname-to-value mapping

These methods return `true` if all fields in the segment are correctly set,
and `false` if one or more fields are not correctly set.

### Field Data and Validators

To support of message creation by application software, the following methods
are available for both message segments and messages:

* `WfMessage.isValid()`: returns a Boolean to indicate if the message is valid, i.e. if all message fields contain valid data
* `WfMessage.isValid(String fieldname)`: returns a Boolean to indicate if specified field contains valid data
* `WfMessage.isValid(String fieldname, String data)`: returns a Boolean to indicate if specified data would be valid for the specified field
* `WfMessage.getNoFields()`: returns an interger with the number of fields
* `WfMessage.getFieldNames()`: returns a Set with all field names

The following will work as well: `WfMessage.header.isValid()`, and
`WfMessage.body.getFieldNames()`, etc. etc.

### Accessing Message Msetadata

The Whiteflag message object also holds the metadata associated with the
message. The metatdata may be accessed with the following methods:

* `WfMessage.addMetadata(String key, String value)`: sets the value if the key does not yet exist, otherwise it returns the existing value
* `WfMessage.getMetadata(String key)`: returns a String with the value of the provided key
* `WfMessage.getMetadataKeys()`: returns a Set of Strings with all existing metadata keys

As described, a metadata field cannot be changed once added.
