# Scope for the Whiteflag Java Library

The Whiteflag Java library (WFJL) is to be an implementation of the Whiteflag
Protocol in Java to support the development of Whiteflag-enabled applications.

The WFJL is to support the main Whiteflag functionality, but with minimal
dependencies. For example, the library is independent from any specific
blockchain, database solution, user interface, etc.

## In scope

### Message handling

The WFJL should provide message handling i.a.w. `v1-draft.6` of the standard:

* initial design pattern (v0.1)
* syntax checks of all message types (v0.2)
* encoding of all message types (v0.2)
* decoding of all message types (v0.2)
* encryption/decryption (v0.4)

### Advanced Protocol Functionality

Advanced protocol features:

* Whiteflag authentication method 2 (token-based) (v0.5)
* Whiteflag authentication method 1 (signature-based):
  * create a Whiteflag digital signature (v0.6)
  * check against internet resource for `A1` messages (v0.6)
* manage encryption keys for different originators:
  * pre-shared encryption keys (v0.4)
  * ECDH negotated encryption keys (v0.4)

### Semantic message processing

Teh WFJL provides the following to support the processing of messages
in a meaningful way:

* keeping track of the Whiteflag state (originators, crypto, etc.) (v0.3)
* a model for real world entities based on and updated by Whiteflag messages (v0.7)

## Outside scope

### Protocol functionality

The following Whiteflag protocol features will not be implemented:

* hierarchical deterministic keys and addresses

### Other functionality

The following may be implemented in many ways with othre libraries and is
therefore not included in the WFJL:

* blockchain accounts and connectivity functionality
* database storage for message and state
