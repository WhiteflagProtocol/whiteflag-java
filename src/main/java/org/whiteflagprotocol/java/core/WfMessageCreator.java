/*
 * Whiteflag Java Library
 */
package org.whiteflagprotocol.java.core;

import java.util.Map;

/**
 * Whiteflag message creator class
 * 
 * <p> This is a builder class to create a Whiteflag message. It contains
 * the logic based on the Whiteflag specification to create a Whiteflag message
 * object. For example, it ensures that the message body corresponds with
 * the message type.
 * 
 * @wfver v1-draft.6
 * @wfref 4. Message Format
 * 
 * @since 1.0
 */
public final class WfMessageCreator {

    /* PROPERTIES */

    /* Message parts */
    private WfMessageSegment header;
    private WfMessageSegment body;

    /* Message type */
    private WfMessageType messageType = WfMessageType.ANY;

    /* Constants */
    private static final String PREFIX = "WF";
    private static final String PROTOCOL_VERSION = "1";
    private static final String FIELD_PREFIX = "Prefix";
    private static final String FIELD_VERSION = "Version";
    private static final String FIELD_MESSAGETYPE = "MessageCode";
    private static final String FIELD_TESTMESSAGETYPE = "PseudoMessageCode";

    /* CONSTRUCTOR */

    /**
     * Constructs a Whiteflag message creator
     */
    public WfMessageCreator() {
        /* Nothing required for instantiating a Whiteflag creator object */
    }

    /* PUBLIC METHODS */

    /**
     * Creates the Whiteflag message core
     * @return a new message core
     */
    public final WfMessageCore create() {
        return new WfMessageCore(messageType, header, body);
    }

    /**
     * Creates a new empty Whiteflag core message object of the specified type
     * @param messageType the type of the new message
     * @return this message creator 
     */
    public final WfMessageCreator type(final WfMessageType messageType) {
        /* Create header and body based on message code */
        this.messageType = messageType;
        this.header = new WfMessageSegment(messageType.getHeaderFields());
        this.body = new WfMessageSegment(messageType.getBodyFields());

        /* Set version and message code field values */
        header.set(FIELD_PREFIX, PREFIX);
        header.set(FIELD_VERSION, PROTOCOL_VERSION);
        header.set(FIELD_MESSAGETYPE, messageType.getCode());

        /* Return this builder */
        return this;
    }

    /**
     * Creates a new Whiteflag core message object from header and body maps
     * @param headerValues a fieldname-to-value mapping of the message header fields
     * @param bodyValues a fieldname-to-value mapping of the message body fields
     * @return this message creator 
     * @throws WfCoreException if the fieldname-to-value mapping is incorrect
     */
    public final WfMessageCreator map(final Map<String, String> headerValues, final Map<String, String> bodyValues) throws WfCoreException {
        /* Create message header, set field values, and determine message type */
        header = new WfMessageSegment(messageType.getHeaderFields());
        if (Boolean.FALSE.equals(header.setAll(headerValues))) {
            throw new WfCoreException("Header fieldname-to-value mapping contains invalid field names and/or values: " + headerValues, null);
        }
        messageType = WfMessageType.fromCode(header.get(FIELD_MESSAGETYPE));

        /* Create message body */
        body = new WfMessageSegment(messageType.getBodyFields());
        /* Add additional fields for some message types */
        switch (messageType) {
            case T:
                /* Extend test message body with pseudo message body */
                final WfMessageType pseudoMessageType = WfMessageType.fromCode(bodyValues.get(FIELD_TESTMESSAGETYPE));
                body.append(new WfMessageSegment(pseudoMessageType.getBodyFields()));
                break;
            case Q:
                /* Extend request message body with remaining request fields (calculated with remaining bytes) */
                final int nRequestObjects = (bodyValues.size() - body.getNoFields()) / 2;   // One request object requires 2 fields
                body.append(new WfMessageSegment(messageType.createRequestFields(nRequestObjects)));
                break;
            default:
                break;
        }
        if (Boolean.FALSE.equals(body.setAll(bodyValues))) {
            throw new WfCoreException("Body fieldname-to-value mapping contains invalid field names and/or values: " + bodyValues, null);
        }
        return this;
    }

    /**
     * Deserializes a serialized Whiteflag message and creates a new Whiteflag core message object
     * @since 1.1
     * @param serializedMsg the uncompressed serialized message
     * @return this message creator
     * @throws WfCoreException if the provided values are invalid
     */
    public final WfMessageCreator deserialize(final String serializedMsg) throws WfCoreException {
        /* Keep track of fields */
        int nextField = 0;

        /* Deserialize header and determine message type */
        header = new WfMessageSegment(messageType.getHeaderFields());
        header.deserialize(serializedMsg, nextField);
        messageType = WfMessageType.fromCode(header.get(FIELD_MESSAGETYPE));

        /* Deserialize message body and add fields as required for certain message types */
        body = new WfMessageSegment(messageType.getBodyFields());
        body.deserialize(serializedMsg, nextField);
        nextField = body.getNoFields();
        switch (messageType) {
            case T:
                /* Extend test message body with pseudo message body */
                final WfMessageType pseudoMessageType = WfMessageType.fromCode(body.get(FIELD_TESTMESSAGETYPE));
                body.append(new WfMessageSegment(pseudoMessageType.getBodyFields()));
                break;
            case Q:
                /* Extend request message body with remaining request fields (calculated with remaining bytes) */
                final int lastFieldByte = body.getField(-1).endByte;
                final int nRequestObjects = (serializedMsg.length() - lastFieldByte) / 4;   // One request object requires 2 fields of 2 bytes
                body.append(new WfMessageSegment(messageType.createRequestFields(nRequestObjects)));
                break;
            default:
                break;
        }
        body.deserialize(serializedMsg, nextField);
        return this;
    }

    /**
     * Decodes an encoded Whiteflag message and creates a new Whiteflag core message object
     * @since 1.1
     * @param msgBuffer a buffer with the compressed binary encoded message
     * @return this message creator 
     * @throws WfCoreException if the encoded message is invalid
     */
    public final WfMessageCreator decode(final WfBinaryBuffer msgBuffer) throws WfCoreException {
        /* Keep track of fields and bit position */
        int bitCursor = 0;
        int nextField = 0;

        /* Decode message header, and determine message type */
        header = new WfMessageSegment(messageType.getHeaderFields());
        header.decode(msgBuffer, bitCursor, nextField);
        bitCursor += header.bitLength();
        messageType = WfMessageType.fromCode(header.get(FIELD_MESSAGETYPE));

        /* Decode message body and add fields as required for certain message types */
        body = new WfMessageSegment(messageType.getBodyFields());
        body.decode(msgBuffer, bitCursor, nextField);
        nextField = body.getNoFields();
        bitCursor += body.bitLength();
        switch (messageType) {
            case T:
                /* Determine pseudo message type and extend test message body with pseudo message body */
                final WfMessageType pseudoMessageType = WfMessageType.fromCode(body.get(FIELD_TESTMESSAGETYPE));
                body.append(new WfMessageSegment(pseudoMessageType.getBodyFields()));
                break;
            case Q:
                /* Extend request message body with request fields (calculated from remaining bits) */
                final int nRequestObjects = (msgBuffer.bitLength() - bitCursor) / 16;   // One request object requires 2 fields of 8 bits
                body.append(new WfMessageSegment(messageType.createRequestFields(nRequestObjects)));
                break;
            default:
                break;
        }
        body.decode(msgBuffer, bitCursor, nextField); 
        return this;
    }

    /**
     * Decodes an encoded Whiteflag message and creates a new Whiteflag core message object
     * @since 1.1
     * @param msgBuffer a buffer with the compressed binary encoded message
     * @return the unencrypted message header
     * @throws WfCoreException if the encoded message is invalid
     */
    public final WfMessageSegment getUnencryptedHeader(final WfBinaryBuffer msgBuffer) throws WfCoreException {
        /* Create temporary message header */
        header = new WfMessageSegment(WfMessageType.ANY.getUnencryptedHeaderFields());
        header.decode(msgBuffer, 0, 0);
        return header;
    }

    /**
     * Compiles a new Whiteflag core message object from field values
     * @param fieldValues String array with the values for the message fields
     * @return this message creator 
     * @throws WfCoreException if the provided values are invalid
     */
    public final WfMessageCreator compile(final String[] fieldValues) throws WfCoreException {
        /* Create message header, set field values and determine message type */
        header = new WfMessageSegment(messageType.getHeaderFields());
        header.setAll(fieldValues, 0);
        messageType = WfMessageType.fromCode(header.get(FIELD_MESSAGETYPE));

        /* Create message body based on message type */
        int bodyStartIndex = header.getNoFields();
        body = new WfMessageSegment(messageType.getBodyFields());

        /* Add additional fields to message body for some message types */
        switch (messageType) {
            case T:
                /* Determine pseudo message type and extend test message body with pseudo message body */
                final WfMessageType pseudoMessageType = WfMessageType.fromCode(fieldValues[bodyStartIndex]);
                body.append(new WfMessageSegment(pseudoMessageType.getBodyFields()));
                break;
            case Q:
                /* Extend request message body with request fields (calculated with remaining fields) */
                final int nRequestObjects = (fieldValues.length - (header.getNoFields() + body.getNoFields())) / 2;  // One request object requires 2 fields
                body.append(new WfMessageSegment(messageType.createRequestFields(nRequestObjects)));
                break;
            default:
                break;
        }
        body.setAll(fieldValues, bodyStartIndex);
        return this;
    }
}
