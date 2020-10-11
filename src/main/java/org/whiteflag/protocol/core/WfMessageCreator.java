/*
 * Whiteflag Java Library
 */
package org.whiteflag.protocol.core;

import java.util.Arrays;
import java.util.Map;

/**
 * Whiteflag message builder class
 * 
 * </p> This is a builder class to create a Whiteflag message. It contains
 * the logic based on the Whiteflag specification to create a Whiteflag message
 * object. For example, it ensures that the message body corresponds with
 * the message type.
 */
public class WfMessageCreator {

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
     * Creates a Whiteflag message builder
     */
    public WfMessageCreator() {
        // Nothing required for instantiating a Whiteflag creator object
    }

    /* PUBLIC METHODS: operations */

    /**
     * Creates the Whiteflag core message
     * @return a new {@link WfMessageCore}
     * @throws WfCoreException if the message cannot be created
     */
    public final WfMessageCore create() {
        return new WfMessageCore(messageType, header, body);
    }

    /**
     * Creates a new empty Whiteflag core message object of the specified type
     * @param messageType the {@link WfMessageType} of the new message
     * @return this {@link WfMessageCreator}
     * @throws WfCoreException if the provided values are invalid
     */
    public final WfMessageCreator type(final WfMessageType messageType) {
        // Create header and body based on message code
        this.messageType = messageType;
        this.header = new WfMessageSegment(messageType.getHeaderFields());
        this.body = new WfMessageSegment(messageType.getBodyFields());

        // Set version and message code field values
        header.setFieldValue(FIELD_PREFIX, PREFIX);
        header.setFieldValue(FIELD_VERSION, PROTOCOL_VERSION);
        header.setFieldValue(FIELD_MESSAGETYPE, messageType.getMessageCode());

        // Return new message
        return this;
    }

    /**
     * Creates a new Whiteflag core message object from header and body maps
     * @param headerMap a name-to-value mapping of the message header fields
     * @param bodyMap a name-to-value mapping of the message body fields
     * @return this {@link WfMessageCreator}
     * @throws WfCoreException if the provided fields and/or values are invalid
     */
    public final WfMessageCreator map(final Map<String, String> headerMap, final Map<String, String> bodyMap) throws WfCoreException {
        // Create message header, set field values, and determine message type
        header = new WfMessageSegment(messageType.getHeaderFields());
        if (Boolean.FALSE.equals(header.fromMap(headerMap))) {
            throw new WfCoreException("Header fields name-to-value mapping contains invalid field names and/or values");
        }
        messageType = WfMessageType.getType(header.getFieldValue(FIELD_MESSAGETYPE));

        // Create message body
        body = new WfMessageSegment(messageType.getHeaderFields());
        // Add additional fields for some message types
        switch (messageType) {
            case T:
                // Extend test message body with pseudo message body
                final WfMessageType pseudoMessageType = WfMessageType.getType(bodyMap.get(FIELD_TESTMESSAGETYPE));
                body.append(new WfMessageSegment(pseudoMessageType.getBodyFields()));
                break;
            case Q:
                // Extend request message body with remaining request fields (calculated with remaining bytes)
                final int nRequestObjects = (bodyMap.size() - body.getNoFields()) / 2;   // One request object requires 2 fields
                body.append(new WfMessageSegment(messageType.createRequestFields(nRequestObjects)));
                break;
            default:
                break;
        }
        if (Boolean.FALSE.equals(body.fromMap(bodyMap))) {
            throw new WfCoreException("Body fields name-to-value mapping contains invalid field names and/or values");
        }
        return this;
    }

    /**
     * Deserializes a serialized Whiteflag message and creates a new Whiteflag core message object
     * @param messageStr String with the uncompressed serialized message
     * @return this {@link WfMessageCreator}
     * @throws WfCoreException if the provided values are invalid
     */
    public final WfMessageCreator deserialize(final String messageStr) throws WfCoreException {
        // Cursor pointing to next field in the serialized message
        int byteCursor = 0;

        // Create and deserialize message header, and determine message type
        header = new WfMessageSegment(messageType.getHeaderFields());
        byteCursor = header.deserialize(messageStr, byteCursor);
        messageType = WfMessageType.getType(header.getFieldValue(FIELD_MESSAGETYPE));

        // Create and deserialize message body
        body = new WfMessageSegment(messageType.getBodyFields());
        byteCursor = body.deserialize(messageStr, byteCursor);

        // Add and deserialize additional fields for some message types
        switch (messageType) {
            case T:
                // Extend test message body with pseudo message body
                final WfMessageType pseudoMessageType = WfMessageType.getType(body.getFieldValue(FIELD_TESTMESSAGETYPE));
                body.append(new WfMessageSegment(pseudoMessageType.getBodyFields()));
                break;
            case Q:
                // Extend request message body with remaining request fields (calculated with remaining bytes)
                final int nRequestObjects = (messageStr.length() - byteCursor) / 4;   // One request object requires 2 fields of 2 bytes
                body.append(new WfMessageSegment(messageType.createRequestFields(nRequestObjects)));
                break;
            default:
                break;
        }
        body.deserialize(messageStr, byteCursor);
        return this;
    }

    /**
     * Decodes an encoded Whiteflag message and creates a new Whiteflag core message object
     * @param messageStr String with the hexadecimal representation of the encoded message
     * @return this {@link WfMessageCreator}
     * @throws WfCoreException if the encoded message is invalid
     */
    public final WfMessageCreator decode(final String messageStr) throws WfCoreException {
        // Cursor pointing to next field in the encoded message
        int bitCursor = 0;

        // Convert hexadecimal string representation into binary string
        final WfBinaryString messageBinStr = toBinStr(messageStr);

        // Create and decode message header, and determine message type
        header = new WfMessageSegment(messageType.getHeaderFields());
        bitCursor = header.decode(messageBinStr, bitCursor);
        messageType = WfMessageType.getType(header.getFieldValue(FIELD_MESSAGETYPE));

        // Create and decode message body
        body = new WfMessageSegment(messageType.getBodyFields());
        bitCursor = body.decode(messageBinStr, bitCursor);

        // Add and decode additional fields for some message types
        switch (messageType) {
            case T:
                // Determine pseudo message type and extend test message body with pseudo message body
                final WfMessageType pseudoMessageType = WfMessageType.getType(body.getFieldValue(FIELD_TESTMESSAGETYPE));
                body.append(new WfMessageSegment(pseudoMessageType.getBodyFields()));
                break;
            case Q:
                // Extend request message body with request fields (calculated with remaining bits)
                final int nRequestObjects = (messageBinStr.length() - bitCursor) / 16;   // One request object requires 2 fields of 8 bits
                body.append(new WfMessageSegment(messageType.createRequestFields(nRequestObjects)));
                break;
            default:
                break;
        }
        body.decode(messageBinStr, bitCursor);
        return this;
    }

    /**
     * Compiles a new Whiteflag core message object from field values
     * @param fieldValues String array with the values for the message fields
     * @return this {@link WfMessageCreator}
     * @throws WfCoreException if the provided values are invalid
     */
    public final WfMessageCreator compile(final String[] fieldValues) throws WfCoreException {
        // Create message header, set field values and determine message type
        header = new WfMessageSegment(messageType.getHeaderFields());
        header.setAllFieldValues(Arrays.copyOfRange(fieldValues, 0, header.getNoFields()));
        messageType = WfMessageType.getType(header.getFieldValue(FIELD_MESSAGETYPE));

        // Create message body based on message type
        body = new WfMessageSegment(messageType.getBodyFields());

        // Add additional fields to message body for some message types
        switch (messageType) {
            case T:
                // Determine pseudo message type and extend test message body with pseudo message body
                final WfMessageType pseudoMessageType = WfMessageType.getType(fieldValues[header.getNoFields()]);
                body.append(new WfMessageSegment(pseudoMessageType.getBodyFields()));
                break;
            case Q:
                // Extend request message body with request fields (calculated with remaining fields)
                final int nRequestObjects = (fieldValues.length - (header.getNoFields() + body.getNoFields())) / 2;  // One request object requires 2 fields
                body.append(new WfMessageSegment(messageType.createRequestFields(nRequestObjects)));
                break;
            default:
                break;
        }
        body.setAllFieldValues(Arrays.copyOfRange(fieldValues, header.getNoFields(), fieldValues.length));
        return this;
    }

    /* PRIVATE METHODS: helper functions */

    /**
     * Converts a hexadecimal string to a {@link WfBinaryString}
     * @param hex String with hexadecimal representation of message
     * @throws WfCoreException if invalid hexadecimal encoded message
     */
    private final WfBinaryString toBinStr(final String hex) throws WfCoreException {
        try {
            return new WfBinaryString().setHexValue(hex);
        } catch (IllegalArgumentException e) {
            throw new WfCoreException("Invalid hexadecimal encoded message: " + e.getMessage());
        }
    }
}
