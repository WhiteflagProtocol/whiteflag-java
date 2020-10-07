/*
 * Whiteflag Java Library
 */
package org.whiteflag.protocol.core;

import java.util.Arrays;

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
    private static final String PROTOCOL_VERSION = "1";
    private static final String FIELD_VERSION = "Version";
    private static final String FIELD_MESSAGETYPE = "MessageCode";
    private static final String FIELD_TESTMESSAGETYPE = "PseudoMessageCode";

    /* Cursors for deserialzing and decoding */
    private int byteCursor = 0;
    private int bitCursor = 0;

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
    public final WfMessageCore create() throws WfCoreException {
        return new WfMessageCore(messageType, header, body);
    }

    /**
     * Deserializes a serialized Whiteflag message and creates a new Whiteflag core message object
     * @param messageCode String with message code
     * @return this {@link WfMessageCreator}
     * @throws WfCoreException if the provided values are invalid
     */
    public final WfMessageCreator newType(final String messageCode) throws WfCoreException {
        // Create header and body based on message code
        messageType = WfMessageType.getType(messageCode);
        header = new WfMessageSegment(messageType.getHeaderFields());
        body = new WfMessageSegment(messageType.getBodyFields());

        // Set version and message code field values
        header.setFieldValue(FIELD_VERSION, PROTOCOL_VERSION);
        header.setFieldValue(FIELD_MESSAGETYPE, messageCode);

        return this;
    }

    /**
     * Deserializes a serialized Whiteflag message and creates a new Whiteflag core message object
     * @param messageStr String with the uncompressed serialized message
     * @return this {@link WfMessageCreator}
     * @throws WfCoreException if the provided values are invalid
     */
    public final WfMessageCreator deserialize(final String messageStr) throws WfCoreException {
        // Cursor pointing to next field to be deserialized
        int fieldCursor = 0;

        // Create and deserialize message header, and determine message type
        header = new WfMessageSegment(messageType.getHeaderFields());
        deserialiseSegment(header, messageStr, fieldCursor);
        messageType = WfMessageType.getType(header.getFieldValue(FIELD_MESSAGETYPE));

        // Create and deserialize message body
        body = new WfMessageSegment(messageType.getBodyFields());
        deserialiseSegment(body, messageStr, fieldCursor);
        fieldCursor = body.getNoFields();

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
        deserialiseSegment(body, messageStr, fieldCursor);

        return this;
    }

    /**
     * Decodes an encoded Whiteflag message and creates a new Whiteflag core message object
     * @param messageStr String with the hexadecimal representation of the encoded message
     * @return this {@link WfMessageCreator}
     * @throws WfCoreException if the encoded message is invalid
     */
    public final WfMessageCreator decode(final String messageStr) throws WfCoreException {
        // Cursor pointing to next field to be decoded
        int fieldCursor = 0;

        // Convert hexadecimal string representation into binary string
        final WfBinaryString messageBinStr = toBinStr(messageStr);

        // Create and decode message header, and determine message type
        header = new WfMessageSegment(messageType.getHeaderFields());
        decodeSegment(header, messageBinStr, fieldCursor);
        messageType = WfMessageType.getType(header.getFieldValue(FIELD_MESSAGETYPE));

        // Create and decode message body
        body = new WfMessageSegment(messageType.getBodyFields());
        decodeSegment(body, messageBinStr, fieldCursor);
        fieldCursor = body.getNoFields();

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
        decodeSegment(body, messageBinStr, fieldCursor);

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

    /**
     * Gets field values from a serialized message for the specified segment
     * @param segment {@link WfMessageSegment} with the message fields to be deserialized
     * @param messageStr String with the full serialized message
     * @param start starting index indicating with which field from the segment to begin
     * @throws WfCoreException if incorrect data or field order
     */
    private final WfMessageSegment deserialiseSegment(WfMessageSegment segment, final String messageStr, final int start) throws WfCoreException {
        for (int i = start; i < segment.getNoFields(); i++) {
            final WfMessageField field = segment.getField(i);

            // Check field sequence
            if (byteCursor != field.startByte) {
                throw new WfCoreException("Invalid field order when deserializing: did not expect field " + field.name + " at byte " + byteCursor);
            }
            // Get field value from serialized message part
            String value;
            if (field.endByte < 0) {
                value = messageStr.substring(field.startByte);
            } else {
                value = messageStr.substring(field.startByte, field.endByte);
            }
            // Set the field value and check result
            if (Boolean.FALSE.equals(segment.setFieldValue(i, value))) {
                throw new WfCoreException("Invalid data provided for " + field.name + " field in uncompressed serialized message at byte " + byteCursor + ": " + value + " does not match regex " + field.pattern.toString());
            }
            // Update cursors
            bitCursor += field.bitLength();
            byteCursor = field.endByte;
        }
        // Return updated segment
        return segment;
    }

    /**
     * Decodes field values from an encoded message for the specified segment
     * @param segment {@link WfMessageSegment} with the message fields to be decoded
     * @param messageBinStr {@link WfBinaryString} with the encoded message
     * @param start starting index indicating with which field from the segment to begin
     * @throws WfCoreException if incorrect data or field order
     */
    private final WfMessageSegment decodeSegment(WfMessageSegment segment, final WfBinaryString messageBinStr, final int start) throws WfCoreException {
        for (int i = start; i < segment.getNoFields(); i++) {
            final WfMessageField field = segment.getField(i);
            final int fieldEndBit = bitCursor + field.bitLength();

            // Check field sequence because message is decoded segment by segment
            if (byteCursor != field.startByte) {
                throw new WfCoreException("Invalid field order when decoding: did not expect field " + field.name + " at byte " + byteCursor);
            }
            // Decode field value from encoded message part
            String value;
            if (field.endByte < 0) {
                value = field.decode(messageBinStr.sub(bitCursor));
            } else {
                value = field.decode(messageBinStr.sub(bitCursor, fieldEndBit));
            }
            // Set the field value and check result
            if (Boolean.FALSE.equals(segment.setFieldValue(i, value))) {
                throw new WfCoreException("Invalid data when decoding " + field.name + " field from encoded binary message at bit " + bitCursor + ": " + value + " does not match regex " + field.pattern.toString());
            }
            // Update cursors
            bitCursor = fieldEndBit;
            byteCursor = field.endByte;
        }
        // Return updated segment
        return segment;
    }
}
