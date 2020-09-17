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

    /* Flag to prevent second creation */
    private Boolean messageCreated = false;

    /* Message parameters */
    private String messageCode;
    private int nFields;
    private int nHeaderFields;

    /* Indexes */
    private int currentBitIndex = 0;

    /* CONSTRUCTOR */

    /**
     * Creates a Whiteflag message builder
     */
    public WfMessageCreator() {
        // Nothing required for instantiating a Whiteflag creator object
    }

    /* PUBLIC METHODS: operations */

    /**
     * Deserializes a serialized Whiteflag message and creates a new Whiteflag core message object
     * @param serializedMessage String with the uncompressed serialized message
     * @return a {@link WfMessageCore} Whiteflag message
     * @throws WfCoreException if the provided values are invalid
     */
    public final WfMessageCore deserialize(String serializedMessage) throws WfCoreException {
        // Check if message already created
        checkCreation();

        // Get number of bytes of serialized message
        int nBytes = serializedMessage.length();

        // Create and deserialize message header
        header = initHeader();
        header = deserialiseSegment(header, serializedMessage);

        // Determine message type
        messageCode = header.getFieldValue("MessageCode");

        // Create and deserialize message body based on message type
        body = initBody(messageCode);
        body = deserialiseSegment(body, serializedMessage);

        // Deserialize additional fields of some message types
        switch (messageCode) {
            case "T":
                // Extend test message body with pseudo message body
                String pseudoMessageCode = body.getFieldValue("PseudoMessageCode");
                WfMessageSegment pseudoBody = new WfMessageSegment(WfMessageDefinitions.getBodyFields(pseudoMessageCode, lastByte()));
                pseudoBody = deserialiseSegment(pseudoBody, serializedMessage);
                body.append(pseudoBody);
                break;
            case "Q":
                // Extend request message body with request fields
                int nRequestObjects = (nBytes - lastByte()) / 4;                  // One request object requires 2 fields of 2 bytes
                WfMessageSegment requestFields = new WfMessageSegment(WfMessageDefinitions.getRequestFields(nRequestObjects, lastByte()));
                requestFields = deserialiseSegment(requestFields, serializedMessage);
                body.append(requestFields);
                break;
            default:
                // Nothing to do for other message types
                break;
        }
        // Create and return the Whiteflag message
        messageCreated = true;
        return new WfMessageCore(header, body);
    }

    /**
     * Decodes an encoded Whiteflag message and creates a new Whiteflag core message object
     * @param encodedMessage String with the hexadecimal representation of the encoded message
     * @return a {@link WfMessageCore} Whiteflag message
     * @throws WfCoreException if the provided values are invalid
     */
    public final WfMessageCore decode(String encodedMessage) throws WfCoreException {
        // Check if message already created
        checkCreation();

        // COnvert hexadecimal string representation into binary string
        WfBinaryString binaryMessage = new WfBinaryString();
        try {
            binaryMessage.setHexValue(encodedMessage);
        } catch (IllegalArgumentException e) {
            throw new WfCoreException("Cannot decode message: " + e.getMessage());
        }
        // Create and decode message header
        header = initHeader();
        header = decodeSegment(header, binaryMessage);

        // Determine message type
        messageCode = header.getFieldValue("MessageCode");

        // Create and decode message body based on message type
        body = initBody(messageCode);
        body = decodeSegment(body, binaryMessage);

        // Decode additional fields of some message types
        switch (messageCode) {
            case "T":
                // Extend test message body with pseudo message body
                String pseudoMessageCode = body.getFieldValue("PseudoMessageCode");
                WfMessageSegment pseudoBody = new WfMessageSegment(WfMessageDefinitions.getBodyFields(pseudoMessageCode, lastByte()));
                pseudoBody = decodeSegment(pseudoBody, binaryMessage);
                body.append(pseudoBody);
                break;
            case "Q":
                // Extend request message body with request fields
                int nRequestObjects = (binaryMessage.length() - currentBitIndex) / 16;      // One request object requires 2 fields of 8 bits
                WfMessageSegment requestFields = new WfMessageSegment(WfMessageDefinitions.getRequestFields(nRequestObjects, lastByte()));
                requestFields = decodeSegment(requestFields, binaryMessage);
                body.append(requestFields);
            default:
                // Nothing to do for other message types
                break;
        }
        // Create and return the Whiteflag message
        messageCreated = true;
        return new WfMessageCore(header, body);
    }

    /**
     * Compiles a new Whiteflag core message object from field values
     * @param fieldValues String array with the values for the message fields
     * @return a {@link WfMessageCore} Whiteflag message
     * @throws WfCoreException if the provided values are invalid
     */
    public final WfMessageCore compile(String[] fieldValues) throws WfCoreException {
        // Check if message already created
        checkCreation();

        // Get number of provided fields
        nFields = fieldValues.length;

        // Create message header, set field values and determine message type
        header = initHeader();
        header.setAllFieldValues(Arrays.copyOfRange(fieldValues, 0, nHeaderFields));
        messageCode = header.getFieldValue("MessageCode");

        // Create message body based on message type
        body = initBody(messageCode);
        switch (messageCode) {
            case "T":
                // Extend test message body with pseudo message body
                String pseudoMessageCode = fieldValues[nHeaderFields];
                body.append(new WfMessageSegment(WfMessageDefinitions.getBodyFields(pseudoMessageCode, lastByte())));
                break;
            case "Q":
                // Extend request message body with request fields
                int nRequestObjects = (nFields - (nHeaderFields + body.getNoFields())) / 2;  // One request object requires 2 fields
                body.append(new WfMessageSegment(WfMessageDefinitions.getRequestFields(nRequestObjects, lastByte())));
                break;
            default:
                // Nothing to do for other message types
                break;
        }
        // Set message body field values
        body.setAllFieldValues(Arrays.copyOfRange(fieldValues, nHeaderFields, nFields));

        // Create and return the Whiteflag message
        messageCreated = true;
        return new WfMessageCore(header, body);
    }

    /* PRIVATE METHODS: helper functions */

    /**
     * Checks whether this message creator obejct already created its message
     */
    private final void checkCreation() {
        if (Boolean.TRUE.equals(messageCreated)) {
            throw new IllegalStateException("This message creator instance already created its message");
        }
    }

    /**
     * Gives the current last byte of the message
     */
    private final int lastByte() {
        return body.getField(body.getNoFields() - 1).endByte;
    }

    /**
     * Initialises new message header segment and sets relatedv ariables
     */
    private final WfMessageSegment initHeader() {
        // Create message header segment and update header characteristics
        header = new WfMessageSegment(WfMessageDefinitions.getHeaderFields());
        nHeaderFields = header.getNoFields();

        // Return new message header
        return header;
    }

    /**
     * Initialises new message body segment and sets relatedv ariables
     */
    private final WfMessageSegment initBody(String messageCode) throws WfCoreException {
        // Create message body segment and update body characteristics
        int bodyOffset = header.getField(nHeaderFields - 1).endByte;
        body = new WfMessageSegment(WfMessageDefinitions.getBodyFields(messageCode, bodyOffset));

        // Return new message body
        return body;
    }

    /**
     * Gets field values from a serialized message for the specified segment
     */
    private final WfMessageSegment deserialiseSegment(WfMessageSegment segment, String serializedMessage) throws WfCoreException {

        // Deserialize field by field
        for (int i = 0; i < segment.getNoFields(); i++) {
            WfMessageField field = segment.getField(i);

            // Get field value from serialized message part
            String value;
            if (field.endByte < 0) {
                value = serializedMessage.substring(field.startByte);
            } else {
                value = serializedMessage.substring(field.startByte, field.endByte);
            }
            // Set the field value and check result
            if (Boolean.FALSE.equals(segment.setFieldValue(i, value))) {
                throw new WfCoreException("Invalid data provided for " + field.name + " field in uncompressed serialized message at byte " + field.startByte + ": " + value + " does not match regex " + field.pattern.toString());
            }
        }
        // Return updated segment
        return segment;
    }

    /**
     * Gets field values from a serialized message for the specified segment
     */
    private final WfMessageSegment decodeSegment(WfMessageSegment segment, WfBinaryString binaryMessage) throws WfCoreException {

        // Decode field by field
        for (int i = 0; i < segment.getNoFields(); i++) {
            WfMessageField field = segment.getField(i);
            int fieldEndBit = currentBitIndex + field.bitLength();

            // Decode field value from encoded message part
            String value;
            try {
                if (field.endByte < 0) {
                    value = field.decode(binaryMessage.sub(currentBitIndex));
                } else {
                    value = field.decode(binaryMessage.sub(currentBitIndex, fieldEndBit));
                }
            } catch (Exception e) {
                throw new WfCoreException("Exception " + e.getClass() + " when decoding " + field.name + " field: " + e.getMessage());
            }
            // Set the field value and check result
            if (Boolean.FALSE.equals(segment.setFieldValue(i, value))) {
                throw new WfCoreException("Invalid data when decoding " + field.name + " field from encoded binary message starting at bit " + currentBitIndex + ": " + value + " does not match regex " + field.pattern.toString());
            }
            // Bit index of next field
            currentBitIndex = fieldEndBit;
        }
        // Return updated segment
        return segment;
    }
}
