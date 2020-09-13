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

    /* Message parameters */
    private String messageCode;
    private int nHeaderFields;
    private int nBodyFields;
    private int lastBodyByte;

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
        // Get number of bytes of serialized message
        int nBytes = serializedMessage.length();

        // Create and desrialize message header
        header = initHeader();
        header = deserialiseSegment(serializedMessage, header);

        // Determine message type
        messageCode = header.getFieldValue("MessageCode");

        // Create message body based on message type
        body = initBody(messageCode);
        switch (messageCode) {
            case "T":
                // Extend test message body with pseudo message body
                body = deserialiseSegment(serializedMessage, body);                 // Deserialze the one field with pseudo message code
                String pseudoMessageCode = body.getFieldValue("PseudoMessageCode");
                body.add(new WfMessageSegment(WfMessageDefinitions.getBodyFields(pseudoMessageCode, lastBodyByte)));
                break;
            case "Q":
                // Extend request message body with request fields
                int nRequestObjects = (nBytes - lastBodyByte) / 4;                  // One request object requires 2 fields of 2 bytes
                body.add(new WfMessageSegment(WfMessageDefinitions.getRequestFields(nRequestObjects, lastBodyByte)));
                break;
            default:
                // Nothing to do for other message types
                break;
        }
        body = deserialiseSegment(serializedMessage, body);

        // Create and return the Whiteflag message
        return new WfMessageCore(header, body);
    }

    /**
     * Decodes an encoded Whiteflag message and creates a new Whiteflag core message object
     * @param encodedMessage String with the hexadecimal representation of the encoded message
     * @return a {@link WfMessageCore} Whiteflag message
     * @throws WfCoreException if the provided values are invalid
     */
    public final WfMessageCore decode(String encodedMessage) throws WfCoreException {
        WfBinaryString binaryMessage = new WfBinaryString();
        try {
            binaryMessage.setHexValue(encodedMessage);
        } catch (IllegalArgumentException e) {
            throw new WfCoreException("Cannot decode message: " + e.getMessage());
        }

        // Create message header and determine message type
        header = initHeader();
        //TODO: loop through header fields to decode
        messageCode = header.getFieldValue("MessageCode");

        // Create message body based on message type
        body = initBody(messageCode);
        //TODO: loop through body fields to decode

        return new WfMessageCore(header, body);
    }

    /**
     * Compiles a new Whiteflag core message object from field values
     * @param fieldValues String array with the values for the message fields
     * @return a {@link WfMessageCore} Whiteflag message
     * @throws WfCoreException if the provided values are invalid
     */
    public final WfMessageCore compile(String[] fieldValues) throws WfCoreException {
        // Get number of provided fields
        int nFields = fieldValues.length;

        // Create message header and determine message type
        header = initHeader();
        header.setAllFieldValues(Arrays.copyOfRange(fieldValues, 0, nHeaderFields));
        messageCode = header.getFieldValue("MessageCode");

        // Create message body based on message type
        body = initBody(messageCode);
        switch (messageCode) {
            case "T":
                // Extend test message body with pseudo message body
                String pseudoMessageCode = fieldValues[nHeaderFields];
                body.add(new WfMessageSegment(WfMessageDefinitions.getBodyFields(pseudoMessageCode, lastBodyByte)));
                break;
            case "Q":
                // Extend request message body with request fields
                int nRequestObjects = (nFields - (nHeaderFields + nBodyFields)) / 2;  // One request object requires 2 fields
                body.add(new WfMessageSegment(WfMessageDefinitions.getRequestFields(nRequestObjects, lastBodyByte)));
                break;
            default:
                // Nothing to do for other message types
                break;
        }
        body.setAllFieldValues(Arrays.copyOfRange(fieldValues, nHeaderFields, nFields));

        // Create and return the Whiteflag message
        return new WfMessageCore(header, body);
    }

    /* PRIVATE METHODS: helper functions */

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
    private final WfMessageSegment initBody (String messageCode) throws WfCoreException {
        // Create message body segment and update body characteristics
        int bodyOffset = header.getField(nHeaderFields - 1).endByte;
        body = new WfMessageSegment(WfMessageDefinitions.getBodyFields(messageCode, bodyOffset));
        nBodyFields = body.getNoFields();
        lastBodyByte = body.getField(nBodyFields - 1).endByte;

        // Return new message body
        return body;
    }

    /**
     * Gets field values from a serialized message for the provided segment
     */
    private static final WfMessageSegment deserialiseSegment(String data, WfMessageSegment segment) throws WfCoreException {
        int nFields = segment.getNoFields();

        for (int i = 0; i < nFields; i++) {
            String value;
            WfMessageField field = segment.getField(i);

            // Get field value from serialized string 
            if (field.endByte < 0) {
                value = data.substring(field.startByte);
            } else {
                value = data.substring(field.startByte, field.endByte);
            }

            // Set the field value and check result
            if (Boolean.TRUE.equals(!segment.setFieldValue(i, value))) {
                throw new WfCoreException("Invalid data provided for " + field.name + " field in uncompressed serialized message at byte " + field.startByte + ": " + value + " must match " + field.pattern.toString());
            }
        }
        // Return updated segment
        return segment;
    }
}
