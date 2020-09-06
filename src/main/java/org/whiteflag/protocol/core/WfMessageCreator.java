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

    /* PUBLIC METHODS */

    /**
     * Creates a Whiteflag core message object from a serialised message
     * @param serializedMessage String with the uncompressed serialized message
     * @return a {@link WfMessageCore} Whiteflag message
     * @throws WfCoreException if the provided values are invalid
     */
    public final WfMessageCore deserialize(String serializedMessage) throws WfCoreException {
        // Get number of bytes
        int nBytes = serializedMessage.length();

        // Create message header and determine message type
        initHeader();
        header.setAllFieldValues(serializedMessage);
        messageCode = header.getFieldValue("MessageCode");

        // Create message body based on message type
        initBody(messageCode);
        switch (messageCode) {
            case "T":
                // Extend test message body with pseudo message body
                body.setAllFieldValues(serializedMessage);                                   // Deserialze the one field with pseudo message code
                String pseudoMessageCode = body.getFieldValue("PseudoMessageCode");
                body.add(new WfMessageSegment(WfMessageDefinitions.getBodyFields(pseudoMessageCode, lastBodyByte)));
                break;
            case "Q":
                // Extend request message body with request fields
                int nRequestObjects = (nBytes - lastBodyByte) / 4;                          // One request object requires 2 fields of 2 bytes
                body.add(new WfMessageSegment(WfMessageDefinitions.getRequestFields(nRequestObjects, lastBodyByte)));
                break;
            default:
                // Nothing to do for other message types
                break;
        }
        body.setAllFieldValues(serializedMessage);

        // Create and return the Whiteflag message
        return new WfMessageCore(header, body);
    }

    /**
     * Creates a Whiteflag core message object from field values
     * @param fieldValues String array with the values for the message fields
     * @return a {@link WfMessageCore} Whiteflag message
     * @throws WfCoreException if the provided values are invalid
     */
    public final WfMessageCore createFromValues(String[] fieldValues) throws WfCoreException {
        // Get number of fields
        int nFields = fieldValues.length;

        // Create message header and determine message type
        initHeader();
        header.setAllFieldValues(Arrays.copyOfRange(fieldValues, 0, nHeaderFields));
        messageCode = header.getFieldValue("MessageCode");

        // Create message body based on message type
        initBody(messageCode);
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

    /* PRIVATE METHODS */

    /**
     * Initialises new message header segment and sets relatedv ariables
     */
    private final void initHeader() {
        // Create message header segment
        header = new WfMessageSegment(WfMessageDefinitions.getHeaderFields());

        // Header characteristics
        nHeaderFields = header.getNoFields();
    }

    /**
     * Initialises new message body segment and sets relatedv ariables
     */
    private final void initBody (String messageCode) throws WfCoreException {
        // Create message body segment
        int bodyOffset = header.getFieldByIndex(nHeaderFields - 1).endByte;
        body = new WfMessageSegment(WfMessageDefinitions.getBodyFields(messageCode, bodyOffset));

        // Header characteristics
        nBodyFields = body.getNoFields();
        lastBodyByte = body.getFieldByIndex(nBodyFields - 1).endByte;
    }
}
