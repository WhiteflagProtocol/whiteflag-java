/*
 * Whiteflag Java Library
 */
package org.whiteflagprotocol.java.core;

/* Field defintions required for message type */
import static org.whiteflagprotocol.java.core.WfMessageDefinitions.*;

/**
 * Whiteflag message type
 *
 * <p> This enumerable class implements the message types as defined in
 * the Whiteflag standard.
 * 
 * @wfver v1-draft.6
 * @wfref 2.4.2 Message Functionality
 * 
 * @since 1.0
 */
public enum WfMessageType {
    /**
     * Undefined message type
     */
    ANY("", UNDEFINED),

    /** 
     * Authentication message type
     * @wfref 4.3.4 Management Messages: Authentication
     */
    A("A", authenticationBodyFields),

    /** 
     * Cryptographic message type
     * @wfref 4.3.5 Management Messages: Cryptographic Support
     */
    K("K", cryptoBodyFields),

    /** 
     * Test message type
     * @wfref 4.3.6 Management Messages: Test
     */
    T("T", testBodyFields),

    /** 
     * Resource message type
     * @wfref 4.3.2 Functional Messages: Resource
     */
    R("R", resourceBodyFields),

    /** 
     * Free Text message type
     * @wfref 4.3.3 Functional Messages: Free Text
     */
    F("F", freetextBodyFields),

    /** 
     * Protective Sign message type
     * @wfref 4.3.1 Functional Messages: Signs/Signals
     * @wfref 4.3.1.2.1 Protective Signs
     */
    P("P", signsignalBodyFields),

    /** 
     * Emergency Signal message type
     * @wfref 4.3.1 Functional Messages: Signs/Signals
     * @wfref 4.3.1.2.2 Emergency Signals
     */
    E("E", signsignalBodyFields),

    /** 
     * Danger Sign message type
     * @wfref 4.3.1 Functional Messages: Signs/Signals
     * @wfref 4.3.1.2.3 Danger and Disaster Signs
     */
    D("D", signsignalBodyFields),

    /** 
     * Status Signal message type
     * @wfref 4.3.1 Functional Messages: Signs/Signals
     * @wfref 4.3.1.2.4 Status Signals
     */
    S("S", signsignalBodyFields),

    /** 
     * Infrastructure Sign message type
     * @wfref 4.3.1 Functional Messages: Signs/Signals
     * @wfref 4.3.1.2.5 Infrastructure Signs
     */
    I("I", signsignalBodyFields),

    /** 
     * Mission Sign message type
     * @wfref 4.3.1 Functional Messages: Signs/Signals
     * @wfref 4.3.1.2.6 Mission Signals
     */
    M("M", signsignalBodyFields),

    /** 
     * Request Signal message type
     * @wfref 4.3.1 Functional Messages: Signs/Signals
     * @wfref 4.3.1.2.7 Request Signals
     */
    Q("Q", signsignalBodyFields);

    /* PROPERTIES */

    /* The valid regex charset of an unencoded field value */
    private final String messageCode;
    private final WfMessageField[] headerFields = genericHeaderFields;
    private final WfMessageField[] bodyFields;

    /* CONSTRUCTOR */

    /**
     * @param messageCode message code of the message type
     * @param bodyFields array of {@link WfMessageField} body fields
     */
    private WfMessageType(final String messageCode, final WfMessageField[] bodyFields) {
        this.messageCode = messageCode;
        this.bodyFields = bodyFields;
    }

    /* STATIC FACTORY METHODS */

    /**
     * Returns the {@link WfMessageType} message type
     * @since 1.1
     * @param messageCode the message code
     */
    public static WfMessageType fromCode(final String messageCode) throws WfCoreException {
        if (messageCode == null || messageCode.isEmpty()) return ANY;
        for (WfMessageType type : WfMessageType.values()) {
            if (type.messageCode.equalsIgnoreCase(messageCode)) return type;
        }
        throw new WfCoreException("Invalid message type: " + messageCode);
    }

    /* PUBLIC METHODS */

    /**
     * Returns the message code string
     * @return the message code
     */
    public String getCode() {
        return messageCode;
    }

    /**
     * Returns an array with the {@link WfMessageField} header fields
     */
    public WfMessageField[] getHeaderFields() {
        return headerFields;
    }

    /**
     * Returns an array with the {@link WfMessageField} body fields
     */
    public WfMessageField[] getBodyFields() {
        return bodyFields;
    }

    /**
     * Returns an array with additional Whiteflag sign/signal message body request fields
     * @param n the number of request objects
     * @return an array with the request message fields
     * @wfver v1-draft.6
     * @wfref 4.3.1.9 Object Request Fields
     */
    public WfMessageField[] createRequestFields(final int n) {
        // Request fields are only defined for Request message type
        if (!this.equals(Q)) return WfMessageDefinitions.UNDEFINED;

        // Field definitions
        final WfMessageField objectField = WfMessageDefinitions.requestFields[0];
        final WfMessageField quantField = WfMessageDefinitions.requestFields[1];
        final int OBJECTFIELDSIZE = objectField.endByte - objectField.startByte;
        final int QUANTFIELDSIZE = quantField.endByte - quantField.startByte;

        // Determine parameters
        final int nFields = (n * 2);
        int startByte = objectField.startByte;

        // Create fields array
        WfMessageField[] fields = new WfMessageField[nFields];
        for (int i = 0; i < nFields; i += 2) {
            // Calculate field number, beginnings and ends
            final int nField = (i / 2) + 1;
            final int splitByte = startByte + OBJECTFIELDSIZE;
            final int endByte = splitByte + QUANTFIELDSIZE;

            // Add object and quantity field to array
            fields[i] = WfMessageField.define(objectField.name + nField, objectField.pattern.toString(), objectField.encoding, startByte, splitByte);
            fields[i + 1] = WfMessageField.define(objectField.name + nField + "Quant", quantField.pattern.toString(), quantField.encoding, splitByte, endByte);

            // Starting byte of next field
            startByte = endByte;
        }
        return fields;
    }
}
