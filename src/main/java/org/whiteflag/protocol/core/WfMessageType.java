/*
 * Whiteflag Java Library
 */
package org.whiteflag.protocol.core;

/* Field defintions required for message type */
import static org.whiteflag.protocol.core.WfMessageDefinitions.*;

/**
 * Whiteflag message type
 *
 * </p> This enumerable class implements the message types as defined in
 * the Whiteflag standard. 
 * 
 * All message types have the same generic header.
 * The message bodies are defined 
 */
public enum WfMessageType {
    /**
     * Undefined message type
     */
    ANY("", UNDEFINED),

    /** 
     * Authentication message type
     */
    A("A", authenticationBodyFields),

    /** 
     * Cryptographic message type
     */
    K("K", cryptoBodyFields),

    /** 
     * Test message type
     */
    T("T", testBodyFields),

    /** 
     * Free Text message type
     */
    F("F", freetextBodyFields),

    /** 
     * Resource message type
     */
    R("R", resourceBodyFields),

    /** 
     * Protective Sign message type
     */
    P("P", signsignalBodyFields),

    /** 
     * Emergency Signal message type
     */
    E("E", signsignalBodyFields),

    /** 
     * Status Signal message type
     */
    S("S", signsignalBodyFields),

    /** 
     * Danger Sign message type
     */
    D("D", signsignalBodyFields),

    /** 
     * Infrastructure Sign message type
     */
    I("I", signsignalBodyFields),

    /** 
     * Mission Sign message type
     */
    M("M", signsignalBodyFields),

    /** 
     * Request Signal message type
     */
    Q("Q", signsignalBodyFields);

    /* PROPERTIES */

    /* The valid regex charset of an unencoded field value */
    private final String messageCode;
    private final WfMessageField[] headerFields = genericHeaderFields;
    private final WfMessageField[] bodyFields;

    /* METHODS */

    /* Constructor */
    /**
     * @param messageCode message code of the message type
     * @param bodyFields array of {@link WfMessageField} body fields
     */
    private WfMessageType(final String messageCode, final WfMessageField[] bodyFields) {
        this.messageCode = messageCode;
        this.bodyFields = bodyFields;
    }

    /**
     * Returns the {@link WfMessageType} message type
     * @param messageCode the message code
     */
    public static WfMessageType byCode(final String messageCode) throws WfCoreException {
        if (messageCode == null || messageCode.isEmpty()) return ANY;
        for (WfMessageType type : WfMessageType.values()) {
            if (type.messageCode.equalsIgnoreCase(messageCode)) return type;
        }
        throw new WfCoreException("Invalid message type: " + messageCode);
    }

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
            fields[i] = new WfMessageField(objectField.name + nField, objectField.pattern.toString(), objectField.encoding, startByte, splitByte);
            fields[i + 1] = new WfMessageField(objectField.name + nField + "Quant", quantField.pattern.toString(), quantField.encoding, splitByte, endByte);

            // Starting byte of next field
            startByte = endByte;
        }
        return fields;
    }
}
