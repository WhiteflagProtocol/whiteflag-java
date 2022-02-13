/*
 * Whiteflag Java Library
 */
package org.whiteflagprotocol.java.core;

import java.util.Arrays;

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
     * <p> Message introducing the sender on the network with the sender’s authentication data
     * @wfref 4.3.4 Management Messages: Authentication
     */
    A("A", authenticationBodyFields),

    /** 
     * Cryptographic message type
     * <p> Message for management of keys and parameters of cryptographic functions
     * @wfref 4.3.5 Management Messages: Cryptographic Support
     */
    K("K", cryptoBodyFields),

    /** 
     * Test message type
     * <p> Message that can be used for testing Whiteflag functionality by applications
     * @wfref 4.3.6 Management Messages: Test
     */
    T("T", testBodyFields),

    /** 
     * Resource message type
     * <p> Message to point to an internet resource
     * @wfref 4.3.2 Functional Messages: Resource
     */
    R("R", resourceBodyFields),

    /** 
     * Free Text message type
     * <p> Message to send a free text string
     * @wfref 4.3.3 Functional Messages: Free Text
     */
    F("F", freetextBodyFields),

    /** 
     * Protective Sign message type
     * <p> Sign to mark objects under the protection of international law
     * @wfref 4.3.1 Functional Messages: Signs/Signals
     * @wfref 4.3.1.2.1 Protective Signs
     */
    P("P", signsignalBodyFields),

    /** 
     * Emergency Signal message type
     * <p> Signal to send an emergency signal when in need of assistance
     * @wfref 4.3.1 Functional Messages: Signs/Signals
     * @wfref 4.3.1.2.2 Emergency Signals
     */
    E("E", signsignalBodyFields),

    /** 
     * Danger Sign message type
     * <p> Sign to mark a location or area of imminent danger, e.g. an area under attack, land mines, disaster, etc.
     * @wfref 4.3.1 Functional Messages: Signs/Signals
     * @wfref 4.3.1.2.3 Danger and Disaster Signs
     */
    D("D", signsignalBodyFields),

    /** 
     * Status Signal message type
     * <p> Signal to provide the status of an object, or specifically for persons: give a proof of life
     * @wfref 4.3.1 Functional Messages: Signs/Signals
     * @wfref 4.3.1.2.4 Status Signals
     */
    S("S", signsignalBodyFields),

    /** 
     * Infrastructure Sign message type
     * <p> Sign to mark critical infrastructure, e.g. roads, utilities, water treatment, hospitals, power plants etc.
     * @wfref 4.3.1 Functional Messages: Signs/Signals
     * @wfref 4.3.1.2.5 Infrastructure Signs
     */
    I("I", signsignalBodyFields),

    /** 
     * Mission Signal message type
     * <p> Signal to provide information on activities undertaken during a mission
     * @wfref 4.3.1 Functional Messages: Signs/Signals
     * @wfref 4.3.1.2.6 Mission Signals
     */
    M("M", signsignalBodyFields),

    /** 
     * Request Signal message type
     * <p> Signal to perform requests to other parties
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
     * @param bodyFields array of message body fields
     */
    private WfMessageType(final String messageCode, final WfMessageField[] bodyFields) {
        this.messageCode = messageCode;
        this.bodyFields = bodyFields;
    }

    /* PUBLIC METHODS */

    /**
     * Returns the message code string
     * @return the message code
     */
    public final String getCode() {
        return messageCode;
    }

    /**
     * Returns an array with the header fields
     * @return an array of the fields from the message header
     * @wfver v1-draft.6
     * @wfref 4.2.1 Generic Message Header
     */
    public final WfMessageField[] getHeaderFields() {
        return headerFields;
    }

    /**
     * Returns an array with the header fields that are never encrypted
     * @since 1.1
     * @return an array of the fields from the message header
     * @wfver v1-draft.6
     * @wfref 4.2.1 Generic Message Header
     * @wfref 4.1.4 Encryption
     */
    public final WfMessageField[] getUnencryptedHeaderFields() {
        return Arrays.copyOf(headerFields, 3);
    }

    /**
     * Returns an array with the body fields
     * @return an array with the fields from the message body
     */
    public final WfMessageField[] getBodyFields() {
        return bodyFields;
    }

    /**
     * Returns an array with additional Whiteflag sign/signal message body request fields
     * @param n the number of request objects
     * @return an array with the request message fields
     * @wfver v1-draft.6
     * @wfref 4.3.1.9 Object Request Fields
     */
    public final WfMessageField[] createRequestFields(final int n) {
        /* Request fields are only defined for Request message type */
        if (!this.equals(Q)) return WfMessageDefinitions.UNDEFINED;

        /* Field definitions */
        final WfMessageField objectField = WfMessageDefinitions.requestFields[0];
        final WfMessageField quantField = WfMessageDefinitions.requestFields[1];
        final int OBJECTFIELDSIZE = objectField.endByte - objectField.startByte;
        final int QUANTFIELDSIZE = quantField.endByte - quantField.startByte;

        /* Determine parameters */
        final int nFields = (n * 2);
        int startByte = objectField.startByte;

        /* Create fields array */
        WfMessageField[] fields = new WfMessageField[nFields];
        for (int i = 0; i < nFields; i += 2) {
            /* Calculate field number, beginnings and ends */
            final int nField = (i / 2) + 1;
            final int splitByte = startByte + OBJECTFIELDSIZE;
            final int endByte = splitByte + QUANTFIELDSIZE;

            /* Add object and quantity field to array */
            fields[i] = WfMessageField.define(objectField.name + nField, objectField.pattern.toString(), objectField.encoding, startByte, splitByte);
            fields[i + 1] = WfMessageField.define(objectField.name + nField + "Quant", quantField.pattern.toString(), quantField.encoding, splitByte, endByte);

            /* Starting byte of next field */
            startByte = endByte;
        }
        return fields;
    }

    /* PUBLIC STATIC METHODS */

    /**
     * Creates the message type from the message code
     * @since 1.1
     * @param messageCode the message code
     * @return the requested message type
     * @throws WfCoreException if the message type is invalid
     */
    public static final WfMessageType fromCode(final String messageCode) throws WfCoreException {
        if (messageCode == null || messageCode.isEmpty()) return ANY;
        for (WfMessageType type : values()) {
            if (type.messageCode.equalsIgnoreCase(messageCode)) return type;
        }
        throw new WfCoreException("Invalid message type: " + messageCode, null);
    }
}
