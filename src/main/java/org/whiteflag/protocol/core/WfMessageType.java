package org.whiteflag.protocol.core;


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
    any("", WfMessageDefinitions.UNDEFINED),

    /** 
     * Authentication message type
     */
    A("A", WfMessageDefinitions.authenticationFields),

    /** 
     * Cryptographic message type
     */
    K("K", WfMessageDefinitions.cryptoFields),

    /** 
     * Cryptographic message type
     */
    T("T", WfMessageDefinitions.testFields),

    /** 
     * Free Text message type
     */
    F("F", WfMessageDefinitions.freetextFields),

    /** 
     * Resource message type
     */
    R("R", WfMessageDefinitions.resourceFields),

    /** 
     * Protective Sign message type
     */
    P("P", WfMessageDefinitions.signsignalFields),

    /** 
     * Emergency Signal  message type
     */
    E("E", WfMessageDefinitions.signsignalFields),

    /** 
     * Cryptographic message type
     */
    S("S", WfMessageDefinitions.signsignalFields),

    /** 
     * Cryptographic message type
     */
    D("D", WfMessageDefinitions.signsignalFields),

    /** 
     * Cryptographic message type
     */
    I("I", WfMessageDefinitions.signsignalFields),

    /** 
     * Cryptographic message type
     */
    M("M", WfMessageDefinitions.signsignalFields),

    /** 
     * Cryptographic message type
     */
    Q("Q", WfMessageDefinitions.signsignalFields);

    /* PROPERTIES */

    /* The valid regex charset of an unencoded field value */
    private final String messageCode;
    private final WfMessageField[] headerFields = WfMessageDefinitions.headerFields;
    private final WfMessageField[] bodyFields;

    /* METHODS */

    /* Constructor */
    /**
     * @param messageCode String with message code of the message type
     * @param bodyFields array of {@link WfMessageField} body fields
     */
    private WfMessageType(final String messageCode, final WfMessageField[] bodyFields) {
        this.messageCode = messageCode;
        this.bodyFields = bodyFields;
    }

    /**
     * Returns the {@link WfMessageType} message type
     * @param code String with the message code
     */
    public static WfMessageType getType(final String code) throws WfCoreException {
        if (code == null || code.isEmpty()) return any;
        for (WfMessageType type : WfMessageType.values()) {
            if (type.messageCode.equalsIgnoreCase(code)) {
                return type;
            }
        }
        throw new WfCoreException("Invalid message type: " + code);
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
}
