/*
 * Whiteflag Java Library
 */
package org.whiteflag.protocol.core;

/**
 * Whiteflag basic message core class
 * 
 * </p> This is a class defining a generic basic Whiteflag message. It
 * includes common properties and methods of all message types, and
 * implementation independent logic specified by the Whiteflag standard.
 * All implementation specific logic should be in a derived message subclass.
 */
public class WfMessageCore {

    /* PROPERTIES */

    /* Message type */
    public final WfMessageType type;

    /* Message parts */
    public final WfMessageSegment header;
    public final WfMessageSegment body;

    /* Constants */
    private static final String FIELD_MESSAGETYPE = "MessageCode";

    /* CONSTRUCTORS */

    /**
     * Creates a Whiteflag message from a header and a body segment
     * @param type the {@link WfMessageType} of the message
     * @param header the {@link WfMessageSegment} message header
     * @param body the {@link WfMessageSegment} message body
     */
    protected WfMessageCore(final WfMessageType type, final WfMessageSegment header, final WfMessageSegment body) {
        this.type = type;
        this.header = new WfMessageSegment(header);
        this.body = new WfMessageSegment(body);
    }

    /* PUBLIC METHODS: basic object interface */

    /**
     * Returns the message as a concatinated string of field values
     * @return String with serialized message
     */
    @Override
    public String toString() {
        String string;
        try {
            string = this.serialize();
        } catch (WfCoreException e) {
            return "";
        }
        return string; 
    }

    /* PUBLIC METHODS: metadata & validators */

    /**
     * Checks if this message contains valid data
     * @return TRUE if all message fields contain valid data, else FALSE
     */
    public Boolean isValid() {
        if (this.type == WfMessageType.ANY) return false;
        if (Boolean.FALSE.equals(header.isValid())) return false;
        if (Boolean.FALSE.equals(body.isValid())) return false;
        if (!this.type.getMessageCode().equals(header.getFieldValue(FIELD_MESSAGETYPE))) return false;
        return true;
    }

    /* PUBLIC METHODS: getters & setters */

    /**
     * Gets the value of the specified field
     * @param name String with the name of the requested field
     * @return String with the field value, or NULL if field does not exist
     */
    public String getFieldValue(final String name) {
        String value = header.getFieldValue(name);
        if (value != null) return value;
        return body.getFieldValue(name);
    }

    /**
     * Sets the value of the specified field
     * @param name String with the name of the field
     * @param data String with data to be set as the field value
     * @return TRUE if field value is set, FALSE if field does not exits, isalready set, or data is invalid
     */
    public Boolean setFieldValue(final String name, final String data) {
        if (Boolean.TRUE.equals(header.setFieldValue(name, data))) return true;
        return body.setFieldValue(name, data);
    }

    /* PUBLIC METHODS: operations */

    /**
     * Serializes the Whiteflag message
     * @return String with the serialized message, i.e. the concatinated string of field values
     * @throws WfCoreException if any of the field does not contain valid data
     */
    public String serialize() throws WfCoreException {
        if (Boolean.FALSE.equals(this.isValid())) {
            throw new WfCoreException("Cannot serialize message with invalid or incomplete data fields");
        }
        return header.serialize() + body.serialize();
    }

    /**
     * Encodes the Whiteflag message without a 0x prefix
     * @return hexadecimal string with the encoded Whiteflag message
     * @throws WfCoreException if any of the field does not contain valid data
     */
    public String encode() throws WfCoreException {
        return encode(false);
    }

    /**
     * Encodes the Whiteflag message
     * @param prefix if TRUE, the resulting string gets a 0x prefix
     * @return hexadecimal string with the encoded Whiteflag message
     * @throws WfCoreException if any of the field does not contain valid data
     */
    public String encode(final Boolean prefix) throws WfCoreException {
        if (Boolean.FALSE.equals(this.isValid())) {
            throw new WfCoreException("Cannot encode message with invalid or incomplete data fields");
        }
        return header.encode().append(body.encode()).toHexString(prefix);
    }
}
