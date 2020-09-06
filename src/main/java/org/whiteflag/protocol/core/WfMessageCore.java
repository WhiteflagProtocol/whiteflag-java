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

    /* Message parts */
    public final WfMessageSegment header;
    public final WfMessageSegment body;

    /* CONSTRUCTORS */

    /**
     * Creates a Whiteflag message from a header and a body segment
     * @param header the {@link WfMessageSegment} message header
     * @param body the {@link WfMessageSegment} message body
     */
    public WfMessageCore(WfMessageSegment header, WfMessageSegment body) {
        this.header = header;
        this.body = body;
    }

    /* PUBLIC METHODS: Message interface */

    /**
     * Checks if this message contains valid data
     * @return TRUE if all message fields contain valid data, else FALSE
     */
    public Boolean isValid() {
        if (Boolean.TRUE.equals(!header.isValid())) return false;
        if (Boolean.TRUE.equals(!body.isValid())) return false;
        return true;
    }

    /**
     * Serializes the Whiteflag message
     * @return String with the serialized message
     * @throws WfCoreException if any field does not contain valid data
     */
    public String serialize() throws WfCoreException {
        if (Boolean.TRUE.equals(!this.isValid())) {
            throw new WfCoreException("Cannot serialize message with invalid or incomplete data fields");
        }
        return header.serialize() + body.serialize();
    }

    /**
     * Encodes the Whiteflag message without a 0x prefix
     * @return hexadecimal string with the encoded Whiteflag message
     * @throws WfCoreException if any field does not contain valid data
     */
    public String encode() throws WfCoreException {
        return encode(false);
    }

    /**
     * Encodes the Whiteflag message
     * @param prefix if TRUE, the resulting string gets a 0x prefix
     * @return hexadecimal string with the encoded Whiteflag message
     * @throws WfCoreException if any field does not contain valid data
     */
    public String encode(Boolean prefix) throws WfCoreException {
        if (Boolean.TRUE.equals(!this.isValid())) {
            throw new WfCoreException("Cannot encode message with invalid or incomplete data fields");
        }
        return header.encode().add(body.encode()).toHexString(prefix);
    }

    /**
     * Gets the value of the specified field
     * @param name String with the name of the field
     * @return String with the field value, or NULL if field does not exist
     */
    public String getFieldValue(String name) {
        String value = header.getFieldValue(name);
        if (value != null) return value;
        return body.getFieldValue(name);
    }
}
