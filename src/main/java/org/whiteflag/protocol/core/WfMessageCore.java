/*
 * Whiteflag Java Library
 */
package org.whiteflag.protocol.core;

import java.util.Set;

/**
 * Whiteflag basic message core class
 * 
 * <p> This is a class defining a generic basic Whiteflag message. It
 * includes common properties and methods of all message types, and
 * implementation independent logic specified by the Whiteflag standard.
 * All implementation specific logic should be in a derived message subclass.
 * 
 * @wfref 4. Message Format
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
     * @param type the of the message
     * @param header the message header
     * @param body the message body
     */
    protected WfMessageCore(final WfMessageType type, final WfMessageSegment header, final WfMessageSegment body) {
        this.type = type;
        this.header = new WfMessageSegment(header);
        this.body = new WfMessageSegment(body);
    }

    /* PUBLIC METHODS: basic object interface */

    /**
     * Returns the message as a concatinated string of field values
     * @return the serialized message
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
        if (!this.type.getCode().equals(header.get(FIELD_MESSAGETYPE))) return false;
        return true;
    }

    /**
     * Checks if the specified message field contains valid data
     * @param fieldname the name of the field
     * @return TRUE if the field contains valid data, else FALSE
     */
    public final Boolean isValid(final String fieldname) {
        if (Boolean.TRUE.equals(header.isValid(fieldname))) return true;
        return body.isValid(fieldname);
    }

    /**
     * Checks if the provided data is valid for the specified message field
     * @param fieldname the name of the field
     * @param data the value to be checked
     * @return TRUE if the field contains valid data, else FALSE
     */
    public final Boolean isValid(final String fieldname, final String data) {
        if (Boolean.TRUE.equals(header.isValid(fieldname, data))) return true;
        return body.isValid(fieldname, data);
    }

    /**
     * Gets the number of fields in this message
     * @return the number of message fields
     */
    public final int getNoFields() {
        return header.getNoFields() + body.getNoFields();
    }

    /**
     * Gets the field names of this message
     * @return a string set with all field names
     */
    public Set<String> getFieldNames() {
        Set<String> names = header.getFieldNames();
        names.addAll(body.getFieldNames());
        return names;
    }

    /* PUBLIC METHODS: getters & setters */

    /**
     * Gets the value of the specified field
     * @param fieldname the name of the requested field
     * @return the field value, or NULL if field does not exist
     */
    public String get(final String fieldname) {
        String value = header.get(fieldname);
        if (value != null) return value;
        return body.get(fieldname);
    }

    /**
     * Sets the value of the specified field
     * @param fieldname the name of the field
     * @param data data to be set as the field value
     * @return TRUE if field value is set, FALSE if field does not exits, isalready set, or data is invalid
     */
    public Boolean set(final String fieldname, final String data) {
        if (Boolean.TRUE.equals(header.set(fieldname, data))) return true;
        return body.set(fieldname, data);
    }

    /* PUBLIC METHODS: operations */

    /**
     * Serializes the Whiteflag message
     * @return the serialized message, i.e. the concatinated string of field values
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
     * @return the hexadecimal representation of the encoded Whiteflag message
     * @throws WfCoreException if any of the field does not contain valid data
     */
    public String encode() throws WfCoreException {
        return encode(false);
    }

    /**
     * Encodes the Whiteflag message
     * @param prefix if TRUE, the resulting string gets a 0x prefix
     * @return the hexadecimal representation of the encoded Whiteflag message
     * @throws WfCoreException if any of the field does not contain valid data
     */
    public String encode(final Boolean prefix) throws WfCoreException {
        if (Boolean.FALSE.equals(this.isValid())) {
            throw new WfCoreException("Cannot encode message with invalid or incomplete data fields");
        }
        return header.encode().append(body.encode()).toHexString(prefix);
    }
}
