/*
 * Whiteflag Java Library
 */
package org.whiteflagprotocol.java.core;

import java.util.Set;

/**
 * Whiteflag basic message class
 * 
 * <p> This is a class defining a generic basic Whiteflag message. It
 * includes common properties and methods of all message types, and
 * implementation independent logic specified by the Whiteflag standard.
 * A message is composed of two {@link WfMessageSegment}s: a message 
 * header and a message body.
 * 
 * @wfref 4. Message Format
 * 
 * @since 1.0
 */
public class WfBasicMessage {

    /* PROPERTIES */

    /* Message type */
    /**
     * The type of the message
     * @wfref 2.4.2 Message Functionality
     */
    public final WfMessageType type;

    /* Message parts */
    /**
     * The message header
     * @wfref 4.2 Message Header
     */
    public final WfMessageSegment header;
    /**
     * The message body
     * @wfref 4.3 Message Body
     */
    public final WfMessageSegment body;

    /* Constants */
    private static final String FIELD_MESSAGETYPE = "MessageCode";

    /* CONSTRUCTORS */

    /**
     * Creates a basic Whiteflag message from a header and a body
     * @param type the message type
     * @param header the message header
     * @param body the message body
     */
    protected WfBasicMessage(final WfMessageType type, final WfMessageSegment header, final WfMessageSegment body) {
        this.type = type;
        this.header = header;
        this.body = body;
    }

    /**
     * Creates a basic Whiteflag message from an existing basic message
     * @since 1.1
     * @param message the basic message
     */
    protected WfBasicMessage(WfBasicMessage message) {
        this.type = message.type;
        this.header = message.header;
        this.body = message.body;
    }

    /* PUBLIC METHODS */

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

    /**
     * Checks if this message contains valid data
     * @return TRUE if all message fields contain valid data, else FALSE
     */
    public boolean isValid() {
        if (this.type == WfMessageType.ANY) return false;
        if (Boolean.FALSE.equals(header.isValid())) return false;
        if (Boolean.FALSE.equals(body.isValid())) return false;
        return this.type.getCode().equals(header.get(FIELD_MESSAGETYPE));
    }

    /**
     * Checks if the specified message field contains valid data
     * @param fieldname the name of the field
     * @return TRUE if the field contains valid data, else FALSE
     */
    public final boolean isValid(final String fieldname) {
        if (Boolean.TRUE.equals(header.isValid(fieldname))) return true;
        return body.isValid(fieldname);
    }

    /**
     * Checks if the provided data is valid for the specified message field
     * @param fieldname the name of the field
     * @param data the value to be checked
     * @return TRUE if the field contains valid data, else FALSE
     */
    public final boolean isValid(final String fieldname, final String data) {
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
    public boolean set(final String fieldname, final String data) {
        if (Boolean.TRUE.equals(header.set(fieldname, data))) return true;
        return body.set(fieldname, data);
    }

    /**
     * Serializes the Whiteflag message
     * @return the serialized message, i.e. the concatinated string of field values
     * @throws WfCoreException if any of the fields does not contain valid data
     */
    public String serialize() throws WfCoreException {
        if (Boolean.FALSE.equals(this.isValid())) {
            throw new WfCoreException("Cannot serialize message with invalid or incomplete data fields", null);
        }
        return header.serialize() + body.serialize();
    }

    /**
     * Encodes the Whiteflag message
     * @return a buffer with the binary encoded Whiteflag message
     * @throws WfCoreException if any of the fields does not contain valid data
     */
    public WfBinaryBuffer encode() throws WfCoreException {
        if (Boolean.FALSE.equals(this.isValid())) {
            throw new WfCoreException("Cannot encode message with invalid or incomplete data fields", null);
        }
        return header.encode().append(body.encode());
    }
}
