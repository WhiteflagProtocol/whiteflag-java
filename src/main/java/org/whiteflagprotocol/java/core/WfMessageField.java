/*
 * Whiteflag Java Library
 */
package org.whiteflagprotocol.java.core;

import java.util.regex.Pattern;

/**
 * Whiteflag message field class
 * 
 * <p>
 * This class represents a Whiteflag message field. Instances of this
 * class represent specific fields in specific message types. A field is
 * defined by a number of properties: name, allowed values, encoding,
 * starting byte and ending byte.
 * 
 * @wfref 4.1 Message Structure
 */
public class WfMessageField {

    /* PROPERTIES */

    /* Fixed properties upon instantiation */
    /**
     * The name of the field
     */
    public final String name;
    /**
     * The regex pattern defining allowed values
     */
    public final Pattern pattern;
    /**
     * The {@link WfMessageCodec.Encoding} encoding type of the field
     */
    public final WfMessageCodec.Encoding encoding;
    /**
     * The starting byte of the field in a serialized / uncompressed message
     */
    public final int startByte;
    /**
     * The ending byte (not included) of the field in a serialized / uncompressed
     * message
     */
    public final int endByte;

    /* Main properties */
    private String value;

    /* CONSTRUCTOR */

    /**
     * Constructs a new Whiteflag message field based on provided poperties
     * 
     * @param name      the name of the Whiteflag field
     * @param pattern   the regex pattern defining allowed values; if null, the
     *                  generic encoding regex is used
     * @param encoding  the encoding of the field
     * @param startByte the starting byte of the field in a serialized /
     *                  uncompressed message
     * @param endByte   the ending byte (not included) of the field in a serialized
     *                  / uncompressed message; ignored if encoding requires a fixed
     *                  field length
     */
    public WfMessageField(String name, String pattern, WfMessageCodec.Encoding encoding, int startByte, int endByte) {
        this.name = name;
        this.encoding = encoding;
        this.startByte = startByte;

        /* If fixed length encoding, automatically set end byte */
        if (Boolean.TRUE.equals(encoding.isFixedLength())) {
            this.endByte = startByte + encoding.byteLength();
        } else {
            this.endByte = endByte;
        }
        /* If no regex pattern given, use generic encoding pattern */
        if (pattern == null) {
            this.pattern = Pattern.compile(encoding.regex());
        } else {
            this.pattern = Pattern.compile(pattern);
        }
    }

    /**
     * Constructs a new Whiteflag message field from an existing message field,
     * without copying the value
     * 
     * @param field the {@link WfMessageField} to copy
     */
    public WfMessageField(final WfMessageField field) {
        this(field, 0);
    }

    /**
     * Constructs a new Whiteflag message field from an existing message field,
     * without copying the value
     * 
     * @param field the {@link WfMessageField} to copy
     * @param shift number of bytes to shift the field
     */
    public WfMessageField(final WfMessageField field, final int shift) {
        this.name = field.name;
        this.pattern = Pattern.compile(field.pattern.toString());
        this.encoding = field.encoding;
        this.startByte = field.startByte + shift;
        if (field.endByte < 0) {
            this.endByte = field.endByte;
        } else {
            this.endByte = field.endByte + shift;
        }
    }

    /* PUBLIC METHODS: basic object interface */

    /**
     * Returns the message field as a string
     * 
     * @return the value of the message field
     * @see #get()
     */
    @Override
    public final String toString() {
        return this.get();
    }

    /* PUBLIC METHODS: metadata & validators */

    /**
     * Checks if the message field value has been set
     * 
     * @return TRUE if the field has been set, else FALSE
     */
    public final Boolean isSet() {
        // Field is considered set if it contains a valid value
        return this.isValid();
    }

    /**
     * Checks if the message field contains a valid value
     * 
     * @return TRUE if the field contains a valid value, else FALSE
     */
    public final Boolean isValid() {
        return isValid(this.value);
    }

    /**
     * Checks if the provided data is a valid value for this field
     * 
     * @param data The data to be checked
     * @return TRUE if data is a valid value for this field
     */
    public final Boolean isValid(final String data) {
        if (data == null) return false;
        return this.pattern.matcher(data).matches();
    }

    /**
     * Gets the byte length of the unencoded field value
     * 
     * @return the byte length of the unencoded field value
     */
    public final int byteLength() {
        if (this.endByte < 0) {
            if (this.value == null) return 0;
            return this.value.length();
        }
        return (this.endByte - this.startByte);
    }

    /**
     * Gets the bit length of the encoded field
     * 
     * @return the bit length of the compressed encoded field value
     */
    public final int bitLength() {
        if (this.endByte < 0) {
            if (this.value == null) return 0;
            return this.encoding.bitLength(this.value.length());
        }
        return this.encoding.bitLength(this.endByte - this.startByte);
    }

    /* PUBLIC METHODS: getters & setters */

    /**
     * Gets the value of the message field
     * 
     * @return the field value
     */
    public final String get() {
        return this.value;
    }

    /**
     * Sets the value of the message field if not already set
     * 
     * @param data the data representing the field value
     * @return TRUE if field value is set, FALSE if field already set or data is
     *         invalid
     */
    public final Boolean set(final String data) {
        // Cannot set value twice
        if (Boolean.TRUE.equals(this.isSet())) {
            return false;
        }
        // Set if data is valid
        if (Boolean.TRUE.equals(isValid(data))) {
            this.value = data;
            return true;
        }
        return false;
    }

    /* PUBLIC METHODS: operations */

    /**
     * Encodes the message field into compressed binary data
     * 
     * @return a byte array with the compressed binary encoded field value
     * @throws WfCoreException if the field cannot be encoded
     */
    public final byte[] encode() throws WfCoreException {
        // Check if field contains a valid value
        if (Boolean.FALSE.equals(this.isValid())) {
            throw new WfCoreException("Cannot encode " + this.name + debugString());
        }
        // Return encoded field
        return WfMessageCodec.encodeField(this);
    }

    /**
     * Decodes the compressed binary data and sets field value
     * 
     * @param data a byte array with the compressed binary encoded field value
     * @return the uncompressed value of the field
     * @throws WfCoreException if the field cannot be decoded
     */
    public final Boolean decode(final byte[] data) throws WfCoreException {
        String str = WfMessageCodec.decodeField(this, data);
        return this.set(str);
    }

    /* PROTECTED METHODS */

    /**
     * Gives debug information of the field
     * 
     * @return field name, value and pattern and validity check
     */
    protected String debugString() {
        return this.name + " field [\"" + this.value + "\", /" + this.pattern.toString() + "/, " + this.isValid() + "]";
    }
}
