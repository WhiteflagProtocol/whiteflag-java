/*
 * Whiteflag Java Library
 */
package org.whiteflagprotocol.java.core;

import java.util.regex.Pattern;

/**
 * Whiteflag message field class
 * 
 * <p> This class represents a Whiteflag message field. Instances of this
 * class represent specific fields in specific message types. A field is
 * defined by a number of properties: name, allowed values, encoding,
 * starting byte and ending byte. The field is considered set if it contains
 * a valid value. Its value cannot be changed once set.
 * 
 * @wfref 4.1 Message Structure
 * 
 * @since 1.0
 */
public final class WfMessageField {

    /* PROPERTIES */

    /**
     * The name of the field
     */
    public final String name;
    /**
     * The regex pattern defining allowed values
     */
    public final Pattern pattern;
    /**
     * The encoding of the field
     */
    public final WfMessageCodec.Encoding encoding;
    /**
     * The starting byte of the field in a serialized / uncompressed message
     */
    public final int startByte;
    /**
     * The ending byte (not included) of the field in a serialized / uncompressed message
     */
    public final int endByte;
    /**
     * The value of the field
     */
    private String value;

    /* CONSTRUCTORS */

    /**
     * Constructs a new Whiteflag message field based on provided poperties
     * @param name the name of the Whiteflag field
     * @param pattern the regex pattern defining allowed values
     * @param encoding the encoding of the field
     * @param startByte the starting byte of the field in a serialized / uncompressed message
     * @param endByte the ending byte (not included) of the field in a serialized / uncompressed message
     */
    private WfMessageField(String name, String pattern, WfMessageCodec.Encoding encoding, int startByte, int endByte) {
        this.name = name;
        this.pattern = Pattern.compile(pattern);
        this.encoding = encoding;
        this.startByte = startByte;
        this.endByte = endByte;
    }

    /* STATIC FACTORY METHODS */

    /**
     * Defines a new Whiteflag message field
     * @since 1.1
     * @param name the name of the message  field
     * @param pattern the regex pattern defining allowed values; if null the generic encoding regex is used
     * @param encoding the encoding of the field
     * @param startByte the starting byte of the field in a serialized / uncompressed message
     * @param endByte the ending byte (not included) of the field in a serialized / uncompressed message; ignored if encoding requires a fixed field length
     * @return a new message field
     * @wfref 4.1.2 Encoding
     */
    public static final WfMessageField define(final String name, String pattern, final WfMessageCodec.Encoding encoding, int startByte, int endByte) {
        /* If fixed length encoding, automatically set end byte */
        if (Boolean.TRUE.equals(encoding.isFixedLength())) {
            endByte = startByte + encoding.byteLength();
        }
        /* If no regex pattern given, use generic encoding pattern */
        if (pattern == null) {
            pattern = encoding.regex();
        }
        /* Call constructor and return new Whiteflag message field object */
        return new WfMessageField(name, pattern, encoding, startByte, endByte);
    }

    /**
     * Creates a new Whiteflag message field from an existing field, without copying the value
     * @param field the message field to copy
     * @return a new message field
     */
    public static final WfMessageField from(final WfMessageField field) {
        return from(field, 0);
    }

    /**
     * Creates a new Whiteflag message field from an existing field, without copying the value
     * @param field the message field to copy
     * @param shift number of bytes to shift the field
     * @return a new message field
     */
    public static final WfMessageField from(final WfMessageField field, final int shift) {
        int startByte = field.startByte + shift;
        int endByte = field.endByte;
        if (field.endByte > -1) endByte += shift;
        return new WfMessageField(field.name, field.pattern.toString(), field.encoding, startByte, endByte);
    }

    /* PUBLIC METHODS */

    /**
     * Returns the message field as a string
     * @return the value of the message field
     * @see #get()
     */
    @Override
    public final String toString() {
        return this.get();
    }

    /**
     * Checks if the message field value has been set
     * @return TRUE if the field has been set, else FALSE
     */
    public final boolean isSet() {
        /* Field is considered set if it contains a valid value */
        return this.isValid();
    }

    /**
     * Checks if the message field contains a valid value
     * @return TRUE if the field contains a valid value, else FALSE
     */
    public final boolean isValid() {
        return isValid(this.value);
    }

    /**
     * Checks if the provided data is a valid value for this field
     * @param data The data to be checked
     * @return TRUE if data is a valid value for this field
     */
    public final boolean isValid(final String data) {
        if (data == null) return false;
        return this.pattern.matcher(data).matches();
    }

    /**
     * Gets the byte length of the unencoded field value
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
     * @return the bit length of the compressed encoded field value
     */
    public final int bitLength() {
        if (this.endByte < 0) {
            if (this.value == null) return 0;
            return this.encoding.bitLength(this.value.length());
        }
        return this.encoding.bitLength(this.endByte - this.startByte);
    }

    /**
     * Gets the value of the message field
     * @return the field value
     */
    public final String get() {
        return this.value;
    }

    /**
     * Sets the value of the message field if not already set
     * @param data the data representing the field value
     * @return TRUE if field value is set, FALSE if field already set or data is invalid
     */
    public final Boolean set(final String data) {
        /* Cannot set value twice */
        if (Boolean.TRUE.equals(this.isSet())) {
            return false;
        }
        /* Set if data is valid */
        if (Boolean.TRUE.equals(isValid(data))) {
            this.value = data;
            return true;
        }
        return false;
    }

    /**
     * Encodes the message field into compressed binary data
     * @since 1.1
     * @return a byte array with the compressed binary encoded field value
     * @throws WfCoreException if the field cannot be encoded
     */
    public final byte[] encode() throws WfCoreException {
        if (Boolean.FALSE.equals(this.isValid())) {
            throw new WfCoreException("Cannot encode invalid field: " + debugInfo(), null);
        }
        return WfMessageCodec.encodeField(this);
    }

    /**
     * Decodes the compressed binary data and sets field value
     * @since 1.1
     * @param data a byte array with the compressed binary encoded field value
     * @return TRUE is the field could be correctly decoded and set, else FALSE
     * @throws WfCoreException if decoding is not possible
     */
    public final String decode(final byte[] data) throws WfCoreException {
        /* Check if field already set */
        if (Boolean.TRUE.equals(this.isSet())) {
            throw new WfCoreException("Cannot decode already set field: " + debugInfo(), null);
        }
        /* Decode, check data and set field */
        String str = WfMessageCodec.decodeField(this, data);
        if (Boolean.FALSE.equals(this.isValid(str))) {
            throw new WfCoreException("Decoded data is invalid for this field: " + debugInfo() + ": " + str, null);
        }
        if (Boolean.FALSE.equals(this.set(str))) {
            throw new WfCoreException("Could not set this field with decoded data: " + debugInfo() + ": " + str, null);
        }
        return str;
    }

    /* PROTECTED METHODS */

    /**
     * Gives debug information of the field
     * @return field name, value and pattern and validity check
     */
    protected final String debugInfo() {
        return this.name + " [data valid: " + this.isValid()
                         + ", regex: /" + this.pattern.toString() + "/"
                         + ", bytes: " + startByte + "-" + endByte + "]";
    }
}
