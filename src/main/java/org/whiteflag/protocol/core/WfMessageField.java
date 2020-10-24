/*
 * Whiteflag Java Library
 */
package org.whiteflag.protocol.core;

import java.util.regex.Pattern;
import java.nio.charset.StandardCharsets;

/* Required encoding types and parameters */
import static org.whiteflag.protocol.core.WfMessageField.Encoding.*;
import static org.whiteflag.protocol.core.WfBinaryString.QUADBIT;
import static org.whiteflag.protocol.core.WfBinaryString.OCTET;
import static org.whiteflag.protocol.core.WfBinaryString.BIT;

/**
 * Whiteflag message field class
 * 
 * <p> This claas represents a Whiteflag message field. Instances of this
 * class represent specific fields in specific message types with a number
 * of defined properties: name, allowed values, encoding, starting byte
 * and ending byte.
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
     * The {@link WfMessageField.Encoding} encoding type of the field
     */
    public final Encoding encoding;
    /**
     * The starting byte of the field in a serialized / uncompressed message
     */
    public final int startByte;
    /**
     * The ending byte (not included) of the field in a serialized / uncompressed message
     */
    public final int endByte;

    /* Main properties */
    private String value;

    /** 
     * Whiteflag message compressed field encodings
     * 
     * <p> Whiteflag fields use these compressed encodings, as defined for
     * each message field in the Whiteflag specification.
     */
    public enum Encoding {

        /**
         * 1-bit binary encoded bit
         */
        BIN("[01]", BIT, false),

        /**
         * 4-bit binary encoded decimal
         */
        DEC("[0-9]", QUADBIT, false),

        /**
         * 4-bit binary encoded hexadecimal
         */
        HEX("[a-fA-F0-9]", QUADBIT, false),

        /**
         * 8-bit binary encoded 1-byte UTF-8 character
         */
        UTF8("[\u0000-\u007F]", OCTET, false),

        /**
         *  4-bit binary encoded date-time coordinate
         */
        DATETIME("[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}Z", 56, true),

        /**
         *  4-bit binary encoded time duration
         */
        DURATION("P[0-9]{2}D[0-9]{2}H[0-9]{2}M", 24, true),

        /**
         *  4-bit binary encoded latitude coordinate
         */
        LAT("[+\\-][0-9]{2}\\.[0-9]{5}", 29, true),

        /**
         *  4-bit binary encoded longitude coordinate
         */
        LONG("[+\\-][0-9]{3}\\.[0-9]{5}", 33, true);

        /* PROPERTIES */

        /* The valid regex charset of an unencoded field value */
        private final String charset;
        private final int bitLength;
        private final Boolean fixedLength;

        /* METHODS */

        /* Constructor */
        /**
         * @param charset regex charset with allowed characters for this encoding
         * @param bitLength the number of bits required to encode a single character or value
         * @param fixedLength TRUE if the encoded field has a fixed bitlentgh, else FALSE
         */
        private Encoding(final String charset, final int bitLength, final Boolean fixedLength) {
            this.charset = charset;
            this.bitLength = bitLength;
            this.fixedLength = fixedLength;
        }

        /**
         * Returns the allowed charters for a given encoding in unencoded fields
         * @return a regex character set, e.g. [0-9]
         */
        public String charset() {
            return charset;
        }

        /**
         * Returns the bit length of a field for a given encoding and unencoded field byte length
         * @param byteLength the number of bytes in the unencoded field
         * @return the number of bits in a compressed encoded field
         */
        public int length(final int byteLength) {
            if (Boolean.TRUE.equals(fixedLength)) return bitLength;
            return (byteLength * bitLength);
        }
    }

    /* CONSTRUCTOR */

    /**
     * Constructs a new Whiteflag message field based on provided poperties
     * @param name the name of the Whiteflag field
     * @param pattern the regex pattern defining allowed values
     * @param encoding the encoding of the field
     * @param startByte the starting byte of the field in a serialized / uncompressed message
     * @param endByte the ending byte (not included) of the field in a serialized / uncompressed message
     */
    public WfMessageField(String name, String pattern, Encoding encoding, int startByte, int endByte) {
        this.name = name;
        this.pattern = Pattern.compile(pattern);
        this.encoding = encoding;
        this.startByte = startByte;
        this.endByte = endByte;
    }

    /**
     * Constructs a new Whiteflag message field from an existing message field, without copying the value
     * @param field the {@link WfMessageField} to copy
     */
    public WfMessageField(final WfMessageField field) {
        this(field, 0);
    }

    /**
     * Constructs a new Whiteflag message field from an existing message field, without copying the value
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
     * @return TRUE if the field has been set, else FALSE
     */
    public final Boolean isSet() {
        // Field is considered set if it contains a valid value
        return this.isValid();
    }

    /**
     * Checks if the message field contains a valid value
     * @return TRUE if the field contains a valid value, else FALSE
     */
    public final Boolean isValid() {
        return isValid(this.value);
    }

    /**
     * Checks if the provided data is a valid value for this field
     * @param data The data to be checked
     * @return TRUE if data is a valid value for this field
     */
    public final Boolean isValid(final String data) {
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
            return this.encoding.length(this.value.length());
        }
        return this.encoding.length(this.endByte - this.startByte);
    }

    /* PUBLIC METHODS: getters & setters */

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
        // Cannot set value twice
        if (Boolean.TRUE.equals(this.isSet())) return false;

        // Set if data is valid
        if (Boolean.TRUE.equals(isValid(data))) {
            this.value = data;
            return true;
        }
        return false;
    }

    /* PUBLIC METHODS: operations */

    /**
     * Encodes the message field into a binary string
     * @return the compressed binary encoding of the field
     * @throws WfCoreException if the field cannot be encoded
     */
    public final WfBinaryString encode() throws WfCoreException {
        // Check if field contains a valid value
        if (Boolean.FALSE.equals(this.isValid())) {
            throw new WfCoreException("Cannot encode " + this.name + debugString());
        }
        // Encode
        return WfMessageCodec.encode(value, encoding);
    }

    /**
     * Decodes the  the message field into a binary string
     * @param binData the compressed binary encoding of the field
     * @return the uncompressed value of the field
     * @throws WfCoreException if the field cannot be decoded
     */
    public final String decode(final WfBinaryString binData) throws WfCoreException {
        // Check number of bits in provided binary data
        int pad = 0;
        if (this.endByte > 0) {
            int nFieldBits = encoding.length(endByte - startByte);
            if (nFieldBits != binData.length()) {
                throw new WfCoreException("Encoded data is not exactly " + nFieldBits + " bits: " + binData.toHexString());
            }
        } else {
            pad = binData.length() % encoding.length(1);
        }

        // Decode
        String data = WfMessageCodec.decode(binData.sub(0, binData.length() - pad), encoding);
        if (Boolean.FALSE.equals(isValid(data))) return null;
        return data;
    }

    /* PROTECTED METHODS */

    /**
     * Gives debug information of the field
     * @return field name, value and pattern and validity check
     */
    protected String debugString() {
        return this.name + " field [\"" + this.value + "\", /" + this.pattern.toString() + "/, " + this.isValid() + "]";
    }
}
