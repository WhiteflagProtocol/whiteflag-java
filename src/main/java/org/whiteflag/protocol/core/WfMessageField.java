/*
 * Whiteflag Java Library
 */
package org.whiteflag.protocol.core;

import java.util.regex.Pattern;
import java.nio.charset.StandardCharsets;

/* Required encoding parameters */
import static org.whiteflag.protocol.core.WfBinaryString.BINRADIX;
import static org.whiteflag.protocol.core.WfBinaryString.HEXRADIX;
import static org.whiteflag.protocol.core.WfBinaryString.QUADBIT;
import static org.whiteflag.protocol.core.WfBinaryString.OCTET;
import static org.whiteflag.protocol.core.WfBinaryString.BIT;

/**
 * Whiteflag generic message field class
 * 
 * </p> This is a class defining a generic Whiteflag message field. Instances
 * of this class represent specific fields in specific message types with a
 * number of defined properties: name, allowed values, encoding, starting byte
 * and ending byte.
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
     * </p> Whiteflag fields use these compressed encodings, as defined for
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
         * @param charset String with regex charset with allowed characters for this encoding
         * @param bitLength integer the number of bits required to encode
         * @param fixedLength Boolean iondicating whether the encoded field has a fixed bitlentgh
         */
        private Encoding(final String charset, final int bitLength, final Boolean fixedLength) {
            this.charset = charset;
            this.bitLength = bitLength;
            this.fixedLength = fixedLength;
        }

        /**
         * Returns the allowed charters for a given encoding in unencoded fields
         * @return String with a regex character set, e.g. [0-9]
         */
        public String charset() {
            return charset;
        }

        /**
         * Returns the bit length of a field for a given encoding and unencoded field byte length
         * @param byteLength integer with the number of bytes in the unencoded field
         * @return integer with the number of bits in a compressed encoded field
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
     * @param pattern a string with the regex pattern defining allowed values
     * @param encoding the @{WfMessageField.Encoding} of the field
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
     * Constructs a new Whiteflag message field from an existing message field, without the value
     * @param field the {@link WfMessageField} to create a new message field from
     */
    public WfMessageField(final WfMessageField field) {
        this(field, 0);
    }

    /**
     * Constructs a new Whiteflag message field from an existing message field, without the value
     * @param field the {@link WfMessageField} to create a new message field from
     * @param shift integer how many bytes to shift the field as copying is mostly used to append
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
     * Returns the message field as a tring
     * @return String with the value of the message field
     */
    @Override
    public final String toString() {
        return this.getValue();
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
        return isDataValid(this.value);
    }

    /**
     * Checks if the provided data is a valid value for this field
     * @param data The data to be checked
     * @return Boolean indicating if data is a valid value for this field
     */
    public final Boolean isDataValid(final String data) {
        if (data == null) return false;
        return this.pattern.matcher(data).matches();
    }

    /**
     * Returns the byte length of the unencoded field value
     * @return integer with the byte length
     */
    public final int byteLength() {
        if (this.endByte < 0) {
            if (this.value == null) return 0;
            return this.value.length();
        }
        return (this.endByte - this.startByte);
    }

    /**
     * Returns the bit length of the encoded field
     * @return integer with the bit length
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
     * @return String with the field value
     */
    public final String getValue() {
        return this.value;
    }

    /**
     * Sets the value of the message field if not already set
     * @param data The data representing the field value
     * @return TRUE if field value is set, FALSE if field already set or data is invalid
     */
    public final Boolean setValue(final String data) {
        // Cannot set value twice
        if (Boolean.TRUE.equals(this.isSet())) return false;

        // Set if data is valid
        if (Boolean.TRUE.equals(isDataValid(data))) {
            this.value = data;
            return true;
        }
        return false;
    }

    /* PUBLIC METHODS: operations */

    /**
     * Encodes the message field into a binary string
     * @return {@link WfBinaryString} the compressed binary encoding of the field
     * @throws WfCoreException if the field cannot be encoded
     */
    public final WfBinaryString encode() throws WfCoreException {
        // Standard encoding error message indiocating field name
        final String genericErrorMsg = "Cannot encode " + name + " field";

        // Check if field contains a valid value
        if (Boolean.FALSE.equals(isDataValid(this.value))) {
            throw new WfCoreException(genericErrorMsg + ": " + value + " is not a valid Whiteflag " + encoding.toString() + " encoding");
        }

        // Build binary string iaw field encoding type
        StringBuilder bin = new StringBuilder();
        switch (encoding) {

            // Encode UTF 8 field
            case UTF8:
                bin.append(encodeUTF(value));
                break;

            // Encode binary field
            case BIN:
                bin.append(value);
                break;

            // Encode decimal or hexadecimal field
            case DEC:
            case HEX:
                bin.append(encodeBDX(value));
                break;

            // Encode datum field
            case DATETIME:
            case DURATION:
            case LAT:
            case LONG:
                // Sign of lat long coordinates
                if (value.substring(0,1).equals("-")) bin.append("0");
                if (value.substring(0,1).equals("+")) bin.append("1");

                // Encode string without fixed characters
                bin.append(encodeBDX(value.replaceAll("[\\-+:.A-Z]", "")));
                break;

            // Unknown encoding
            default:
                throw new WfCoreException(genericErrorMsg + ": " + "Undefined message encoding: " + encoding);
        }
        // Return binary string with compressed field encoding
        return new WfBinaryString(bin.toString());
    }

    /**
     * Decodes the  the message field into a binary string
     * @param binaryData {@link WfBinaryString} the compressed binary encoding of the field
     * @return String with the uncompressed value of the field
     * @throws WfCoreException if the field cannot be decoded
     */
    public final String decode(final WfBinaryString binaryData) throws WfCoreException {
        // Standard encoding error message indiocating field name
        final String genericErrorMsg = "Cannot decode " + name + " field";

        // Get bineary string
        String bin = binaryData.toBinString();

        // Check number of bits
        if (this.endByte > 0) {
            int nFieldBits = encoding.length(endByte - startByte);
            if (nFieldBits != bin.length()) {
                throw new WfCoreException(genericErrorMsg + ": " + "Encoded data is not exactly " + nFieldBits + " bits: " + bin);
            }
        } else {
            // Remove trailing zero's
            int pad = bin.length() % encoding.length(1);
            bin = bin.substring(0, bin.length() - pad);
        }
        // Build value string iaw field encoding type
        StringBuilder data = new StringBuilder();
        switch (encoding) {

            // Encode UTF 8 field
            case UTF8:
                data.append(decodeUTF(bin));
                break;

            // Decode binary field
            case BIN:
                // A binary string is already a binary string...
                data.append(bin);
                break;

            // Decode decimal or hexadecimal field
            case HEX:
            case DEC:
                data.append(decodeBDX(bin));
                break;

            // Decode datetime field
            case DATETIME:
                data.append(decodeBDX(bin));

                // Reinsert fixed characters
                data.insert(4, "-");
                data.insert(7, "-");
                data.insert(10, "T");
                data.insert(13, ":");
                data.insert(16, ":");
                data.insert(19, "Z");
                break;

            // Decode duration field
            case DURATION:
                data.append(decodeBDX(bin));

                // Reinsert fixed characters
                data.insert(0, "P");
                data.insert(3, "D");
                data.insert(6, "H");
                data.insert(9, "M");
                break;
            
            // Decode lat-long fields
            case LAT:
            case LONG:
                // Sign of lat long coordinates
                if (bin.substring(0,1).equals("0")) data.append("-");
                if (bin.substring(0,1).equals("1")) data.append("+");

                // Decode digits
                data.append(decodeBDX(bin.substring(1)));

                // Insert decimal dot
                data.insert(data.length() - 5, ".");
                break;

            // Unknown encoding
            default:
                throw new WfCoreException(genericErrorMsg + ": " + "Undefined message encoding: " + encoding);
        }
        // Return decoded uncompressed data string
        return data.toString();
    }

    /* STATIC METHODS */

    /**
     * Encodes a (hexa)decimal string into a binary string
     * @param data String with the (hexa)decimal data to encode
     * @return String representing of the binary encoding
     */
    public static final String encodeBDX(final String data) {
        StringBuilder bin = new StringBuilder();

        // Run through digits of the string and convert to binary string with leading zeros one by one
        for(char c : data.toCharArray()) {
            bin.append(WfBinaryString.padLeft(Integer.toBinaryString(Character.digit(c, HEXRADIX)), QUADBIT));
        }
        return bin.toString();
    }

    /**
     * Encodes UTF data into a binary string
     * @param data String with the UTF data to encode
     * @return String representing of the binary encoding
     */
    public static final String encodeUTF(final String data) {
        StringBuilder bin = new StringBuilder();

        // Run through bytes of the string and convert to binary string with leading zeros one by one
        for(byte b : data.getBytes(StandardCharsets.UTF_8)) {
            bin.append(WfBinaryString.padLeft(Integer.toBinaryString(b & 0xff), OCTET));
        }
        return bin.toString();
    }

    /**
     * Decodes a binary string into a (hexa)decimal string
     * @param bin the binary string to decode
     * @return String representing the decoded (hexa)decimal value
     */
    public static final String decodeBDX(final String bin) {
        StringBuilder data = new StringBuilder();
        for (int i = 0; i < bin.length(); i += QUADBIT) {
            // Parse 4 bits from the binary string back to the (hexa)decimal data, and convert that to char
            data.append(Integer.toHexString(Integer.parseUnsignedInt(bin.substring(i, i + QUADBIT), BINRADIX)));
        }
        return data.toString();
    }

    /**
     * Decodes a binary string into a UTF string
     * @param bin the binary string to decode
     * @return String with the decoded UTF data
     */
    public static final String decodeUTF(final String bin) {
        StringBuilder data = new StringBuilder();
        for (int i = 0; i < bin.length(); i += OCTET) {
            // Parse 8 UTF bits from the binary string to an integer, and cast that as a char
            data.append((char) Integer.parseUnsignedInt(bin.substring(i, i + OCTET), BINRADIX));
        }
        return data.toString();
    }

    /* PROTECTED METHODS */

    /**
     * Returns debug information
     * @return String with debug information
     */
    protected String debugString() {
        return this.name + " field [\"" + this.value + "\", /" + this.pattern.toString() + "/, " + this.isValid() + "]";
    }
}
