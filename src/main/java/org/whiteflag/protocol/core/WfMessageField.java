/*
 * Whiteflag Java Library
 */
package org.whiteflag.protocol.core;

import java.util.regex.Pattern;
import java.nio.charset.StandardCharsets;

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

    /* Encoding parameters */
    private static final int HEXRADIX = WfBinaryString.HEXRADIX;
    private static final int BDXBITS = WfBinaryString.QUADBIT;
    private static final int UTFBITS = WfBinaryString.OCTET;

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
    public final WfMessageField.Encoding encoding;
    /**
     * The starting byte of the field in a serialized / uncompressed message
     */
    public final int startByte;
    /**
     * The ending byte (not included) of the field in a serialized / uncompressed message
     */
    public final int endByte;

    /* Main property */
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
        BIN("[01]", BDXBITS, false),

        /**
         * 4-bit binary encoded decimal
         */
        DEC("[0-9]", BDXBITS, false),

        /**
         * 4-bit binary encoded hexadecimal
         */
        HEX("[a-fA-F0-9]", BDXBITS, false),

        /**
         * 8-bit binary encoded 1-byte UTF-8 character
         */
        UTF8("[\u0000-\u007F]", UTFBITS, false),

        /**
         *  4-bit binary encoded date-time coordinate
         */
        DATETIME("[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}Z", 8, true),

        /**
         *  4-bit binary encoded time duration
         */
        DURATION("P[0-9]{2}D[0-9]{2}H[0-9]{2}M", 8, true),

        /**
         *  4-bit binary encoded latitude coordinate
         */
        LAT("[+\\-][0-9]{2}\\.[0-9]{5}", 8, true),

        /**
         *  4-bit binary encoded longitude coordinate
         */
        LONG("[+\\-][0-9]{3}\\.[0-9]{5}", 8, true);

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
        public int length(int byteLength) {
            if (fixedLength) return bitLength;
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
    public WfMessageField(String name, String pattern, WfMessageField.Encoding encoding, int startByte, int endByte) {
        this.name = name;
        this.pattern = Pattern.compile(pattern);
        this.encoding = encoding;
        this.startByte = startByte;
        this.endByte = endByte;
    }

    /**
     * Constructs a new Whiteflag message field from an existing message field
     * @param field the {@link WfMessageField} to create a new message field from
     */
    public WfMessageField(WfMessageField field) {
        this.name = field.name;
        this.pattern = Pattern.compile(field.pattern.toString());
        this.encoding = field.encoding;
        this.startByte = field.startByte;
        this.endByte = field.endByte;
        this.value = field.getValue();
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
     * Checks if the message field contains a valid value
     * @return TRUE if the field contains a valid value, else FALSE
     */
    public final Boolean isValid() {
        return isDataValid(this.value);
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
     * Sets the value of the message field
     * @param data The data representing the field value
     * @return TRUE if the data was valid and the field value is set, else FALSE
     */
    public final Boolean setValue(String data) {
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
        if (Boolean.TRUE.equals(!isDataValid(this.value))) {
            throw new WfCoreException(genericErrorMsg + ": " + value + " is not a valid Whiteflag " + encoding.toString() + " encoding");
        }

        // Build binary string iaw field encoding type
        StringBuilder bin = new StringBuilder();
        switch (encoding) {

            // Encode UTF 8 field
            case UTF8:
                for(byte b : value.getBytes(StandardCharsets.UTF_8)) {
                    bin.append(WfBinaryString.padLeft(Integer.toBinaryString(b & 0xff), UTFBITS));
                }
                break;

            // Encode binary field
            case BIN:
                bin = bin.append(value);
                break;

            // Encode decimal or hexadecimal field
            case DEC:
            case HEX:
                for(char c : value.toCharArray()) {
                    bin.append(WfBinaryString.padLeft(Integer.toBinaryString(Character.digit(c, HEXRADIX)), BDXBITS));
                }
                break;

            // Encode datum field
            case DATETIME:
            case DURATION:
            case LAT:
            case LONG:
                // Sign of lat long coordinates
                if (value.substring(0,1).equals("-")) bin.append("0");
                if (value.substring(0,1).equals("+")) bin.append("1");
                // Prepare string by removing fixed characters
                String dec = value.replaceAll("[\\-+:.A-Z]", "");
                // Run through characters of the string and convert to binary one by one
                for(char c : dec.toCharArray()) {
                    bin.append(WfBinaryString.padLeft(Integer.toBinaryString(Character.digit(c, HEXRADIX)), BDXBITS));
                }
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
     * @param data {@link WfBinaryString} the compressed binary encoding of the field
     * @return String with the uncompressed value of the field
     * @throws WfCoreException if the field cannot be decoded
     */
    public final String decode(WfBinaryString data) throws WfCoreException {
        // Standard encoding error message indiocating field name
        final String genericErrorMsg = "Cannot decode " + name + " field";

        // Get bineary string
        String bin = data.toBinString();

        // Check number of bits
        if (this.endByte > 0) {
            int binLength = encoding.length(endByte - startByte);
            if (binLength != bin.length()) {
                throw new WfCoreException(genericErrorMsg + ": " + "Encoded data is not exactly " + binLength + " bits: " + bin);
            }
        } else {
            //TODO: remove padding zeros at end
        }
        // Build value string iaw field encoding type
        StringBuilder value = new StringBuilder();
        switch (encoding) {

            // Encode UTF 8 field
            case UTF8:
                //TODO: UTF decoding
                break;

            // Decode binary field
            case BIN:
                value = value.append(bin);
                break;

            // Decode decimal or hexadecimal field
            case DEC:
            case HEX:
                //TODO: DEC/HEX decoding
                break;

            // Decode datetime field
            case DATETIME:
                //TODO: DATETIME decoding
                break;

            // Decode duration field
            case DURATION:
                //TODO: DURATION decoding
                break;
            
            // Decode lat-long fields
            case LAT:
            case LONG:
                // Sign of lat long coordinates
                if (bin.substring(0,1).equals("0")) value.append("-");
                if (bin.substring(0,1).equals("1")) value.append("+");
                //TODO: LAT-LONG decoding
                break;

            // Unknown encoding
            default:
                throw new WfCoreException(genericErrorMsg + ": " + "Undefined message encoding: " + encoding);
        }
        // Return decoded uncompressed value string
        return value.toString();
    }

    /* PRIVATE METHODS */

    /**
     * Checks if the provided data is a valid value for this field
     * @param data The data to be checked
     * @return Boolean indicating if data is a valid value for this field
     */
    private final Boolean isDataValid(String data) {
        if (data == null) return false;
        return this.pattern.matcher(data).matches();
    }
}
