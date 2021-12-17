/*
 * Whiteflag Java Library
 */
package org.whiteflagprotocol.java.core;

import java.nio.charset.StandardCharsets;

/* Required encoding types and parameters */
import static org.whiteflagprotocol.java.core.WfBinaryBuffer.BINRADIX;
import static org.whiteflagprotocol.java.core.WfBinaryBuffer.HEXRADIX;
import static org.whiteflagprotocol.java.core.WfBinaryBuffer.QUADBIT;
import static org.whiteflagprotocol.java.core.WfBinaryBuffer.OCTET;
import static org.whiteflagprotocol.java.core.WfBinaryBuffer.BIT;

/**
 * Whiteflag codec utility class
 * 
 * <p> This is a non-instantiatable utility class with the Whiteflag message
 * field encoding and decoding functions, all implemented as static functions.
 * No implementation specific properties and methods are defined by this class.
 * 
 * @wfver v1-draft.6
 * 
 * @wfref 4 Message Format
 * @wfref 4.1.2 Encoding
 * @wfref 4.1.3 Compression
 */
public class WfMessageCodec {

    /* CONSTRUCTOR */

    /** 
     * Prevents the utility class to be instantiated
     */
    private WfMessageCodec() {
        throw new IllegalStateException("Cannot instantiate Whiteflag codec utility class");
    }

    /**
     * Encodes the message field into a binary string
     * @return the compressed binary encoding of the field
     * @throws WfCoreException if the field cannot be encoded
     */
    public static final WfBinaryString encodeString(final String value, final Encoding encoding) throws WfCoreException {
        // Build binary string iaw field encoding type
        StringBuilder binStr = new StringBuilder();
        switch (encoding) {
            // Encode UTF 8 field
            case UTF8:
                binStr.append(encodeUTF(value));
                break;

            // Encode binary field
            case BIN:
                binStr.append(value);
                break;

            // Encode decimal or hexadecimal field
            case DEC:
            case HEX:
                binStr.append(encodeBDX(value));
                break;

            // Encode datum field
            case DATETIME:
            case DURATION:
            case LAT:
            case LONG:
                // Sign of lat long coordinates
                if (value.substring(0,1).equals("-")) binStr.append("0");
                if (value.substring(0,1).equals("+")) binStr.append("1");

                // Encode string without fixed characters
                binStr.append(encodeBDX(value.replaceAll("[\\-+:.A-Z]", "")));
                break;

            // Unknown encoding
            default:
                throw new WfCoreException("Undefined message encoding: " + encoding);
        }
        // Return binary string with compressed field encoding
        return new WfBinaryString(binStr.toString());
    }

    /**
     * Decodes the message field into a binary string
     * @param binData the compressed binary encoded field data
     * @return the uncompressed value of the field
     * @throws WfCoreException if the field cannot be decoded
     */
    public static final String decodeString(final WfBinaryString binData, final Encoding encoding) throws WfCoreException {
        // Get binary string
        String binStr = binData.toBinString();

        // Build value string iaw field encoding type
        StringBuilder data = new StringBuilder();
        switch (encoding) {

            // Encode UTF 8 field
            case UTF8:
                data.append(decodeUTF(binStr));
                break;

            // Decode binary field
            case BIN:
                // A binary string is already a binary string...
                data.append(binStr);
                break;

            // Decode decimal or hexadecimal field
            case HEX:
            case DEC:
                data.append(decodeBDX(binStr));
                break;

            // Decode datetime field
            case DATETIME:
                data.append(decodeBDX(binStr));

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
                data.append(decodeBDX(binStr));

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
                if (binStr.substring(0,1).equals("0")) data.append("-");
                if (binStr.substring(0,1).equals("1")) data.append("+");

                // Decode digits
                data.append(decodeBDX(binStr.substring(1)));

                // Insert decimal dot
                data.insert(data.length() - 5, ".");
                break;

            // Unknown encoding
            default:
                throw new WfCoreException("Undefined message encoding: " + encoding);
        }
        // Return decoded uncompressed data string
        return data.toString();
    }

    /**
     * Encodes a Whiteflag field into a byte array
     * @param field a {@link WfMessageField}
     * @return a byte array with the encooded field
     */
    public static byte[] encodeField(WfMessageField field) {
        switch (field.encoding) {
            // Encode UTF 8 field
            case UTF8:
                return field.toString().getBytes(StandardCharsets.UTF_8);

            // Encode binary field
            case BIN:
                byte[] bin = {(byte) 0x00};
                if (field.toString().equals("1")) bin[0] = (byte) 0x80;
                return bin;

            // Encode decimal or hexadecimal field
            case DEC:
            case HEX:
                return WfBinaryBuffer.convertToByteArray(field.toString());

            // Encode datum field
            case DATETIME:
            case DURATION:
            case LAT:
            case LONG:
                // Encode string without fixed characters
                byte[] datum = WfBinaryBuffer.convertToByteArray(field.toString().replaceAll("[\\-+:.A-Z]", ""));
                
                // Sign of lat long coordinates
                if (field.toString().substring(0,1).equals("-")) {
                    datum = WfBinaryBuffer.shiftRight(datum, 1);
                }
                if (field.toString().substring(0,1).equals("+")) {
                    datum = WfBinaryBuffer.shiftRight(datum, 1);
                    datum[0] |= (byte) 0x80;
                }
                return datum;
            // Unknown encoding
            default:
                return new byte[0];
        }
    }

    /**
     * Decodes the message field into a binary string
     * @param binData a byte array with the compressed binary encoded field data
     * @param nBits the number of bits 
     * @return the uncompressed value of the field
     * @throws WfCoreException if the field cannot be decoded
     */
    public static final String decodeField(final byte[] binData, final int nBits, final Encoding encoding) {
        StringBuilder fieldStrBuilder = new StringBuilder();
        switch (encoding) {

            /* Decode UTF 8 field */
            case UTF8:
                fieldStrBuilder.append(new String(binData, StandardCharsets.UTF_8));
                break;

            /* Decode binary field */
            case BIN:
            if ((binData[0] >> 1 & 1) == 1) {
                fieldStrBuilder.append("1");
            } else {
                fieldStrBuilder.append("0");
            }
            break;

            /* Decode decimal or hexadecimal field */
            case HEX:
            case DEC:
                fieldStrBuilder.append(decodeBDX(binData, nBits));    
                break;

            /* Decode datetime field */
            case DATETIME:
                fieldStrBuilder.append(decodeBDX(binData, nBits));

                /* Reinsert fixed characters */
                fieldStrBuilder.insert(4, "-");
                fieldStrBuilder.insert(7, "-");
                fieldStrBuilder.insert(10, "T");
                fieldStrBuilder.insert(13, ":");
                fieldStrBuilder.insert(16, ":");
                fieldStrBuilder.insert(19, "Z");
                break;

            /* Decode duration field */
            case DURATION:
                fieldStrBuilder.append(decodeBDX(binData, nBits));

                /* Reinsert fixed characters */
                fieldStrBuilder.insert(0, "P");
                fieldStrBuilder.insert(3, "D");
                fieldStrBuilder.insert(6, "H");
                fieldStrBuilder.insert(9, "M");
                break;
            
            /* Decode lat-long fields */
            case LAT:
            case LONG:
                /* Sign of lat long coordinates */
                if ((binData[0] >> 1 & 1) != 1) fieldStrBuilder.append("-");
                if ((binData[0] >> 1 & 1) == 1) fieldStrBuilder.append("+");

                /* Decode digits and insert decimal dot */
                fieldStrBuilder.append(decodeBDX(WfBinaryBuffer.shiftLeft(binData, 1), nBits - 1));
                fieldStrBuilder.insert(fieldStrBuilder.length() - 5, ".");
                break;

            /* Unknown encoding */
            default:
                return null;
        }
        return fieldStrBuilder.toString();
    }

    /* PROTECTED STATIC METHODS */

    /**
     * Encodes a (hexa)decimal string into a binary string
     * @param value the (hexa)decimal value to encode
     * @return string representation of the binary encoding
     */
    protected static final String encodeBDX(final String value) {
        StringBuilder binStr = new StringBuilder();

        // Run through digits of the string and convert to binary string with leading zeros one by one
        for(char c : value.toCharArray()) {
            binStr.append(WfBinaryString.padLeft(Integer.toBinaryString(Character.digit(c, HEXRADIX)), QUADBIT));
        }
        return binStr.toString();
    }

    /**
     * Encodes UTF value into a binary string
     * @param value the UTF value to encode
     * @return string representation of the binary encoding
     */
    protected static final String encodeUTF(final String value) {
        StringBuilder binStr = new StringBuilder();

        // Run through bytes of the string and convert to binary string with leading zeros one by one
        for(byte b : value.getBytes(StandardCharsets.UTF_8)) {
            binStr.append(WfBinaryString.padLeft(Integer.toBinaryString(b & 0xff), OCTET));
        }
        return binStr.toString();
    }

    /**
     * Decodes a binary string into a UTF string
     * @param binStr the binary string to decode
     * @return the decoded UTF data
     */
    protected static final String decodeUTF(final String binStr) {
        StringBuilder data = new StringBuilder();
        for (int i = 0; i < binStr.length(); i += OCTET) {
            // Parse 8 UTF bits from the binary string to an integer, and cast that as a char
            data.append((char) Integer.parseUnsignedInt(binStr.substring(i, i + OCTET), BINRADIX));
        }
        return data.toString();
    }

    /**
     * Decodes a binary string into a (hexa)decimal string
     * @param binStr the binary string to decode
     * @return the decoded (hexa)decimal value
     */
    protected static final String decodeBDX(final String binStr) {
        StringBuilder data = new StringBuilder();
        for (int i = 0; i < binStr.length(); i += QUADBIT) {
            // Parse 4 bits from the binary string back to the (hexa)decimal data, and convert that to char
            data.append(Integer.toHexString(Integer.parseUnsignedInt(binStr.substring(i, i + QUADBIT), BINRADIX)));
        }
        return data.toString();
    }

    /**
     * Converts bits into a (hexa)decimal string
     * @param byteArray the byte array containing the bits to decode
     * @param nBits the number of bits in the bitset
     * @return the decoded (hexa)decimal value
     */
    protected static final String decodeBDX(final byte[] byteArray, final int nBits) {
        StringBuilder hexStrBuilder = new StringBuilder();
        int bitIndex = 0;
        for (int i = 0; i < byteArray.length; i++) {
            /* Check if all bits have already been processed */
            if (bitIndex > nBits) break;

            /* Add first half of byte */
            hexStrBuilder.append(Character.forDigit((byteArray[i] >> QUADBIT) & 0xF, HEXRADIX));
            bitIndex += QUADBIT;
            if (bitIndex < nBits) {
                /* Add second half of byte */
                hexStrBuilder.append(Character.forDigit((byteArray[i] & 0xF), HEXRADIX));
                bitIndex += QUADBIT;
            }
        }
        return hexStrBuilder.toString().toLowerCase();
    }

    /* ENUM */

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
}
