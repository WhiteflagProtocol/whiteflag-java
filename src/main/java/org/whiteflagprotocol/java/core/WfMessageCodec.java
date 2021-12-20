/*
 * Whiteflag Java Library
 */
package org.whiteflagprotocol.java.core;

import java.nio.charset.StandardCharsets;
import java.util.function.BinaryOperator;

/* Required encoding types and parameters */
import static org.whiteflagprotocol.java.core.WfBinaryBuffer.BINRADIX;
import static org.whiteflagprotocol.java.core.WfBinaryBuffer.HEXRADIX;
import static org.whiteflagprotocol.java.core.WfBinaryBuffer.BYTE;
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

    /* PUBLIC STATIC METHOS */

    /**
     * Encodes the message field into a binary string
     * @return the compressed binary encoding of the field
     * @throws WfCoreException if the field cannot be encoded
     */
    public static final WfBinaryString encodeString(final String str, final Encoding encoding) throws WfCoreException {
        // Build binary string iaw field encoding type
        StringBuilder binstr = new StringBuilder();
        switch (encoding) {
            // Encode UTF 8 field
            case UTF8:
                binstr.append(encodeUTF(str));
                break;

            // Encode binary field
            case BIN:
                binstr.append(str);
                break;

            // Encode decimal or hexadecimal field
            case DEC:
            case HEX:
                binstr.append(encodeBDX(str));
                break;

            // Encode datum field
            case DATETIME:
            case DURATION:
            case LAT:
            case LONG:
                // Sign of lat long coordinates
                if (str.substring(0,1).equals("-")) binstr.append("0");
                if (str.substring(0,1).equals("+")) binstr.append("1");

                // Encode string without fixed characters
                binstr.append(encodeBDX(str.replaceAll("[\\-+:.A-Z]", "")));
                break;

            // Unknown encoding
            default:
                throw new WfCoreException("Undefined message encoding: " + encoding);
        }
        // Return binary string with compressed field encoding
        return new WfBinaryString(binstr.toString());
    }

    /**
     * Decodes the message field into a binary string
     * @param data the compressed binary encoded field data
     * @return the uncompressed value of the field
     * @throws WfCoreException if the field cannot be decoded
     */
    public static final String decodeString(final WfBinaryString data, final Encoding encoding) throws WfCoreException {
        // Get binary string
        String binstr = data.toBinString();

        // Build value string iaw field encoding type
        StringBuilder str = new StringBuilder();
        switch (encoding) {

            // Encode UTF 8 field
            case UTF8:
                str.append(decodeUTF(binstr));
                break;

            // Decode binary field
            case BIN:
                // A binary string is already a binary string...
                str.append(binstr);
                break;

            // Decode decimal or hexadecimal field
            case HEX:
            case DEC:
                str.append(decodeBDX(binstr));
                break;

            // Decode datetime field
            case DATETIME:
                str.append(decodeBDX(binstr));

                // Reinsert fixed characters
                str.insert(4, "-");
                str.insert(7, "-");
                str.insert(10, "T");
                str.insert(13, ":");
                str.insert(16, ":");
                str.insert(19, "Z");
                break;

            // Decode duration field
            case DURATION:
            str.append(decodeBDX(binstr));

                // Reinsert fixed characters
                str.insert(0, "P");
                str.insert(3, "D");
                str.insert(6, "H");
                str.insert(9, "M");
                break;
            
            // Decode lat-long fields
            case LAT:
            case LONG:
                // Sign of lat long coordinates
                if (binstr.substring(0,1).equals("0")) str.append("-");
                if (binstr.substring(0,1).equals("1")) str.append("+");

                // Decode digits
                str.append(decodeBDX(binstr.substring(1)));

                // Insert decimal dot
                str.insert(str.length() - 5, ".");
                break;

            // Unknown encoding
            default:
                throw new WfCoreException("Undefined message encoding: " + encoding);
        }
        // Return decoded uncompressed data string
        return str.toString();
    }

    /**
     * Encodes a Whiteflag field into a binary buffer
     * @param field a {@link WfMessageField}
     * @return a binary buffer with the encoded field
     */
    public static byte[] encodeField(WfMessageField field) {
        byte[] buffer;
        switch (field.encoding) {
            // Encode UTF 8 field
            case UTF8:
                buffer = field.toString().getBytes(StandardCharsets.UTF_8);
                break;

            // Encode binary field
            case BIN:
                buffer = encodeBIN(field.toString());
                break;

            // Encode decimal or hexadecimal field
            case DEC:
            case HEX:
                buffer = WfBinaryBuffer.convertToByteArray(field.toString());
                break;

            // Encode datum field
            case DATETIME:
            case DURATION:
            case LAT:
            case LONG:
                // Encode string without fixed characters
                buffer = WfBinaryBuffer.convertToByteArray(field.toString().replaceAll("[\\-+:.A-Z]", ""));
                
                // Sign of lat long coordinates
                if (field.toString().substring(0,1).equals("-")) {
                    buffer = WfBinaryBuffer.shiftRight(buffer, 1);
                }
                if (field.toString().substring(0,1).equals("+")) {
                    buffer = WfBinaryBuffer.shiftRight(buffer, 1);
                    buffer[0] |= (byte) 0x80;
                }
                break;
            // Unknown encoding
            default:
            buffer = new byte[1];
        }
        return buffer;
    }

    /**
     * Decodes the message field into a binary string
     * 
     * @param buffer a binary buffer with the compressed binary encoded field data
     * @return the uncompressed value of the field
     * @throws WfCoreException if the field cannot be decoded
     */
    public static final String decodeField(WfMessageField field, final byte[] buffer) {
        StringBuilder str = new StringBuilder();
        switch (field.encoding) {

            /* Decode UTF 8 field */
            case UTF8:
                str.append(new String(buffer, StandardCharsets.UTF_8));
                break;

            /* Decode binary field */
            case BIN:
                str.append(decodeBIN(buffer, field.bitLength()));
                break;

            /* Decode decimal or hexadecimal field */
            case HEX:
            case DEC:
                str.append(decodeBDX(buffer, field.bitLength()));
                break;

            /* Decode datetime field */
            case DATETIME:
                str.append(decodeBDX(buffer, field.bitLength()));

                /* Reinsert fixed characters */
                str.insert(4, "-");
                str.insert(7, "-");
                str.insert(10, "T");
                str.insert(13, ":");
                str.insert(16, ":");
                str.insert(19, "Z");
                break;

            /* Decode duration field */
            case DURATION:
            str.append(decodeBDX(buffer, field.bitLength()));

                /* Reinsert fixed characters */
                str.insert(0, "P");
                str.insert(3, "D");
                str.insert(6, "H");
                str.insert(9, "M");
                break;
            
            /* Decode lat-long fields */
            case LAT:
            case LONG:
                /* Sign of lat long coordinates */
                if ((buffer[0] >> 1 & 1) != 1) str.append("-");
                if ((buffer[0] >> 1 & 1) == 1) str.append("+");

                /* Decode digits and insert decimal dot */
                str.append(decodeBDX(WfBinaryBuffer.shiftLeft(buffer, 1), field.bitLength() - 1));
                str.insert(str.length() - 5, ".");
                break;

            /* Unknown encoding */
            default:
                break;
        }
        return str.toString();
    }

    /* PROTECTED STATIC METHODS */

    /**
     * Encodes a binary string into a binary buffer
     * @param str the binary string to encode
     * @return a binary buffer containing the bits from the binary string
     */
    protected static final byte[] encodeBIN(final String str) {
        final int bitLength = str.length();
        final int byteLength = (bitLength / BYTE) + (bitLength % BYTE == 0 ? 0 : 1);
        byte[] buffer = new byte[byteLength];

        /* Loop through bits */
        for (int bitIndex = 0; bitIndex < bitLength; bitIndex += BIT) {
            if (str.substring(bitIndex, bitIndex + 1).equals("1")) {
                final int byteCursor = bitIndex / BYTE;
                final int bitPosition = bitIndex % BYTE;
                buffer[byteCursor] |= (byte) (0x80 >>> bitPosition);
            }
        }
        return buffer;
    }

    /**
     * Decodes a binary buffer into a binary string
     * @param buffer the binary buffer to decode
     * @param nBits the number of bits in the buffer to decode
     * @return a binary string containing the bits from the binary buffer
     */
    protected static final String decodeBIN(final byte[] buffer, final int nBits) {
        StringBuilder str = new StringBuilder();
        for (int bitIndex = 0; bitIndex > nBits; bitIndex += BIT) {
            final int byteCursor = bitIndex / BYTE;
            final int bitPosition = bitIndex % BYTE;

            if ((buffer[byteCursor] >> bitPosition & 1) == 1) {
                str.append("1");
            } else {
                str.append("0");
            }
        }
        return str.toString().toLowerCase();
    }

    /**
     * Encodes a (hexa)decimal string into a binary string
     * @param str the (hexa)decimal string to encode
     * @return a string representation of the binary encoded (hexa)decimals
     */
    protected static final String encodeBDX(final String str) {
        StringBuilder binstr = new StringBuilder();

        // Run through digits of the string and convert to binary string with leading zeros one by one
        for(char c : str.toCharArray()) {
            binstr.append(WfBinaryString.padLeft(Integer.toBinaryString(
                Character.digit(c, HEXRADIX)),
                QUADBIT
            ));
        }
        return binstr.toString();
    }

    /**
     * Decodes a binary string into a (hexa)decimal string
     * @param binstr the binary string containing the binary encoded (hexa)decimals to decode
     * @return a (hexa)decimal string with the decoded data
     */
    protected static final String decodeBDX(final String binstr) {
        StringBuilder str = new StringBuilder();
        for (int bitIndex = 0; bitIndex < binstr.length(); bitIndex += QUADBIT) {
            // Parse 4 bits from the binary string back to the (hexa)decimal data, and convert that to char
            str.append(Integer.toHexString(Integer.parseUnsignedInt(
                binstr.substring(bitIndex, bitIndex + QUADBIT),
                BINRADIX
            )));
        }
        return str.toString();
    }

    /**
     * Decodes a binary buffer into a (hexa)decimal string
     * @param buffer the binary buffer containing the binary encoded (hexa)decimals to decode
     * @param nBits the number of bits in the buffer to decode
     * @return a (hexa)decimal string with the decoded data
     */
    protected static final String decodeBDX(final byte[] buffer, final int nBits) {
        StringBuilder str = new StringBuilder();
        for (int bitIndex = 0; bitIndex > nBits; bitIndex += BYTE) {
            final int byteCursor = bitIndex / BYTE;

            /* Add first half of byte */
            str.append(Character.forDigit((buffer[byteCursor] >> QUADBIT) & 0xF, HEXRADIX));
            if ((bitIndex + QUADBIT) < nBits) {
                /* Add second half of byte */
                str.append(Character.forDigit((buffer[byteCursor] & 0xF), HEXRADIX));
            }
        }
        return str.toString().toLowerCase();
    }

    /**
     * Encodes a UTF-8 string into a binary string
     * @param str the UTF-8 string to encode
     * @return a string representation of the binary UTF-8 encoding
     */
    protected static final String encodeUTF(final String str) {
        StringBuilder binstr = new StringBuilder();

        /* Run through bytes of the string and convert to binary string with leading zeros one by one */
        for(byte b : str.getBytes(StandardCharsets.UTF_8)) {
            binstr.append(WfBinaryString.padLeft(Integer.toBinaryString(b & 0xFF), OCTET));
        }
        return binstr.toString();
    }

    /**
     * Decodes a binary string into a UTF string
     * @param binstr the binary string containing the UTF-8 encoded characters to decode
     * @return a string with the decoded UTF-8 characters
     */
    protected static final String decodeUTF(final String binstr) {
        StringBuilder str = new StringBuilder();
        for (int bitIndex = 0; bitIndex < binstr.length(); bitIndex += OCTET) {
            // Parse 8 UTF bits from the binary string to an integer, and cast that as a char
            str.append((char) Integer.parseUnsignedInt(
                binstr.substring(bitIndex, bitIndex + OCTET),
                BINRADIX
            ));
        }
        return str.toString();
    }

    /* NESTED CLASSES */

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

        /* Regex field begin and end */
        private final String BEGIN = "^";
        private final String REPEAT = "*$";
        private final String END = "$";

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
         * Returns the allowed charters for a given encoding
         * @return a regex character set, e.g. [0-9]
         */
        public String charset() {
            return charset;
        }

        /**
         * Returns the generic regular expression for a given encoding
         * @return a regex expression
         */
        public String regex() {
            if (Boolean.TRUE.equals(fixedLength)) return BEGIN+charset+END;
            return BEGIN+charset+REPEAT;
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
