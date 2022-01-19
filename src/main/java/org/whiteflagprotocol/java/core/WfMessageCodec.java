/*
 * Whiteflag Java Library
 */
package org.whiteflagprotocol.java.core;

import java.nio.charset.StandardCharsets;

/* Required encoding types and parameters */
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
 * 
 * @since 1.0
 */
public final class WfMessageCodec {

    /* CONSTRUCTOR */

    /** 
     * Prevents the utility class to be instantiated
     */
    private WfMessageCodec() {
        throw new IllegalStateException("Cannot instantiate Whiteflag codec utility class");
    }

    /* PUBLIC STATIC METHOS */

    /**
     * Encodes a Whiteflag field into a binary buffer
     * @since 1.1
     * @param field a {@link WfMessageField}
     * @return a binary buffer with the encoded field
     */
    public static final byte[] encodeField(WfMessageField field) {
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
                buffer = encodeBDX(field.toString());
                break;

            // Encode datum field
            case DATETIME:
            case DURATION:
                // Encode string without fixed characters
                buffer = encodeBDX(field.toString().replaceAll("[\\-+:.A-Z]", ""));
                break;
            case LAT:
            case LONG:
                buffer = encodeLatLong(field.toString());
                break;
            // Unknown encoding
            default:
                buffer = new byte[0];
                break;
        }
        return buffer;
    }

    /**
     * Sets the field value from a binary buffer
     * @since 1.1
     * @param field the field for which to decode the binary value
     * @param buffer a binary buffer with the compressed binary encoded field data
     * @return the uncompressed value of the field
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
                if (((buffer[0] >>> (BYTE - 1)) & 1) == 1) {
                    str.append("+");
                } else {
                    str.append("-");
                }
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
     * @since 1.1
     * @param binstr the binary string to encode
     * @return a binary buffer containing the bits from the binary string
     */
    protected static final byte[] encodeBIN(final String binstr) {
        final int bitLength = binstr.length();
        final int byteLength = (bitLength / BYTE) + (bitLength % BYTE == 0 ? 0 : 1);
        byte[] buffer = new byte[byteLength];

        /* Loop through bits of the binary string */
        for (int bitIndex = 0; bitIndex < bitLength; bitIndex += BIT) {
            if (binstr.substring(bitIndex, bitIndex + 1).equals("1")) {
                final int byteCursor = bitIndex / BYTE;
                final int bitPosition = bitIndex % BYTE;
                buffer[byteCursor] |= (byte) (0x80 >>> bitPosition);
            }
        }
        return buffer;
    }

    /**
     * Decodes a binary buffer into a binary string
     * @since 1.1
     * @param buffer the binary buffer to decode
     * @param bitLength the buffer length, i.e. the number of bits in the buffer to decode
     * @return a binary string containing the bits from the binary buffer
     */
    protected static final String decodeBIN(final byte[] buffer, final int bitLength) {
        StringBuilder str = new StringBuilder();

        /* Loop strough bits of binary buffer */
        for (int bitIndex = 0; bitIndex < bitLength; bitIndex += BIT) {
            final int byteCursor = bitIndex / BYTE;
            final int bitPosition = bitIndex % BYTE;
            if ((buffer[byteCursor] >>> (BYTE - bitPosition - 1) & 1) == 1) {
                str.append("1");
            } else {
                str.append("0");
            }
        }
        return str.toString().toLowerCase();
    }

    /**
     * Encodes a (hexa)decimal string into a binary buffer
     * @since 1.1
     * @param bdxstr the (hexa)decimal string to encode
     * @return a binary buffer containing the encoded (hexa)decimal string
     */
    protected static final byte[] encodeBDX(final String bdxstr) {
        /* Prepare string by removing prefix and adding trailing 0 */
        String str = bdxstr;
        if (bdxstr.length() % 2 == 1) str = str + "0";

        /* Loop through hexadecimal string and take two chars at a time*/
        final int strLength = str.length();
        byte[] byteArray = new byte[strLength / 2];
        for (int i = 0; i < strLength; i += 2) {
            byteArray[i / 2] = (byte) ((Character.digit(str.charAt(i), HEXRADIX) << QUADBIT)
                                      + Character.digit(str.charAt(i + 1), HEXRADIX));
        }
        return byteArray;
    }

    /**
     * Decodes a binary buffer into a (hexa)decimal string
     * @since 1.1
     * @param buffer the binary buffer containing the binary encoded (hexa)decimals to decode
     * @param bitLength the buffer length, i.e. the number of bits in the buffer to decode
     * @return a (hexa)decimal string with the decoded data
     */
    protected static final String decodeBDX(final byte[] buffer, final int bitLength) {
        StringBuilder str = new StringBuilder();

        /* Loop through the bits in the binary buffer */
        for (int bitIndex = 0; bitIndex < bitLength; bitIndex += BYTE) {
            final int byteCursor = bitIndex / BYTE;

            /* Add first half of byte to string */
            str.append(Character.forDigit((buffer[byteCursor] >> QUADBIT) & 0xF, HEXRADIX));
            if ((bitIndex + QUADBIT) < bitLength) {
                /* Add second half of byte to string */
                str.append(Character.forDigit((buffer[byteCursor] & 0xF), HEXRADIX));
            }
        }
        return str.toString().toLowerCase();
    }

    /**
     * Encodes a datum string into binary buffer
     * @since 1.1
     * @param datumstr the datum string to encode
     * @return a binary buffer containing the encoded datum
     */
    protected static final byte[] encodeLatLong(final String datumstr) {
        // Encode string without fixed characters
        final String str = datumstr.replaceAll("[\\-+:.A-Z]", "");
        byte[] byteArray = encodeBDX(str); 
        
        // Sign of lat long coordinates
        if (datumstr.substring(0,1).equals("-")) {
            byteArray = WfBinaryBuffer.shiftRight(byteArray, 1);
        }
        if (datumstr.substring(0,1).equals("+")) {
            byteArray = WfBinaryBuffer.shiftRight(byteArray, 1);
            byteArray[0] |= (byte) 0x80;
        }
        // Return byte array without excess bits at end
        final int bitLength = 1 + str.length() * QUADBIT;
        return WfBinaryBuffer.cropBits(byteArray, bitLength);
    }

    /* NESTED CLASSES */

    /**
     * Whiteflag message compressed field encodings
     * 
     * <p> Whiteflag fields use these compressed encodings, as defined for
     * each message field in the Whiteflag specification.
     * 
     * @since 1.1
     */
    @SuppressWarnings("java:S116")
    public enum Encoding {

        /**
         * 1-bit binary encoded bit
         */
        BIN("[01]", BIT, -1),

        /**
         * 4-bit binary encoded decimal
         */
        DEC("[0-9]", QUADBIT, -1),

        /**
         * 4-bit binary encoded hexadecimal
         */
        HEX("[a-fA-F0-9]", QUADBIT, -1),

        /**
         * 8-bit binary encoded 1-byte UTF-8 character
         */
        UTF8("[\u0000-\u007F]", OCTET, -1),

        /**
         *  4-bit binary encoded date-time coordinate
         */
        DATETIME("[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}Z", 56, 20),

        /**
         *  4-bit binary encoded time duration
         */
        DURATION("P[0-9]{2}D[0-9]{2}H[0-9]{2}M", 24, 10),

        /**
         *  4-bit binary encoded latitude coordinate
         */
        LAT("[+\\-][0-9]{2}\\.[0-9]{5}", 29, 9),

        /**
         *  4-bit binary encoded longitude coordinate
         */
        LONG("[+\\-][0-9]{3}\\.[0-9]{5}", 33, 10);

        /* PROPERTIES */

        /* Regex field begin and end */
        private final String BEGIN = "^";
        private final String REPEAT = "*$";
        private final String END = "$";

        /* The valid regex charset of an unencoded field value */
        private final String charset;
        private final int bitLength;
        private final int byteLength;
        private final Boolean fixedLength;

        /* METHODS */

        /* Constructor */
        /**
         * @param charset regex charset with allowed characters for this encoding
         * @param bitLength the number of bits required to encode a single character or value
         * @param byteLength the nmber of bytes in an unencoded fixed length field; negative for non-fixed length encoding
         */
        private Encoding(final String charset, final int bitLength, final int byteLength) {
            this.charset = charset;
            this.bitLength = bitLength;
            this.byteLength = byteLength;
            if (byteLength < 0) this.fixedLength = false;
                else this.fixedLength = true;
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
         * Checks if this encoding requires a fixed field length
         * @return TRUE if the encoding requires a fixed field length
         */
        public Boolean isFixedLength() {
            return this.fixedLength;
        }

        /**
         * Returns the byte length for a fixed length unencoded field, else -1
         * @return the byte length for a fixed length unencoded field, or -1 for variable length fields
         */
        public int byteLength() {
            return this.byteLength;
        }

        /**
         * Returns the bit length the totol number of bits for a fixed length field, else the bits to encode a single character
         * @return the bit length for a fixed length encoded field, or the bits to encode a single character of a 
         */
        public int bitLength() {
            return this.bitLength;
        }

        /**
         * Returns the bit length of a field for a given encoding and unencoded field byte length
         * @param byteLength the number of bytes in the unencoded field
         * @return the number of bits in a compressed encoded field
         */
        public int bitLength(final int byteLength) {
            if (Boolean.TRUE.equals(fixedLength)) return this.bitLength;
            return (byteLength * this.bitLength);
        }
    }
}
