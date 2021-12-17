/*
 * Whiteflag Java Library
 */
package org.whiteflagprotocol.java.core;

import java.nio.charset.StandardCharsets;

/* Required encoding types and parameters */
import static org.whiteflagprotocol.java.core.WfBinaryString.BINRADIX;
import static org.whiteflagprotocol.java.core.WfBinaryString.HEXRADIX;
import static org.whiteflagprotocol.java.core.WfBinaryString.QUADBIT;
import static org.whiteflagprotocol.java.core.WfBinaryString.OCTET;

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
    public static final WfBinaryString encode(final String value, final WfMessageField.Encoding encoding) throws WfCoreException {
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
    public static final String decode(final WfBinaryString binData, final WfMessageField.Encoding encoding) throws WfCoreException {
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

    /* STATIC METHODS */

    /**
     * Encodes a (hexa)decimal string into a binary string
     * @param value the (hexa)decimal value to encode
     * @return string representation of the binary encoding
     */
    public static final String encodeBDX(final String value) {
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
    public static final String encodeUTF(final String value) {
        StringBuilder binStr = new StringBuilder();

        // Run through bytes of the string and convert to binary string with leading zeros one by one
        for(byte b : value.getBytes(StandardCharsets.UTF_8)) {
            binStr.append(WfBinaryString.padLeft(Integer.toBinaryString(b & 0xff), OCTET));
        }
        return binStr.toString();
    }

    /**
     * Decodes a binary string into a (hexa)decimal string
     * @param binStr the binary string to decode
     * @return the decoded (hexa)decimal value
     */
    public static final String decodeBDX(final String binStr) {
        StringBuilder data = new StringBuilder();
        for (int i = 0; i < binStr.length(); i += QUADBIT) {
            // Parse 4 bits from the binary string back to the (hexa)decimal data, and convert that to char
            data.append(Integer.toHexString(Integer.parseUnsignedInt(binStr.substring(i, i + QUADBIT), BINRADIX)));
        }
        return data.toString();
    }

    /**
     * Decodes a binary string into a UTF string
     * @param binStr the binary string to decode
     * @return the decoded UTF data
     */
    public static final String decodeUTF(final String binStr) {
        StringBuilder data = new StringBuilder();
        for (int i = 0; i < binStr.length(); i += OCTET) {
            // Parse 8 UTF bits from the binary string to an integer, and cast that as a char
            data.append((char) Integer.parseUnsignedInt(binStr.substring(i, i + OCTET), BINRADIX));
        }
        return data.toString();
    }
}
