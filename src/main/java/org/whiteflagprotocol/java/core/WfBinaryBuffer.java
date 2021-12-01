/*
 * Whiteflag Java Library
 */
package org.whiteflagprotocol.java.core;

import java.util.regex.Pattern;
import java.nio.charset.StandardCharsets;

/**
 * Whiteflag encoded message object
 * 
 * <p> This class defines represents an encoded message. It can be created
 * by appending {@link WfMessageField} objects of a Whiteflag message, which
 * are encoded when added. Alternatively, an encoded message object may be
 * crated by providing a hexadecimal string or byte array with an encoded
 * message. The encoded message object can provide a decode message.
 * 
 * @wfref 4.1 Message Structure
 */
public class WfBinaryBuffer {

    /* PROPERTIES */

    /* Constants */
    public static final Pattern HEXPATTERN = Pattern.compile("^[a-fA-F0-9]*$");
    public static final String BINPREFIX = "0b";
    public static final String HEXPREFIX = "0x";
    public static final int BINRADIX = 2;
    public static final int HEXRADIX = 16;
    public static final int BYTE = 8;
    public static final int OCTET = 8;
    public static final int QUADBIT = 4;
    public static final int BIT = 1;

    /**
     * A byte array with the encoded message
     */
    private byte[] buffer;

    /**
     * Encoding and decoding cursor
     */
    private int bitCursor = 0;

    /**
     * Encoding and decoding cursor
     */
    private boolean encoded = false;

    /* CONSTRUCTOR */

    /**
     * Constructs a new Whiteflag encoded message object from a byte array
     * @param message a byte array
     */
    private WfBinaryBuffer(final byte[] message) {
        this.buffer = message;
        this.encoded = true;
        this.bitCursor = 0;
    }

    /* PUBLIC METHODS: basic interface */

    /**
     * Checks if the encoded message is finalised, i.e. if it contains a complete message
     * @return TRUE if message is fully encoded, else FALSE
     */
    public boolean isEncoded() {
        return this.encoded;
    }

    /**
     * Completes the encoding of the Whiteflag message
     * @return this {@link WfBinaryBuffer}
     */
    public WfBinaryBuffer encode() {
        this.bitCursor = 0;
        this.encoded = true;
        return this;
    }

    /**
     * Constructs a new Whiteflag encoded message object from a byte array
     * @param byteArray a byte array with an encoded message
     */
    public static WfBinaryBuffer fromByteArray(final byte[] byteArray) {
        return new WfBinaryBuffer(byteArray);
    }

    /**
     * Constructs a new Whiteflag encoded message object from a byte array
     * @param hexString a hexadecimal string with the encoded message
     */
    public static WfBinaryBuffer fromHexString(final String data) {
        if (data == null) throw new IllegalArgumentException("Null is not a valid hexadecimal string");

        // Check hexadecimal string
        String hexString = removePrefix(data, HEXPREFIX);
        if (!HEXPATTERN.matcher(hexString).matches()) {
            throw new IllegalArgumentException("Invalid hexadecimal string: " + hexString);
        }
        return new WfBinaryBuffer(convertToByteArray(hexString));
    }

    /**
     * Returns the Whiteflag encoded message as a byte array
     * @return a byte array with an encoded message
     */
    public byte[] toByteArray() {
        return this.buffer;
    }

    /**
     * Returns the Whiteflag encoded message as a hexademical string
     * @return a hexadecimal string with the encoded message
     */
    public String toHexString() {
        return convertToHexString(this.buffer);
    }

    /* PUBLIC METHODS: message field operations */

    /**
     * Adds the provided number of bits from the provided byte
     */
    public WfBinaryBuffer addMessageField(WfMessageField field) {
        buffer = addBytes(encodeField(field));
        bitCursor += field.bitLength();
        return this;
    }

    /* PUBLIC STATIC UTILITY METHODS */

    /**
     * Converts a hexadecimal string to a byte array
     * @param hexString the hexadecimal string
     * @return a byte array
     */
    public static final byte[] convertToByteArray(final String hexString) {
        final int length = hexString.length();
        byte[] byteArray = new byte[length / 2];
        for (int i = 0; i < length; i += 2) {
            byteArray[i / 2] = (byte) ((Character.digit(hexString.charAt(i), HEXRADIX) << 4)
                                      + Character.digit(hexString.charAt(i + 1), HEXRADIX));
        }
        return byteArray;
    }

    /**
     * Converts a byte array to a hexadecimal string
     * @param byteArray the byte array
     * @return a hexadecimal string
     */
    public static final String convertToHexString(final byte[] byteArray) {
        StringBuffer hexBuffer = new StringBuffer();
        for (int i = 0; i < byteArray.length; i++) {
            char[] hexDigits = new char[2];
            hexDigits[0] = Character.forDigit((byteArray[i] >> QUADBIT) & 0xF, HEXRADIX);
            hexDigits[1] = Character.forDigit((byteArray[i] & 0xF), HEXRADIX);
            hexBuffer.append(new String(hexDigits));
        }
        return hexBuffer.toString().toLowerCase();
    }

    /**
     * Bitwise right shift of whole byte array
     */
    public static byte[] shiftRight(final byte[] srcByteArray, int shift) {
        final int mod = shift % BYTE;
        final byte mask = (byte) (0xFF >>> (BYTE - mod));
        byte[] newByteArray = new byte[srcByteArray.length + 1];

        // Fill the new new byte array, starting at the end
        for (int i = srcByteArray.length; i > 0; i--) {
            newByteArray[i] |= (byte) ((srcByteArray[i - 1] & mask) << (BYTE - mod));;
            newByteArray[i - 1] = (byte) ((0xFF & srcByteArray[i - 1]) >>> mod);
        }
        return newByteArray;
    }

    /* PROTECTED METHODS */

    /**
     * Encodes a Whiteflag field into a byte array
     * @param field a {@link WfMessageField}
     * @return a byte array with the encooded field
     */
    protected static byte[] encodeField(WfMessageField field) {

        switch (field.encoding) {
            // Encode UTF 8 field
            case UTF8:
                return field.toString().getBytes(StandardCharsets.UTF_8);

            // Encode binary field
            case BIN:
                byte[] bin = new byte[1];
                if (field.toString().equals("0")) bin[0] = (byte) 0x00;
                if (field.toString().equals("1")) bin[0] = (byte) 0x80;
                return bin;

            // Encode decimal or hexadecimal field
            case DEC:
            case HEX:
                return convertToByteArray(field.toString());

            // Encode datum field
            case DATETIME:
            case DURATION:
            case LAT:
            case LONG:
                // Encode string without fixed characters
                byte[] datum = convertToByteArray(field.toString().replaceAll("[\\-+:.A-Z]", ""));
                
                // Sign of lat long coordinates
                if (field.toString().substring(0,1).equals("-")) {
                    datum = shiftRight(datum, 1);
                }
                if (field.toString().substring(0,1).equals("+")) {
                    datum = shiftRight(datum, 1);
                    datum[0] |= (byte) 0x80;
                }
                return datum;
            // Unknown encoding
            default:
                return new byte[0];
        }
    }

    /* PRIVATE METHODS */

    /**
     * Adds the provided bytes array
     */
    private final byte[] addBytes(byte[] byteArray) {
        byte[] newBuffer = new byte[(buffer.length + byteArray.length)];
        byte[] addBuffer = shiftRight(byteArray, bitCursor);
        int index = 0;

        /* Add existing buffer to new buffer */
        for (int i = 0; i < buffer.length; i++) {
            newBuffer[i] = buffer[i];
            index = i;
        }
        /* Add overlapping byte */
        newBuffer[index] |= addBuffer[0];

        /* Add the rest of the bytes to new buffer */
        for (int i = 1; i < newBuffer.length; i++) {
            index++;
            newBuffer[index] = buffer[i];
        }
        return newBuffer;
    }

    /**
     * Checks for and removes prefix from string
     * @param str string to be checked
     * @param prefix the prefix to be checked for
     * @return the string without prefix
     */
    private static final String removePrefix(final String str, final String prefix) {
        if (str.startsWith(prefix)) {
            return str.substring(prefix.length());
        }
        return str;
    }
}
