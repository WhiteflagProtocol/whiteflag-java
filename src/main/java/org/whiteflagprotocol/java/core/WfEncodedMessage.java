/*
 * Whiteflag Java Library
 */
package org.whiteflagprotocol.java.core;

import java.util.regex.Pattern;

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
public class WfEncodedMessage {

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
    private byte[] message;

    /**
     * Encoding and decoding cursor
     */
    private int bitCursor = 0;

    /**
     * Encoding and decoding cursor
     */
    private boolean complete = false;

    /* CONSTRUCTOR */

    /**
     * Constructs a new Whiteflag encoded message object from a byte array
     * @param message a byte array
     */
    private WfEncodedMessage(final byte[] message) {
        this.message = message;
        this.complete = true;
        this.bitCursor = 0;
    }

    /* METHODS */

    /**
     * Checks if the encoded message is finalised, i.e. if it contains a complete message
     * @return TRUE if message is fully encoded, else FALSE
     */
    public boolean isComplete() {
        return this.complete;
    }

    /**
     * Constructs a new Whiteflag encoded message object from a byte array
     * @param byteArray a byte array with an encoded message
     */
    public static WfEncodedMessage fromByteArray(final byte[] byteArray) {
        return new WfEncodedMessage(byteArray);
    }

    /**
     * Constructs a new Whiteflag encoded message object from a byte array
     * @param hexString a hexadecimal string with the encoded message
     */
    public static WfEncodedMessage fromHexString(final String data) {
        if (data == null) throw new IllegalArgumentException("Null is not a valid hexadecimal string");

        // Check hexadecimal string
        String hexString = removePrefix(data, HEXPREFIX);
        if (!HEXPATTERN.matcher(hexString).matches()) {
            throw new IllegalArgumentException("Invalid hexadecimal string: " + hexString);
        }
        return new WfEncodedMessage(convertToByteArray(hexString));
    }

    /**
     * Returns the Whiteflag encoded message as a byte array
     * @return a byte array with an encoded message
     */
    public byte[] toByteArray() {
        return this.message;
    }

    /**
     * Returns the Whiteflag encoded message as a hexademical string
     * @return a hexadecimal string with the encoded message
     */
    public String toHexString() {
        return convertToHexString(this.message);
    }

    /* PRIVATE METHODS */

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
    private static final String convertToHexString(final byte[] byteArray) {
        StringBuffer hexBuffer = new StringBuffer();
        for (int i = 0; i < byteArray.length; i++) {
            char[] hexDigits = new char[2];
            hexDigits[0] = Character.forDigit((byteArray[i] >> QUADBIT) & 0xF, HEXRADIX);
            hexDigits[1] = Character.forDigit((byteArray[i] & 0xF), HEXRADIX);
            hexBuffer.append(new String(hexDigits));
        }
        return hexBuffer.toString().toLowerCase();
    }
}
