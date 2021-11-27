/*
 * Whiteflag Java Library
 */
package org.whiteflagprotocol.java.core;

import java.util.regex.Pattern;

/**
 * Whiteflag binary string object
 * 
 * <p> This class defines a binary string used for the encoding and
 * conversion of Whiteflag messages and message fields. Instances of
 * this class represent a binary encoded message or binary encoded field.
 * 
 * @wfref 4.1 Message Structure
 */
public class WfBinaryString {

    /* PROPERTIES */

    /* Constants */
    public static final Pattern BINPATTERN = Pattern.compile("^[01]*$");
    public static final Pattern HEXPATTERN = Pattern.compile("^[a-fA-F0-9]*$");
    public static final String BINPREFIX = "0b";
    public static final String HEXPREFIX = "0x";
    public static final int BINRADIX = 2;
    public static final int HEXRADIX = 16;
    public static final int OCTET = 8;
    public static final int QUADBIT = 4;
    public static final int BIT = 1;

    /* Main property */
    private String value;

    /* CONSTRUCTORS */

    /**
     * Constructs a new empty binary string object
     */
    public WfBinaryString() {
        this.value = "";
    }

    /**
     * Constructs a new binary string object from a string with binary encoded data
     * @param data string representation of the binary encoded data; only characters "0" and "1" are allowed
     * @throws IllegalArgumentException if the provided string is not binary encoded data
     */
    public WfBinaryString(final String data) {
        set(data);
    }

    /**
     * Constructs a new binary string object from an existing binary string object
     * @param binaryString the binary string to be copied
     */
    public WfBinaryString(final WfBinaryString binaryString) {
        set(binaryString.toBinString());
    }

    /* PUBLIC METHODS: basic object interface */

    /**
     * Returns the binary string object as a binary string
     * @return binary representation of the object
     * @see #toBinString()
     */
    @Override
    public final String toString() {
        return this.toBinString();
    }

    /* PUBLIC METHODS: metadata & validators */

    /**
     * Checks if this binary string is empty
     * @return TRUE if null or empty, else FALSE
     */
    public final Boolean isEmpty() {
        if (this.value == null || this.value.isEmpty()) return true;
        return false;
    }

    /**
     * Gets the length of the binary string
     * @return the length of the binary string, i.e. the number of bits
     */
    public final int length() {
        return this.value.length();
    }

    /* PUBLIC METHODS: getters & setters */

    /**
     * Sets the value with the provided binary string
     * @param data the binary representation of the value
     * @return this {@link WfBinaryString}
     * 
     */
    public final WfBinaryString set(final String data)  {
        return setBinValue(data);
    }

    /**
     * Sets the value with the provided binary string
     * @param data the binary representation of the value
     * @return this {@link WfBinaryString}
     */
    public final WfBinaryString setBinValue(final String data) {
        if (data == null) throw new IllegalArgumentException("Null is not a valid binary string");

        // Check binary string
        String bin = removePrefix(data, BINPREFIX);
        if (!BINPATTERN.matcher(bin).matches()) {
            throw new IllegalArgumentException("Invalid binary string: " + bin);
        }
        // Set value and return object
        this.value = bin;
        return this;
    }

    /**
     * Sets the value with the provided hexadecimal string
     * @param data the hexadecimal representation of the value
     * @return this {@link WfBinaryString}
     */
    public final WfBinaryString setHexValue(final String data) {
        if (data == null) throw new IllegalArgumentException("Null is not a valid hexadecimal string");

        // Check hexadecimal string
        String hex = removePrefix(data, HEXPREFIX);
        if (!HEXPATTERN.matcher(hex).matches()) {
            throw new IllegalArgumentException("Invalid hexadecimal string: " + hex);
        }
        // Convert hexadecimal to binary
        StringBuilder bin = new StringBuilder();
        for(char c : hex.toCharArray()) {
            bin.append(padLeft(Integer.toBinaryString(Character.digit(c, HEXRADIX)), QUADBIT));
        }
        // Set value and return object
        this.value = bin.toString();
        return this;
    }

    /**
     * Gets the binary string value as a binary string without 0b prefix
     * @return binary representation of the value
     * @see #toBinString(Boolean)
     */
    public final String toBinString() {
        return this.toBinString(false);
    }

    /**
     * Gets the binary string value as a binary string
     * @param prefix if TRUE, the resulting string gets a 0b prefix
     * @return binary representation of the value
     */
    public final String toBinString(final Boolean prefix) {
        if (Boolean.TRUE.equals(prefix)) {
            return BINPREFIX + this.value;
        }
        return this.value;
    }

    /**
     * Gets the binary string value as a hexadeciomal string without 0x prefix
     * @return hexadecimal representation of the value
     * @see #toHexString(Boolean)
     */
    public final String toHexString() {
        return this.toHexString(false);
    }

    /**
     * Gets the binary string value as a hexadeciomal string
     * @param prefix if TRUE, the resulting string gets a 0x prefix
     * @return hexadecimal representation of the value
     */
    public final String toHexString(final Boolean prefix) {
        StringBuilder hex = new StringBuilder();
        String bin = padRight(value, OCTET);

        // Prefix
        if (Boolean.TRUE.equals(prefix)) hex.append(HEXPREFIX);

        // Build hex string
        for (int i = 0; i < bin.length(); i = i + QUADBIT) {
            int dec = Integer.parseInt(bin.substring(i, i + QUADBIT), BINRADIX);
            hex.append(Integer.toString(dec, HEXRADIX));
        }
        return hex.toString().toLowerCase();
    }

    /* PUBLIC METHODS: operations */

    /**
     * Extracts the specified bit range of the binary string
     * @param startBit first bit of the requested range (included)
     * @param endBit last bit of the range (not included)
     * @return new binary string with the extracted bit range
     * @see #sub(int)
     */
    public final WfBinaryString sub(final int startBit, final int endBit) {
        return new WfBinaryString(this.value.substring(startBit, endBit));
    }

    /**
     * Extracts range from the specified starting bit to the end of the binary string
     * @param startBit first bit of the requested range (included)
     * @return new binary string with the extracted bit range
     * @see #sub(int, int)
     */
    public final WfBinaryString sub(final int startBit) {
        return new WfBinaryString(this.value.substring(startBit));
    }

    /**
     * Appends a binary string to this binary string
     * @param binString the binary string to be added
     * @return this {@link WfBinaryString}
     */
    public final WfBinaryString append(final WfBinaryString binString) {
        this.value = this.value + binString.toBinString();
        return this;
    }

    /* STATIC METHODS */

    /**
     * Checks for and removes prefix from string
     * @param str string to be checked
     * @param prefix the prefix to be checked for
     * @return the string without prefix
     */
    public static final String removePrefix(final String str, final String prefix) {
        if (str.startsWith(prefix)) {
            return str.substring(prefix.length());
        }
        return str;
    }

    /**
     * Adds 0 to the left to fill the encoded field to its fields size
     * @param str the string to be padded
     * @param n the field size
     * @return the padded encoded field
     */
    public static final String padRight(final String str, final int n) {
        int pad = str.length() % n;
        if (pad == 0) return str;
        return str + "00000000".substring(0, n - pad);
    }

    /**
     * Adds 0 to the left to fill the encoded field to its fields size
     * @param str the string to be padded
     * @param n the field size
     * @return the padded encoded field
     */
    public static final String padLeft(final String str, final int n) {
        return "00000000".substring(0, n - str.length()) + str;
    }
}
