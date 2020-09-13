/*
 * Whiteflag Java Library
 */
package org.whiteflag.protocol.core;

import java.util.regex.Pattern;

/**
 * Whiteflag binary string object
 * 
 * </p> This class defines a binary string used for the encoding and
 * conversion of Whiteflag messages and message fields. Instances of
 * this class represent a binary encoded message or binary encoded field.
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
     * @param data String representing binary encoded data; only characters "0" and "1" are allowed
     * @throws IllegalArgumentException if the provided string is not binary encoded data
     */
    public WfBinaryString(String data) {
        setValue(data);
    }

    /**
     * Constructs a new binary string object from an existing binary string object
     * @param binaryString the {@link WfBinaryString} to create a new binary string from
     */
    public WfBinaryString(WfBinaryString binaryString) {
        setValue(binaryString.toBinString());
    }

    /* PUBLIC METHODS: basic object interface */

    /**
     * Returns the binary string object as a binary string
     * @return String with binary representation of the object
     */
    @Override
    public final String toString() {
        return this.toBinString();
    }

    /* PUBLIC METHODS: metadata & validators */

    /**
     * Chekcs if this binary string is empty
     * @return TRUE if null or empty, else FALSE
     */
    public final Boolean isEmpty() {
        if (this.value.isEmpty()) return true;
        return false;
    }

    /**
     * Returns the length of the binary string
     * @return integer with the length
     */
    public final int length() {
        return this.value.length();
    }

    /* PUBLIC METHODS: getters & setters */

    /**
     * Sets the value with the provided binary string
     * @param data a string with the binary representation of the value
     * @return this {@link WfBinaryString}
     */
    public final WfBinaryString setValue(String data)  {
        return setBinValue(data);
    }

    /**
     * Sets the value with the provided binary string
     * @param data a string with the binary representation of the value
     * @return this {@link WfBinaryString}
     */
    public final WfBinaryString setBinValue(String data) {
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
     * @param data a string with the hexadecimal representation of the value
     * @return this {@link WfBinaryString}
     */
    public final WfBinaryString setHexValue(String data) {
        if (data == null) throw new IllegalArgumentException("Null is not a valid hexadecimal string");

        // Check hexadecimal string
        String hex = removePrefix(data, HEXPREFIX);
        if (!HEXPATTERN.matcher(hex).matches()) {
            throw new IllegalArgumentException("Invalid hexadecimal string: " + hex);
        }
        // Convert hexadecimal to binary
        StringBuilder bin = new StringBuilder();;
        for(char c : hex.toCharArray()) {
            bin.append(padLeft(Integer.toBinaryString(Character.digit(c, HEXRADIX)), QUADBIT));
        }
        // Set value and return object
        this.value = bin.toString();
        return this;
    }

    /**
     * Returns the binary string value as a binary string without 0b prefix
     * @return String with binary representation
     */
    public final String toBinString() {
        return this.toBinString(false);
    }

    /**
     * Returns the binary string value as a binary string
     * @param prefix if TRUE, the resulting string gets a 0b prefix
     * @return String with binary representation
     */
    public final String toBinString(Boolean prefix) {
        if (Boolean.TRUE.equals(prefix)) {
            return BINPREFIX + this.value;
        }
        return this.value;
    }

    /**
     * Returns the binary string value as a hexadeciomal string without 0x prefix
     * @return String with hexadecimal representation
     */
    public final String toHexString() {
        return this.toHexString(false);
    }

    /**
     * Returns the binary string value as a hexadeciomal string
     * @param prefix if TRUE, the resulting string gets a 0x prefix
     * @return String with hexadecimal representation
     */
    public final String toHexString(Boolean prefix) {
        StringBuilder hex = new StringBuilder();
        String bin = padRight(value, OCTET);

        // Prefix
        if (Boolean.TRUE.equals(prefix)) hex.append(HEXPREFIX);

        // Build hex string
        for (int i = 0; i < bin.length(); i = i + QUADBIT) {
            int dec = Integer.parseInt(bin.substring(i, i + QUADBIT), BINRADIX);
            hex.append(Integer.toString(dec, HEXRADIX));
        }
        return hex.toString();
    }

    /* PUBLIC METHODS: operations */

    /**
     * Extracts the specified bit range of the binary string into a new binary string object
     * @param startBit first bit of the requested range
     * @param endBit last bit of the range (not included)
     * @return String with binary representation
     */
    public final WfBinaryString sub(int startBit, int endBit) {
        return new WfBinaryString(this.value.substring(startBit, endBit));
    }

    /**
     * Appends a binary string to this binary string
     * @param binString the {@link WfBinaryString} to be added
     * @return The updated binary string object
     */
    public final WfBinaryString append(WfBinaryString binString) {
        this.value = this.value + binString.toBinString();
        return this;
    }

    /* STATIC METHODS */

    /**
     * Checks for and removes prefix from string
     * @param string STring to be checked
     * @param prefix The prefix to be checked for
     * @return String without prefix
     */
    public static final String removePrefix(String string, String prefix) {
        if (string.startsWith(prefix)) {
            return string.substring(prefix.length());
        }
        return string;
    }

    /**
     * Adds 0 to the left to fill the encoded field to its fields size
     * @param s the string to be padded
     * @param n the field size
     * @return String with the padded encoded field
     */
    public static final String padRight(String s, int n) {
        int pad = s.length() % n;
        if (pad == 0) return s;
        return s + "00000000".substring(0, n - pad);
    }

    /**
     * Adds 0 to the left to fill the encoded field to its fields size
     * @param s the string to be padded
     * @param n the field size
     * @return String with the padded encoded field
     */
    public static final String padLeft(String s, int n) {
        return "00000000".substring(0, n - s.length()) + s;
    }
}
