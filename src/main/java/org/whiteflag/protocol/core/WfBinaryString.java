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
     * Creates an empty binary string object
     */
    public WfBinaryString() {
        this.value = "";
    }

    /**
     * Creates a binary string object from a string with binary encoded data
     * @param data String representing binary encoded data
     * @throws WfCoreException if the provided string is not binary encoded data
     */
    public WfBinaryString(String data) throws WfCoreException {
        if (data == null || !BINPATTERN.matcher(data).matches()) {
            throw new WfCoreException("Invalid binary string");
        }
        this.value = data;
    }

    /* PUBLIC METHODS */

    /**
     * Chekcs if this binary string is empty
     * @return TRUE if null or empty, else FALSE
     */
    public Boolean isEmpty() {
        if (value == null) return true;
        if (value.isEmpty()) return true;
        return false;
    }

    /**
     * Converts the binary string object to a string without 0b prefix
     * @return String representation of the binary object
     */
    public String toString() {
        return this.toBinString(false);
    }

    /**
     * Returns the binary string value as a binary string
     * @param prefix if TRUE, the resulting string gets a 0b prefix
     * @return String with binary representation
     */
    public String toString(Boolean prefix) {
        return this.toBinString(prefix);
    }

    /**
     * Returns the binary string value as a binary string
     * @return String with binary representation
     */
    public String toBinString() {
        return this.toBinString(false);
    }

    /**
     * Returns the binary string value as a binary string
     * @param prefix if TRUE, the resulting string gets a 0b prefix
     * @return String with binary representation
     */
    public String toBinString(Boolean prefix) {
        if (Boolean.TRUE.equals(prefix)) {
            return BINPREFIX + this.value;
        }
        return this.value;
    }

    /**
     * Converts the specified range of the binary string object to a binary string
     * @param beginIndex first bit of the requested range
     * @param endIndex last bit of the range (not included)
     * @return String with binary representation
     */
    public WfBinaryString substring(int beginIndex, int endIndex) throws WfCoreException {
        return new WfBinaryString(this.value.substring(beginIndex, endIndex));
    }

    /**
     * Returns the binary string value as a hexadeciomal string without 0x prefix
     * @return String with hexadecimal representation
     */
    public String toHexString() {
        return this.toHexString(false);
    }

    /**
     * Returns the binary string value as a hexadeciomal string
     * @param prefix if TRUE, the resulting string gets a 0x prefix
     * @return String with hexadecimal representation
     */
    public String toHexString(Boolean prefix) {
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

    /**
     * Add a binary string object to this binary string object
     * @param binString the {@link WfBinaryString} to be added
     * @return The updated binary string object
     */
    public WfBinaryString add(WfBinaryString binString) {
        this.value = this.value + binString.toString();
        return this;
    }

    /* PRIVATE METHODS */

    /**
     * Adds 0 to the left to fill the encoded field to its fields size
     * @param s the string to be padded
     * @param n the field size
     * @return String with the padded encoded field
     */
    private static final String padRight(String s, int n) {
        int pad = s.length() % n;
        if (pad == 0) return s;
        return s + "00000000".substring(0, n - pad);
    }
}
