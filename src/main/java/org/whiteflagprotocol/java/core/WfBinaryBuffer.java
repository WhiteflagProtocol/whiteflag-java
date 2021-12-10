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
     * A byte array holding the binary buffer
     */
    private byte[] buffer;

    /**
     * The length of the binary buffer in bits
     */
    private int length = 0;

    /**
     * Decoding bit cursor
     */
    private int cursor = 0;

    /**
     * Encoding status
     */
    private boolean encoded = false;

    /* CONSTRUCTOR */

    /**
     * Constructs a new empty Whiteflag binary encoded message buffer
     * @param message a byte array
     */
    private WfBinaryBuffer() {
        this.buffer = new byte[0];
    }
    
    /**
     * Constructs a new Whiteflag binary encoded message buffer from a byte array
     * @param message a byte array
     */
    private WfBinaryBuffer(final byte[] message) {
        this.buffer = message;
        this.encoded = true;
        this.length = message.length * BYTE;
    }

    /**
     * Constructs a new Whiteflag binary encoded message buffer from a byte array
     * @param message a byte array with a (partially) binary encoded message
     * @param encoded TRUE, if this is a full encoded message, else FALSE
     */
    private WfBinaryBuffer(final byte[] message, final boolean encoded) {
        this.buffer = message;
        this.encoded = encoded;
        this.length = message.length * BYTE;
    }

    /* PUBLIC METHODS: basic interface */

    /**
     * Creates a new Whiteflag binary buffer
     * @return a new {@link WfBinaryBuffer}
     */
    public static WfBinaryBuffer create() {
        return new WfBinaryBuffer();
    }

    /**
     * Constructs a new Whiteflag binary encoded message buffer from a byte array
     * @param byteArray a byte array with a binary encoded Whiteflag message
     * @return a new {@link WfBinaryBuffer}
     */
    public static WfBinaryBuffer fromByteArray(final byte[] byteArray) {
        return new WfBinaryBuffer(byteArray);
    }

    /**
     * Constructs a new Whiteflag binary buffer from a byte array
     * @param byteArray a byte array
     * @param encoded TRUE, if this is a full encoded message, else FALSE
     * @return a new {@link WfBinaryBuffer}
     * @
     */
    public static WfBinaryBuffer fromByteArray(final byte[] byteArray, final boolean encoded) {
        return new WfBinaryBuffer(byteArray, encoded);
    }

    /**
     * Constructs a new Whiteflag binary encoded message buffer from a hexadecimal string
     * @param hexString a hexadecimal string with a binary encoded Whiteflag message
     * @return a new {@link WfBinaryBuffer}
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
     * Returns the bit length of the binary buffer
     * @return the buffer length in bits
     */
    public int length() {
        return this.length;
    }

    /**
     * Completes the encoding of the Whiteflag message
     * @return this {@link WfBinaryBuffer}
     */
    public WfBinaryBuffer encode() {
        this.encoded = true;
        return this;
    }

    /**
     * Checks if the encoded message is finalised, i.e. if it contains a complete message
     * @return TRUE if message is fully encoded, else FALSE
     */
    public boolean isEncoded() {
        return this.encoded;
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
     * @param field the next {@link WfMessageField} to be encoded and added to the buffer
     * @return this {@link WfBinaryBuffer}
     */
    public WfBinaryBuffer addMessageField(WfMessageField field) {
        final byte[] byteArray = encodeField(field);
        return addBits(byteArray, field.bitLength());
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

    /* PROTECTED METHODS */

    /**
     * Adds the specified number of bits from a bytes array to the binary buffer
     * @param byteArray the byte array with the bits to be added
     * @param nAddBits the number of bits to add from the byte array
     * @return the string without prefix
     */
    protected final WfBinaryBuffer addBits(final byte[] byteArray, int nAddBits) {
        /* Check number of bits */
        if (nAddBits > (byteArray.length * BYTE)) {
            throw new IllegalArgumentException("Cannot add " + nAddBits + " from byte array of length " + byteArray.length);
        }
        /* Calculate support parameters */
        final int shiftBits = this.length % BYTE;
        final int freeBits = (shiftBits == 0 ? 0 : BYTE - shiftBits);
        final int currentByteLength = (this.length == 0 ? 0 : (this.length / BYTE) + (freeBits == 0 ? 0 : 1));
        final int newBitLength = this.length + nAddBits;
        final int newByteLength = (newBitLength / BYTE) + 1;

        /* Add the bits from the byte array */
        if (freeBits >= nAddBits && newByteLength == currentByteLength) {
            /* If field is less then a free bits, then put in last byte of buffer */ 
            this.buffer[buffer.length - 1] |= (byte) (byteArray[0] >>> shiftBits);
        } else {
            /* If field is longer, add the old and provided byte arrays to a new buffer */
            byte[] newBuffer = new byte[newByteLength];
            byte[] addBuffer = shiftRight(byteArray, shiftBits);

            /* Add existing buffer to new buffer */
            int newByteIndex = 0;
            int addByteStart = 0;
            if (currentByteLength != 0) {
                for (int i = 0; i < currentByteLength; i++) {
                    newByteIndex = i;
                    newBuffer[newByteIndex] = this.buffer[i];
                }
                /* Add overlapping byte */
                if (freeBits > 0) {
                    newBuffer[newByteIndex] |= addBuffer[0];
                    addByteStart = 1;
                }
                newByteIndex++;
            }
            /* Add the rest of the bytes to new buffer */
            final int nAddBytes = addByteStart + newByteLength - newByteIndex;
            for (int i = addByteStart; i < nAddBytes; i++) {
                newBuffer[newByteIndex] = addBuffer[i];
                newByteIndex++;
            }
            this.buffer = newBuffer;
        }
        /* Update properties */
        this.length = newBitLength;

        /* Clear bits after at the end */
        final int clearBits = BYTE - (this.length % BYTE);
        if (clearBits < BYTE) {
            this.buffer[buffer.length - 1] &= (byte) (0xFF << clearBits);
        }
        /* Done */
        return this;
    }

    /* PROTECTED STATIC UTILITY METHODS */

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

    /**
     * Bitwise right shift of whole byte array, returning a new byte array
     * @param srcByteArray the source byte array to be shifted
     * @param shift the nummber of bits to be shifted; will be modulo 8 bits
     * @return a new byte array from the right shifted source byte array
     */
    protected static byte[] shiftRight(final byte[] srcByteArray, int shift) {
        /* Calculate shift */
        final int mod = shift % BYTE;
        if (mod == 0) return srcByteArray;

        /* Create new byte array, starting at the end */
        final byte mask = (byte) (0xFF >>> (BYTE - mod));
        byte[] newByteArray = new byte[srcByteArray.length + 1];
        for (int i = srcByteArray.length; i > 0; i--) {
            newByteArray[i] |= (byte) ((srcByteArray[i - 1] & mask) << (BYTE - mod));
            newByteArray[i - 1] = (byte) ((0xFF & srcByteArray[i - 1]) >>> mod);
        }
        return newByteArray;
    }

    /**
     * Checks for and removes prefix from string
     * @param str string to be checked
     * @param prefix the prefix to be checked for
     * @return the string without prefix
     */
    protected static final String removePrefix(final String str, final String prefix) {
        if (str.startsWith(prefix)) {
            return str.substring(prefix.length());
        }
        return str;
    }
}
