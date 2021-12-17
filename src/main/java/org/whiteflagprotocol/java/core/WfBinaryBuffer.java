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
        final byte[] byteArray = WfMessageField.encodeField(field);
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
        StringBuilder hexBuffer = new StringBuilder();
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
     * @param nBits the number of bits to add from the byte array
     * @return the string without prefix
     */
    protected final WfBinaryBuffer addBits(final byte[] byteArray, int nBits) {
        /* Check number of bits */
        if (nBits > (byteArray.length * BYTE)) {
            throw new IllegalArgumentException("Cannot add " + nBits + " from byte array of length " + byteArray.length);
        }
        this.buffer = concatinateBits(this.buffer, this.length, byteArray, nBits);
        this.length += nBits;
        return this;
    }

    /* PROTECTED STATIC UTILITY METHODS */

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
     * Concatinates two bit sets
     * @param byteArray1 byte array containing the first set of bits 
     * @param nBit1 number of bits in the first set, i.e. which bits to take from the first byte array
     * @param byteArray2 byte array containing the second set of bits
     * @param nBit1 number of bits in the second set, i.e. which bits to take from the second byte array
     * @return byte array with the concatinated bits
     */
    protected static final byte[] concatinateBits(final byte[] byteArray1, final int nBit1, final byte[] byteArray2, final int nBit2) {
        /* Calculate support parameters */
        final int shiftBits = nBit1 % BYTE;
        final int freeBits = (shiftBits == 0 ? 0 : BYTE - shiftBits);
        final int byteLength1 = (nBit1 / BYTE) + (freeBits == 0 ? 0 : 1);
        final int bitLength = nBit1 + nBit2;
        final int byteLength = (bitLength / BYTE) + (bitLength % BYTE == 0 ? 0 : 1);

        /* Prepare byte arrays */
        byte[] byteArray2shift = shiftRight(byteArray2, shiftBits);
        byte[] byteArray = new byte[byteLength];
        
        /* Concatination */
        int byteIndex = 0;
        int startByte2 = 0;
        if (byteLength1 != 0) {
            /* Add first byte array */
            for (int i = 0; i < byteLength1; i++) {
                byteIndex = i;
                byteArray[byteIndex] = byteArray1[i];
            }
            /* Add overlapping byte from second byte array*/
            if (freeBits > 0) {
                byteArray[byteIndex] |= byteArray2shift[0];
                startByte2 = 1;
            }
            byteIndex++;
        }
        /* Add the rest of the second byte array */
        final int endByte2 = startByte2 + byteLength - byteIndex;
        for (int i = startByte2; i < endByte2; i++) {
            byteArray[byteIndex] = byteArray2shift[i];
            byteIndex++;
        }
        return clearEndBits(byteArray, bitLength);
    }

    /**
     * Clears unused bits in last byte of the byte array
     * @param byteArray the byte array containing the bit set
     * @param nBits the number of used bits in the bit set
     * @return the byte array with the unused bits cleared
     */
    protected static final byte[] clearEndBits(byte[] byteArray, final int nBits) {
        /* Clear bits after at the end */
        final int clearBits = BYTE - (nBits % BYTE);
        if (clearBits < BYTE) {
            byteArray[byteArray.length - 1] &= (byte) (0xFF << clearBits);
        }
        return byteArray;
    }

    /**
     * Checks for and removes prefix from string
     * @param str string to be checked
     * @param prefix the prefix to be checked for
     * @return the string without prefix
     */
    protected static final String removePrefix(final String str, final String prefix) {
        if (str.startsWith(prefix)) return str.substring(prefix.length());
        return str;
    }
}
