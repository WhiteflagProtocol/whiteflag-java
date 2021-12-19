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
    private WfBinaryBuffer(final byte[] byteArray) {
        this.buffer = byteArray;
        this.length = byteArray.length * BYTE;
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
    public static WfBinaryBuffer fromByteArray(final byte[] data) {
        return new WfBinaryBuffer(data);
    }

    /**
     * Constructs a new Whiteflag binary encoded message buffer from a hexadecimal string
     * @param hexstr a hexadecimal string with a binary encoded Whiteflag message
     * @return a new {@link WfBinaryBuffer}
     */
    public static WfBinaryBuffer fromHexString(final String data) {
        if (data == null) throw new IllegalArgumentException("Null is not a valid hexadecimal string");

        // Check hexadecimal string
        String hexstr = removeStringPrefix(data, HEXPREFIX);
        if (!HEXPATTERN.matcher(hexstr).matches()) {
            throw new IllegalArgumentException("Invalid hexadecimal string: " + hexstr);
        }
        return new WfBinaryBuffer(convertToByteArray(hexstr));
    }

    /**
     * Returns the bit length of the binary buffer
     * @return the buffer length in bits
     */
    public int length() {
        return this.length;
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
        final byte[] byteArray = WfMessageCodec.encodeField(field);
        return appendBits(byteArray, field.bitLength());
    }

    /* PUBLIC STATIC UTILITY METHODS */

    /**
     * Converts a hexadecimal string to a byte array
     * @param hexstr the hexadecimal string
     * @return a byte array
     */
    public static final byte[] convertToByteArray(final String hexstr) {
        final int length = hexstr.length();
        byte[] byteArray = new byte[length / 2];
        for (int i = 0; i < length; i += 2) {
            byteArray[i / 2] = (byte) ((Character.digit(hexstr.charAt(i), HEXRADIX) << QUADBIT)
                                      + Character.digit(hexstr.charAt(i + 1), HEXRADIX));
        }
        return byteArray;
    }

    /**
     * Converts a byte array to a hexadecimal string
     * @param byteArray the byte array
     * @return a hexadecimal string
     */
    public static final String convertToHexString(final byte[] byteArray) {
        StringBuilder hexstr = new StringBuilder();
        for (int byteIndex = 0; byteIndex < byteArray.length; byteIndex++) {
            char[] hexDigits = new char[2];
            hexDigits[0] = Character.forDigit((byteArray[byteIndex] >> QUADBIT) & 0xF, HEXRADIX);
            hexDigits[1] = Character.forDigit((byteArray[byteIndex] & 0xF), HEXRADIX);
            hexstr.append(new String(hexDigits));
        }
        return hexstr.toString().toLowerCase();
    }

    /**
     * Shifts bits in a byte array to the right modulo 8
     * @param byteArray the byte array to be right shifted
     * @param shift the nummber of bits to be right shifted by modulo 8 bits
     * @return a new byte array from the right shifted source byte array
     */
    public static byte[] shiftRight(final byte[] byteArray, int shift) {
        /* Check negative value */
        if (shift < 0) return shiftLeft(byteArray, -shift);

        /* Calculate shift parameters */
        final int mod = shift % BYTE; if (mod == 0) return byteArray;
        final byte mask = (byte) (0xFF >>> (BYTE - mod));

        /* Create new byte array, starting at the end */
        byte[] newByteArray = new byte[byteArray.length + 1];
        for (int byteIndex = byteArray.length; byteIndex > 0; byteIndex--) {
            newByteArray[byteIndex] |= (byte) ((0xFF & byteArray[byteIndex - 1] & mask) << (BYTE - mod));
            newByteArray[byteIndex - 1] = (byte) ((0xFF & byteArray[byteIndex - 1]) >>> mod);
        }
        return newByteArray;
    }

    /**
     * Shifts bits in a byte array to the left modulo 8
     * @param byteArray the byte array to be left shifted
     * @param shift the nummber of bits to be left shifted by modulo 8 bits
     * @return a new byte array from the left shifted source byte array
     */
    public static byte[] shiftLeft(final byte[] byteArray, int shift) {
        /* Check negative value */
        if (shift < 0) return shiftRight(byteArray, -shift);

        /* Calculate shift parameters */
        final int mod = shift % BYTE; if (mod == 0) return byteArray;
        final byte mask = (byte) (0xFF << (BYTE - mod));

        /* Create new byte array, starting at the end */
        byte[] newByteArray = new byte[byteArray.length];
        for (int byteIndex = 0; byteIndex < byteArray.length; byteIndex++) {
            newByteArray[byteIndex] = (byte) ((0xFF & byteArray[byteIndex]) << mod);
        }
        for (int byteIndex = 0; byteIndex < (byteArray.length - 1); byteIndex++) {
            newByteArray[byteIndex] |= (byte) ((0xFF & byteArray[byteIndex + 1] & mask) >>> (BYTE - mod));
        }
        return clearUnusedBits(newByteArray, BYTE - shift);
    }

    /* PROTECTED METHODS */

    /**
     * Appends the specified number of bits from a bytes array to the binary buffer
     * @param byteArray the byte array with the bits to be appended
     * @param nBits the number of bits to be appended from the byte array
     * @return the string without prefix
     */
    protected final WfBinaryBuffer appendBits(final byte[] byteArray, int nBits) {
        /* Check number of bits */
        if (nBits > (byteArray.length * BYTE)) nBits = byteArray.length * BYTE;

        /* Add bits to the end of the buffer */
        this.buffer = concatinateBits(this.buffer, this.length, byteArray, nBits);
        this.length += nBits;
        return this;
    }

    /**
     * Returns a byte array with a subset of the bits in the buffer
     * @param startBit the first bit of the subset to extract
     * @param bitLength the length of the subset, i.e. the number of bits to extract
     * @return a byte array with the extracted bits
     */
    protected final byte[] extractBits(int startBit, int bitLength) {
        /* Check subset range */
        if (startBit < 0) startBit = 0;
        if (bitLength > (this.length - startBit)) bitLength = (this.length - startBit);

        /* Calculate parameters */
        final int startByte = startBit / BYTE;
        final int byteLength = (bitLength / BYTE) + ((bitLength % BYTE) == 0 ? 0 : 1);
        final int shift = startBit % BYTE;
        final byte mask = (byte) (0xFF << (BYTE - shift));

        /* Create new byte array with the subset */ 
        byte[] newByteArray = new byte[byteLength];
        for (int byteIndex = 0; byteIndex < byteLength; byteIndex++) {
            newByteArray[byteIndex] = (byte) ((0xFF & this.buffer[startByte + byteIndex]) << shift);
        }
        final int endByte = byteLength < this.buffer.length ? byteLength : (byteLength - 1);
        for (int byteIndex = 0; byteIndex < endByte; byteIndex++) {
            newByteArray[byteIndex] |= (byte) ((0xFF & this.buffer[startByte + byteIndex + 1] & mask) >>> (BYTE - shift));
        }
        return clearUnusedBits(newByteArray, bitLength);
    }

    /* PROTECTED STATIC UTILITY METHODS */

    /**
     * Concatinates two bitsets
     * @param byteArray1 byte array containing the first bitset
     * @param nBits1 number of bits in the first bitset, i.e. which bits to take from the first byte array
     * @param byteArray2 byte array containing the second bitset
     * @param nBits1 number of bits in the second bitset, i.e. which bits to take from the second byte array
     * @return a new byte array with the concatinated bits
     */
    protected static final byte[] concatinateBits(final byte[] byteArray1, int nBits1, final byte[] byteArray2, int nBits2) {
        /* Check number of bits */
        if (nBits1 > (byteArray1.length * BYTE)) nBits1 = byteArray1.length * BYTE;
        if (nBits2 > (byteArray2.length * BYTE)) nBits2 = byteArray2.length * BYTE;

        /* Calculate parameters */
        final int shift = nBits1 % BYTE;
        final int freeBits = (shift == 0 ? 0 : BYTE - shift);
        final int byteLength1 = (nBits1 / BYTE) + (freeBits == 0 ? 0 : 1);
        final int bitLength = nBits1 + nBits2;
        final int byteLength = (bitLength / BYTE) + (bitLength % BYTE == 0 ? 0 : 1);

        /* Prepare byte arrays */
        byte[] byteArray2shift = shiftRight(byteArray2, shift);
        byte[] newByteArray = new byte[byteLength];

        /* Concatination */
        int byteCursor = 0;
        int startByte2 = 0;
        if (byteLength1 != 0) {
            /* Add first byte array */
            for (int byteIndex = 0; byteIndex < byteLength1; byteIndex++) {
                byteCursor = byteIndex;
                newByteArray[byteCursor] = byteArray1[byteIndex];
            }
            /* Add overlapping byte from second byte array*/
            if (freeBits > 0) {
                newByteArray[byteCursor] |= byteArray2shift[0];
                startByte2 = 1;
            }
            byteCursor++;
        }
        /* Add the rest of the second byte array */
        final int endByte2 = startByte2 + byteLength - byteCursor;
        for (int byteIndex = startByte2; byteIndex < endByte2; byteIndex++) {
            newByteArray[byteCursor] = byteArray2shift[byteIndex];
            byteCursor++;
        }
        return clearUnusedBits(newByteArray, bitLength);
    }

    /**
     * Clears unused bits in last byte of the byte array
     * @param byteArray the byte array containing the bitset
     * @param nBits the number of used bits in the bitset
     * @return the byte array with the unused bits cleared
     */
    protected static final byte[] clearUnusedBits(byte[] byteArray, final int nBits) {
        /* Check number of bits */
        if (nBits > (byteArray.length * BYTE)) return byteArray;
        
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
     * @return a string without prefix
     */
    protected static final String removeStringPrefix(final String str, final String prefix) {
        if (str.startsWith(prefix)) return str.substring(prefix.length());
        return str;
    }
}
