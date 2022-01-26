/*
 * Whiteflag Java Library
 */
package org.whiteflagprotocol.java.core;

import java.util.Arrays;
import java.util.regex.Pattern;

/**
 * Whiteflag encoded message object
 * 
 * <p> This class defines a binary buffer to represent encoded messages. It
 * can either be created 1. by appending {@link WfMessageField} fields from a
 * Whiteflag message, which are encoded when added, or 2. by providing a
 * hexadecimal string or byte array with an encoded message. The binary buffer
 * can be converted to a hexadecimal string or a byte byffer, e.g. for
 * encryption or embedding the message in a blockchain transaction. After the
 * buffer is marked as complete, it cannot be altered.
 * 
 * @wfref 4.1 Message Structure
 * 
 * @since 1.1
 */
public final class WfBinaryBuffer {

    /* PROPERTIES */

    /* Constants */
    /**
     * The regex pattern describing a valid hexadecimnal string
     */
    public static final Pattern HEXPATTERN = Pattern.compile("^[a-fA-F0-9]*$");
    /**
     * The "0b" prefix of a binary string
     */
    public static final String BINPREFIX = "0b";
    /**
     * The "0x" prefix of a hexadecimal string
     */
    public static final String HEXPREFIX = "0x";
    /**
     * The radic of a binary digit
     */
    public static final int BINRADIX = 2;
    /**
     * The radix of a hexadecimal digit
     */
    public static final int HEXRADIX = 16;
    /**
     * The bit size of a byte
     */
    public static final int BYTE = 8;
    /**
     * The bit size of an octet
     */
    public static final int OCTET = 8;
    /**
     * The bit size of a quadbit
     */
    public static final int QUADBIT = 4;
    /**
     * The bit size of a bit
     */
    public static final int BIT = 1;

    /* Main variables */
    /**
     * A byte array holding the binary buffer
     */
    private byte[] buffer;
    /**
     * The length of the binary buffer in bits
     */
    private int length = 0;
    /** 
     * Marks the buffer as complete and makes it read-only
     */
    private boolean complete = false;
    /**
     * The chached hexadecimal string representation of the completed buffer
     */
    private String cachedHexString = null;

    /* CONSTRUCTORS */

    /**
     * Constructs a new empty Whiteflag binary encoded message buffer
     * @param message a byte array
     */
    private WfBinaryBuffer() {
        this.buffer = new byte[0];
    }

    /**
     * Constructs a new Whiteflag binary encoded message buffer from a byte array with specified bit length
     * @param byteArray a byte array
     * @param bitLength the bit length, i.e. the number of used bits
     * @param complete boolean indicating if the the buffer should ne marked as complete to make it read-only
     */
    private WfBinaryBuffer(final byte[] byteArray, final int bitLength, final boolean complete) {
        this.buffer = Arrays.copyOf(byteArray, byteLength(bitLength));
        this.length = bitLength;
        this.complete = complete;
    }

    /* STATIC FACTORY METHODS */

    /**
     * Creates a new Whiteflag binary buffer
     * @return a new {@link WfBinaryBuffer}
     */
    public static final WfBinaryBuffer create() {
        return new WfBinaryBuffer();
    }

    /**
     * Creates a new Whiteflag binary encoded message buffer from a byte array
     * @param data a byte array with a binary encoded Whiteflag message data
     * @return a new {@link WfBinaryBuffer}
     */
    public static final WfBinaryBuffer fromByteArray(final byte[] data) {
        return new WfBinaryBuffer(data, (data.length * BYTE), false);
    }

    /**
     * Creates a new Whiteflag binary encoded message buffer from a byte array
     * @param data a byte array with a binary encoded Whiteflag message data
     * @param bitLength the bit length
     * @return a new {@link WfBinaryBuffer}
     */
    public static final WfBinaryBuffer fromByteArray(final byte[] data, final int bitLength) {
        if (bitLength < 0) return new WfBinaryBuffer();
        if (bitLength > (data.length * BYTE)) return fromByteArray(data);
        return new WfBinaryBuffer(data, bitLength, false);
    }

    /**
     * Creates a new Whiteflag binary encoded message buffer from a hexadecimal string
     * @param data a hexadecimal string with a binary encoded Whiteflag message data
     * @return a new {@link WfBinaryBuffer}
     */
    public static final WfBinaryBuffer fromHexString(final String data) {
        return fromByteArray(convertToByteArray(data));
    }

    /* PUBLIC METHODS */

    /**
     * Returns the bit length of the binary buffer
     * @return the buffer length in bits
     */
    public final int bitLength() {
        return +this.length;
    }

    /**
     * Returns the byte length of the binary buffer
     * @return the buffer length in bits
     */
    public final int byteLength() {
        return +this.buffer.length;
    }

    /**
     * Makes a copy of the binary buffer
     * @return a copy of this {@link WfBinaryBuffer}
     */
    public final WfBinaryBuffer copy() {
        return new WfBinaryBuffer(this.buffer, this.length, this.complete);
    }

    /**
     * Returns the Whiteflag encoded message as a byte array
     * @return a byte array with an encoded message
     */
    public final byte[] toByteArray() {
        return Arrays.copyOf(this.buffer, this.buffer.length);
    }

    /**
     * Returns the Whiteflag encoded message as a hexademical string and caches it if the buffer is marked complete
     * @return a hexadecimal string with the encoded message
     */
    public final String toHexString() {
        if (Boolean.FALSE.equals(this.complete)) {
            return convertToHexString(this.buffer);
        }
        if (this.cachedHexString == null) {
            this.cachedHexString = convertToHexString(this.buffer);
        }
        return this.cachedHexString;
    }

    /**
     * Marks the buffer as complete and makes it read-only
     * @return this {@link WfBinaryBuffer}
     */
    public final WfBinaryBuffer markComplete() {
        if (Boolean.FALSE.equals(this.complete)) {
            this.complete = true;
            this.cachedHexString = convertToHexString(this.buffer);
        }
        return this;
    }

    /**
     * Checks if the buffer is marked as complete and cannot be altered
     * @return TRUE if buffer is marked as complete
     */
    public final boolean isComplete() {
        return this.complete;
    }

    /**
     * Appends a binary buffer to this binary buffer
     * @param binaryBuffer the {@link WfBinaryBuffer} to append to this binary buffer
     * @return this {@link WfBinaryBuffer}
     * @throws IllegalStateException if the buffer is marked complete and cannot be altered
     */
    public final WfBinaryBuffer append(final WfBinaryBuffer binaryBuffer) throws IllegalStateException {
        return appendBits(binaryBuffer.toByteArray(), binaryBuffer.bitLength());
    }

    /**
     * Encodes a Whiteflag message field and adds it to the end of the binary buffer
     * @param field the next {@link WfMessageField} to be encoded and added to the buffer
     * @return this {@link WfBinaryBuffer}
     * @throws WfCoreException if field connot be encoded
     * @throws IllegalStateException if the buffer is marked complete and cannot be altered
     */
    public final WfBinaryBuffer addMessageField(WfMessageField field) throws WfCoreException {
        return appendBits(field.encode(), field.bitLength());
    }

    /**
     * Extracts and decodes a Whiteflag message field from the binary buffer
     * @param field the {@link WfMessageField} to be extracted and decoded
     * @param startBit the bit where the encoded field is located in the buffer
     * @return String with the decoded field value
     * @throws WfCoreException if field connot be decoded
     */
    public final WfMessageField extractMessageField(WfMessageField field, final int startBit) throws WfCoreException {
        byte[] data;
        if (field.bitLength() < 1) {
            int bitLength = this.length - startBit;
            bitLength -= bitLength % field.encoding.bitLength();    // Remove excess bits, such as padding zeros
            data = extractBits(startBit, bitLength);
        } else {
            data = extractBits(startBit, field.bitLength());
        }
        field.decode(data);
        return field;
    }

    /* PUBLIC STATIC METHODS */

    /**
     * Checks for and removes prefix from string
     * @param str string to be checked
     * @param prefix the prefix to be checked for
     * @return a string without prefix
     */
    public static final String removeStringPrefix(final String str, final String prefix) {
        if (str == null) return "";
        if (prefix == null) return str;
        if (str.startsWith(prefix)) return str.substring(prefix.length());
        return str;
    }

    /**
     * Converts a hexadecimal string to a byte array
     * @param hexstr the hexadecimal string
     * @return a byte array
     * @throws IllegalArgumentException if argument is not a hexadecimal string
     */
    public static final byte[] convertToByteArray(final String hexstr) {
        /* Prepare string by removing prefix and adding trailing 0 */
        String str = removeStringPrefix(hexstr, HEXPREFIX);
        if (str.length() % 2 == 1) str = str + "0";
        if (!HEXPATTERN.matcher(str).matches()) {
            throw new IllegalArgumentException("Invalid hexadecimal string");
        }
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
    public static final byte[] shiftRight(final byte[] byteArray, int shift) {
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
    public static final byte[] shiftLeft(final byte[] byteArray, int shift) {
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
        return cropBits(newByteArray, -(shift % BYTE));
    }

    /**
     * Appends a bytes array to the binary buffer
     * @param byteArray the byte array with the bits to be appended
     * @return this {@link WfBinaryBuffer}
     * @throws IllegalStateException if the buffer is marked complete and cannot be altered
     */
    public final WfBinaryBuffer appendBits(final byte[] byteArray) {
        appendBits(byteArray, (byteArray.length * BYTE));
        return this;
    }

    /**
     * Appends the specified number of bits from a bytes array to the binary buffer
     * @param byteArray the byte array with the bits to be appended
     * @param nBits the number of bits to be appended from the byte array
     * @return this {@link WfBinaryBuffer}
     * @throws IllegalStateException if the buffer is marked complete and cannot be altered
     */
    public final WfBinaryBuffer appendBits(final byte[] byteArray, int nBits) {
        /* Check if buffer is complete and cannot be altered */
        if (this.complete) throw new IllegalStateException("Binary buffer marked as complete and cannot be altered", null);

        /* Check number of bits */
        if (nBits > (byteArray.length * BYTE)) nBits = byteArray.length * BYTE;

        /* Add bits to the end of the buffer */
        this.buffer = concatinateBits(this.buffer, this.length, byteArray, nBits);
        this.length += nBits;
        return this;
    }

    /**
     * Returns a byte array with a subset of the bits in the buffer from the specified start bit to the end
     * @param startBit the first bit of the subset to extract
     * @return a byte array with the extracted bits
     */
    public final byte[] extractBits(final int startBit) {
        return extractBits(startBit, (this.length - startBit));
    }

    /**
     * Returns a byte array with a subset of the bits in the buffer
     * @param startBit the first bit of the subset to extract
     * @param bitLength the length of the subset, i.e. the number of bits to extract
     * @return a byte array with the extracted bits
     */
    public final byte[] extractBits(int startBit, int bitLength) {
        /* Check subset range */
        if (startBit < 0) startBit = 0;
        if (bitLength < 1 || bitLength > (this.length - startBit)) {
            bitLength = (this.length - startBit);
        }
        /* Calculate parameters */
        final int startByte = startBit / BYTE;
        final int byteLength = byteLength(bitLength);
        final int shift = startBit % BYTE;
        final byte mask = (byte) (0xFF << (BYTE - shift));

        /* Create new byte array with the subset */ 
        byte[] newByteArray = new byte[byteLength];
        if (shift == 0) {
            /* Faster loop if no shitft needed */
            for (int byteIndex = 0; byteIndex < byteLength; byteIndex++) {
                newByteArray[byteIndex] = this.buffer[startByte + byteIndex];
            }
        } else {
            /* Loop through bytes to shift */
            for (int byteIndex = 0; byteIndex < byteLength; byteIndex++) {
                newByteArray[byteIndex] = (byte) ((0xFF & this.buffer[startByte + byteIndex]) << shift);
            }
            final int endByte = byteLength < (this.buffer.length - startByte) ? byteLength : (byteLength - 1);
            for (int byteIndex = 0; byteIndex < endByte; byteIndex++) {
                newByteArray[byteIndex] |= (byte) ((0xFF & this.buffer[startByte + byteIndex + 1] & mask) >>> (BYTE - shift));
            }
        }
        return cropBits(newByteArray, bitLength);
    }

    /* PROTECTED STATIC METHODS */

    /**
     * Concatinates two bitsets
     * @param byteArray1 byte array containing the first bitset
     * @param nBits1 number of bits in the first bitset, i.e. which bits to take from the first byte array
     * @param byteArray2 byte array containing the second bitset
     * @param nBits2 number of bits in the second bitset, i.e. which bits to take from the second byte array
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
        final int byteLength = byteLength(bitLength);

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
        return cropBits(newByteArray, bitLength);
    }

    /**
     * Shortens the byte array to fit the length of the used bits
     * @param byteArray the byte array containing the bitset
     * @param bitLength the bit length of the buffer (i.e. the number of used bits in the bitset), or, if negative, the number of bits to remove (i.e. the number of unused bits in the bitset)
     * @return the byte array with the unused bits cleared
     */
    protected static final byte[] cropBits(byte[] byteArray, final int bitLength) {
        /* Nothing happnes if bit length is 0 */
        if (bitLength == 0) return byteArray;

        /* Determine resulting byte array length and bits to clear */
        int byteLength;
        int clearBits;
        if (bitLength > 0) {
            byteLength = byteLength(bitLength);
            if (byteLength > byteArray.length) return byteArray;
            clearBits = BYTE - (bitLength % BYTE);
        } else {
            byteLength = byteArray.length - (-bitLength / BYTE);
            if (byteLength < 1) return new byte[0];
            clearBits = -bitLength;
        }
        /* Shorten byte array */
        if (byteLength < byteArray.length) {
            byteArray = Arrays.copyOf(byteArray, byteLength);
        }
        /* Clear unused bits in last byte */
        if (clearBits < BYTE) {
            byteArray[byteArray.length - 1] &= (byte) (0xFF << clearBits);
        }
        /* All done */
        return byteArray;
    }

    /* PRIVATE STATIC METHODS */

    /**
     * Calculates the number of bytes required to hold the given number of bits
     * @param bitLength the bit length of a buffer
     * @return the byte length of the buffer
     */
    private static final int byteLength(final int bitLength) {
        return (bitLength / BYTE) + ((bitLength % BYTE) > 0 ? 1 : 0);
    }
}
