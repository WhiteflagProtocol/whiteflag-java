/*
 * Whiteflag Java Library tests
 */
package org.whiteflagprotocol.java.core;

import org.junit.Test;
import static org.junit.Assert.*;

/* Field encodings required for field definitions */
import static org.whiteflagprotocol.java.core.WfMessageField.Encoding.*;

/**
 * Whiteflag binary string test class
 */
public class WfBinaryBufferTest {
    /* Fieldname */
    private final String FIELDNAME = "TESTFIELD";

    /* Regex field begin and end */
    private final String BEGIN = "^";
    private final String REPEAT = "*$";
    private final String END = "$";

    /**
     * Tests right shift of byte array
     */
    @Test
    public void testByteArrayShiftRight0() {
        /* Setup */ 
        final byte[] byteArray = {(byte) 0x53,(byte) 0x7D};
        final byte[] result = byteArray;

        /* Verify */
        assertArrayEquals("Byte array should be indentical after zero shift", byteArray, WfBinaryBuffer.shiftRight(byteArray, 0));
    }

    /**
     * Tests right shift of byte array
     */
    @Test
    public void testByteArrayShiftRight1() {
        /* Setup */ 
        final byte[] byteArray = {(byte) 0x53,(byte) 0x7D};             //    0101001101111101
        final byte[] result = {(byte) 0x0A,(byte) 0x6F,(byte) 0xA0};    // 000010100110111110100000

        /* Verify */
        assertArrayEquals("Byte array should have been correctly shifted right", result, WfBinaryBuffer.shiftRight(byteArray, 3));
    }

    /**
     * Tests right shift of byte array
     */
    @Test
    public void testByteArrayShiftRight2() {
        /* Setup */
        final byte[] byteArray = {(byte) 0xF6,(byte) 0x38,(byte) 0x6D};             //      111101100011100001101101
        final byte[] result = {(byte) 0x07,(byte) 0xB1,(byte) 0xC3,(byte) 0x68};    // 00000111101100011100001101101000

        /* Verify */
        assertArrayEquals("Byte array should have been correctly shifted right", result, WfBinaryBuffer.shiftRight(byteArray, 5));
    }

    /**
     * Tests right shift of byte array
     */
    @Test
    public void testByteArrayShiftRight3() {
        /* Setup */
        final byte[] byteArray = {(byte) 0xE6,(byte) 0x38,(byte) 0x6D,(byte) 0x84};             //     11100110001110000110110110000100
        final byte[] result = {(byte) 0x0E,(byte) 0x63,(byte) 0x86,(byte) 0xD8,(byte) 0x40};    // 0000111001100011100001101101100001000000

        /* Verify */
        assertArrayEquals("Byte array should have been correctly shifted right", result, WfBinaryBuffer.shiftRight(byteArray, 4));
    }

    /**
     * Tests right shift of byte array
     */
    @Test
    public void testAddBits1() {
        /* Setup */
        final byte[] byteArray1 = {(byte) 0xE6,(byte) 0x38,(byte) 0x87};    // 1110 0110 | 0011 1000 | 1000 0111
        final byte[] byteArray2 = {(byte) 0x6E,(byte) 0x7f};                // 0110 1110 | 0111 1111
        WfBinaryBuffer message = WfBinaryBuffer.create();

        /* Verify */
        assertFalse("Message should not yet be encoded", message.isEncoded());
        assertEquals("Binary buffer length should be 0 bits", 0, message.length());
        message.addBits(byteArray1, 22);         // 1110 0110 | 0011 1000 | 1000 01(00)
        assertEquals("Binary buffer length should be 22 bits", 22, message.length());
        assertTrue("Byte array 1 should have been correctly added to the binary buffer", message.toHexString().equalsIgnoreCase("e63884"));
        message.addBits(byteArray2, 13);         // 1110 0110 | 0011 1000 | 1000 0101 | 1011 1001 | 1110 0000
        assertEquals("Binary buffer length should be 34 bits", 35, message.length());
        assertTrue("Byte array 2 should have been correctly added to the binary buffer", message.toHexString().equalsIgnoreCase("e63885b9e0"));
    }

    /**
     * Tests right shift of byte array
     */
    @Test
    public void testAddBits2() {
        /* Setup */
        final byte[] byteArray1 = {(byte) 0xE6,(byte) 0x38,(byte) 0x87};    // 1110 0110 | 0011 1000 | 1000 0111
        final byte[] byteArray2 = {(byte) 0x6E,(byte) 0x6f};                // 0110 1110 | 0110 1111
        WfBinaryBuffer message = WfBinaryBuffer.fromByteArray(byteArray1, false);

        /* Verify */
        assertFalse("Message should not yet be encoded", message.isEncoded());
        assertEquals("Binary buffer length should be 24 bits", 24, message.length());
        assertTrue("Byte array 1 should have been correctly added to the binary buffer", message.toHexString().equalsIgnoreCase("e63887"));
        message.addBits(byteArray2, 12);         // 1110 0110 | 0011 1000 | 1000 0111 | 0110 1110 | 0110 0000
        assertEquals("Binary buffer length should be 36 bits", 36, message.length());
        assertTrue("Byte array 2 should have been correctly added to the binary buffer", message.toHexString().equalsIgnoreCase("e638876e60"));
    }

        /**
     * Tests right shift of byte array
     */
    @Test
    public void testAddBits3() {
        /* Setup */
        final byte[] byteArray1 = {(byte) 0xDD,(byte) 0xFF};    // 1101 1101 | 1111 1111
        final byte[] byteArray2 = {(byte) 0xBF};                // 1011 1111
        WfBinaryBuffer message = WfBinaryBuffer.create();

        /* Verify */
        assertFalse("Message should not yet be encoded", message.isEncoded());
        assertEquals("Binary buffer length should be 0 bits", 0, message.length());
        message.addBits(byteArray1, 4);         // 1101 0000
        assertEquals("Binary buffer length should be 4 bits", 4, message.length());
        assertTrue("Byte array 1 should have been correctly added to the binary buffer", message.toHexString().equalsIgnoreCase("d0"));
        message.addBits(byteArray2, 3);         // 1101 1010
        assertEquals("Binary buffer length should be 7 bits", 7, message.length());
        assertTrue("Byte array 2 should have been correctly added to the binary buffer", message.toHexString().equalsIgnoreCase("da"));
    }

    /**
     * Tests basic operations
     */
    @Test
    public void testFieldEncodingUTF1() {
        WfBinaryBuffer message = WfBinaryBuffer.create();
        WfMessageField field = new WfMessageField(FIELDNAME, BEGIN+UTF8.charset()+REPEAT, UTF8, 0, -1);

        /* Verify */
        assertTrue("Should be able to set field value", field.set("text"));
        message.addMessageField(field);
        assertEquals("Binary buffer length should be equal to field length", field.bitLength(), message.length());
        assertTrue("Message field (UTF) should be correctly encoded", message.toHexString().equalsIgnoreCase("74657874"));
    }
}
