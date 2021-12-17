/*
 * Whiteflag Java Library tests
 */
package org.whiteflagprotocol.java.core;

import org.junit.Test;
import static org.junit.Assert.*;

/* Field encodings required for field definitions */
import static org.whiteflagprotocol.java.core.WfMessageCodec.Encoding.*;

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

        /* Verify */
        assertArrayEquals("Byte array should be indentical after zero shift", byteArray, WfBinaryBuffer.shiftRight(byteArray, 0));
    }

    /**
     * Tests left shift of byte array
     */
    @Test
    public void testByteArrayShiftLeft0() {
        /* Setup */ 
        final byte[] byteArray = {(byte) 0x53,(byte) 0x7D};

        /* Verify */
        assertArrayEquals("Byte array should be indentical after zero shift", byteArray, WfBinaryBuffer.shiftLeft(byteArray, 0));
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
        assertArrayEquals("Byte array should have been correctly shifted right by 3 bits", result, WfBinaryBuffer.shiftRight(byteArray, 3));
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
        assertArrayEquals("Byte array should have been correctly shifted right by 5 bits", result, WfBinaryBuffer.shiftRight(byteArray, 5));
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
    public void testByteArrayShiftLeft1() {
        /* Setup */ 
        final byte[] byteArray = {(byte) 0x53,(byte) 0x7D};     // 0101 0011 | 0111 1101
        final byte[] result = {(byte) 0x9B,(byte) 0xE8};        // 1001 1011 | 1110 1000

        /* Verify */
        System.out.println("Byte Array 1: " + WfBinaryBuffer.convertToHexString(byteArray));
        System.out.println("Left shift 1: " + WfBinaryBuffer.convertToHexString(WfBinaryBuffer.shiftLeft(byteArray, 3)));
        System.out.println("Expected    : " + WfBinaryBuffer.convertToHexString(result));
        assertArrayEquals("Byte array should have been correctly shifted left by 3 bits", result, WfBinaryBuffer.shiftLeft(byteArray, 3));
    }

    /**
     * Tests right shift of byte array
     */
    @Test
    public void testByteArrayShiftLeft2() {
        /* Setup */ 
        final byte[] byteArray = {(byte) 0xE6,(byte) 0x38,(byte) 0x87};     // 1110 0110 | 0011 1000 | 1000 0111
        final byte[] result = {(byte) 0x1C,(byte) 0x43,(byte) 0x80};        // 0001 1100 | 0100 0011 | 1000 0000

        /* Verify */
        System.out.println("Byte Array 2: " + WfBinaryBuffer.convertToHexString(byteArray));
        System.out.println("Left shift 2: " + WfBinaryBuffer.convertToHexString(WfBinaryBuffer.shiftLeft(byteArray, 7)));
        System.out.println("Expected    : " + WfBinaryBuffer.convertToHexString(result));
        assertArrayEquals("Byte array should have been correctly shifted left by 7 bits", result, WfBinaryBuffer.shiftLeft(byteArray, 7));
    }

    /**
     * Tests right shift of byte array
     */
    @Test
    public void testByteArrayShiftLeft3() {
        /* Setup */ 
        final byte[] byteArray = {(byte) 0xD4,(byte) 0x4B,(byte) 0x93, (byte) 0x93};     // 1101 0100 | 0100 1011 | 1001 0011 | 1001 0011
        final byte[] result = {(byte) 0x89,(byte) 0x72,(byte) 0x72, (byte) 0x60};        // 1000 1001 | 0111 0010 | 0111 0010 | 0110 0000

        /* Verify */
        System.out.println("Byte Array 3: " + WfBinaryBuffer.convertToHexString(byteArray));
        System.out.println("Left shift 3: " + WfBinaryBuffer.convertToHexString(WfBinaryBuffer.shiftLeft(byteArray, 5)));
        System.out.println("Expected    : " + WfBinaryBuffer.convertToHexString(result));
        assertArrayEquals("Byte array should have been correctly shifted left by 7 bits", result, WfBinaryBuffer.shiftLeft(byteArray, 5));
    }

    /**
     * Tests right shift of byte array
     */
    @Test
    public void testByteArrayShiftLeft4() {
        /* Setup */ 
        final byte[] byteArray = {(byte) 0xE6,(byte) 0x38,(byte) 0x87, (byte) 0x0f};     // 1110 0110 | 0011 1000 | 1000 0111 | 1000 0111
        final byte[] result = {(byte) 0x63,(byte) 0x88,(byte) 0x70, (byte) 0xf0};        // 0110 0011 | 1000 1000 | 0111 1000 | 0111 0000

        /* Verify */
        System.out.println("Byte Array 4: " + WfBinaryBuffer.convertToHexString(byteArray));
        System.out.println("Left shift 4: " + WfBinaryBuffer.convertToHexString(WfBinaryBuffer.shiftLeft(byteArray, 4)));
        System.out.println("Expected    : " + WfBinaryBuffer.convertToHexString(result));
        assertArrayEquals("Byte array should have been correctly shifted left by 4 bits", result, WfBinaryBuffer.shiftLeft(byteArray, 4));
    }

    /**
     * Tests addition of bits
     */
    @Test
    public void testAppendBits1() {
        /* Setup */
        final byte[] byteArray1 = {(byte) 0xE6,(byte) 0x38,(byte) 0x87};    // 1110 0110 | 0011 1000 | 1000 0111
        final byte[] byteArray2 = {(byte) 0x6E,(byte) 0x7f};                // 0110 1110 | 0111 1111
        WfBinaryBuffer message = WfBinaryBuffer.create();

        /* Verify */
        assertEquals("Binary buffer length should be 0 bits", 0, message.length());
        message.appendBits(byteArray1, 22);         // 1110 0110 | 0011 1000 | 1000 01(00)
        assertEquals("Binary buffer length should be 22 bits", 22, message.length());
        assertTrue("Byte array 1 should have been correctly added to the binary buffer", message.toHexString().equalsIgnoreCase("e63884"));
        message.appendBits(byteArray2, 13);         // 1110 0110 | 0011 1000 | 1000 0101 | 1011 1001 | 1110 0000
        assertEquals("Binary buffer length should be 34 bits", 35, message.length());
        assertTrue("Byte array 2 should have been correctly added to the binary buffer", message.toHexString().equalsIgnoreCase("e63885b9e0"));
    }

    /**
     * Tests right shift of byte array
     */
    @Test
    public void testAppendBits2() {
        /* Setup */
        final byte[] byteArray1 = {(byte) 0xE6,(byte) 0x38,(byte) 0x87};    // 1110 0110 | 0011 1000 | 1000 0111
        final byte[] byteArray2 = {(byte) 0x6E,(byte) 0x6f};                // 0110 1110 | 0110 1111
        WfBinaryBuffer message = WfBinaryBuffer.fromByteArray(byteArray1);

        /* Verify */
        assertEquals("Binary buffer length should be 24 bits", 24, message.length());
        assertTrue("Byte array 1 should have been correctly added to the binary buffer", message.toHexString().equalsIgnoreCase("e63887"));
        message.appendBits(byteArray2, 12);         // 1110 0110 | 0011 1000 | 1000 0111 | 0110 1110 | 0110 0000
        assertEquals("Binary buffer length should be 36 bits", 36, message.length());
        assertTrue("Byte array 2 should have been correctly added to the binary buffer", message.toHexString().equalsIgnoreCase("e638876e60"));
    }

        /**
     * Tests right shift of byte array
     */
    @Test
    public void testAppendBits3() {
        /* Setup */
        final byte[] byteArray1 = {(byte) 0xDD,(byte) 0xFF};    // 1101 1101 | 1111 1111
        final byte[] byteArray2 = {(byte) 0xBF};                // 1011 1111
        WfBinaryBuffer message = WfBinaryBuffer.create();

        /* Verify */
        assertEquals("Binary buffer length should be 0 bits", 0, message.length());
        message.appendBits(byteArray1, 4);         // 1101 0000
        assertEquals("Binary buffer length should be 4 bits", 4, message.length());
        assertTrue("Byte array 1 should have been correctly added to the binary buffer", message.toHexString().equalsIgnoreCase("d0"));
        message.appendBits(byteArray2, 3);         // 1101 1010
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
