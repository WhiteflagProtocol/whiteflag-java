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
        assertArrayEquals("Byte array should have been correctly shifted left by -3 bits", result, WfBinaryBuffer.shiftLeft(byteArray, -3));
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
        assertArrayEquals("Byte array should have been correctly shifted right by 12 bits", result, WfBinaryBuffer.shiftRight(byteArray, 12));
        assertArrayEquals("Byte array should have been correctly shifted left by -12 bits", result, WfBinaryBuffer.shiftLeft(byteArray, -12));
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
        assertArrayEquals("Byte array should have been correctly shifted left by 3 bits", result, WfBinaryBuffer.shiftLeft(byteArray, 3));
        assertArrayEquals("Left by 3 bits should be equal to right shift by -11 bits", result, WfBinaryBuffer.shiftRight(byteArray, -11));
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
        assertArrayEquals("Byte array should have been correctly shifted left by 4 bits", result, WfBinaryBuffer.shiftLeft(byteArray, 4));
    }

    /**
     * Tests addition of bits
     */
    @Test
    public void testAppendBits1() throws WfCoreException {
        /* Setup */
        final byte[] byteArray1 = {(byte) 0xE6,(byte) 0x38,(byte) 0x87};    // 1110 0110 | 0011 1000 | 1000 0111
        final byte[] byteArray2 = {(byte) 0x6E,(byte) 0x7f};                // 0110 1110 | 0111 1111
        WfBinaryBuffer buffer = WfBinaryBuffer.create();

        /* Verify */
        assertEquals("Binary buffer length should be 0 bits", 0, buffer.bitLength());
        buffer.appendBits(byteArray1, 22);         // 1110 0110 | 0011 1000 | 1000 01(00)
        assertEquals("Binary buffer length should be 22 bits", 22, buffer.bitLength());
        assertTrue("Byte array 1 should have been correctly added to the binary buffer", buffer.toHexString().equalsIgnoreCase("e63884"));
        buffer.appendBits(byteArray2, 13);         // 1110 0110 | 0011 1000 | 1000 0101 | 1011 1001 | 1110 0000
        assertEquals("Binary buffer length should be 34 bits", 35, buffer.bitLength());
        assertTrue("Byte array 2 should have been correctly added to the binary buffer", buffer.toHexString().equalsIgnoreCase("e63885b9e0"));
    }

    /**
     * Tests addition of bits
     */
    @Test
    public void testAppendBits2() throws WfCoreException {
        /* Setup */
        final byte[] byteArray1 = {(byte) 0xE6,(byte) 0x38,(byte) 0x87};    // 1110 0110 | 0011 1000 | 1000 0111
        final byte[] byteArray2 = {(byte) 0x6E,(byte) 0x6f};                // 0110 1110 | 0110 1111
        WfBinaryBuffer buffer = WfBinaryBuffer.fromByteArray(byteArray1);

        /* Verify */
        assertEquals("Binary buffer length should be 24 bits", 24, buffer.bitLength());
        assertTrue("Byte array 1 should have been correctly added to the binary buffer", buffer.toHexString().equalsIgnoreCase("e63887"));
        buffer.appendBits(byteArray2, 12);         // 1110 0110 | 0011 1000 | 1000 0111 | 0110 1110 | 0110 0000
        assertEquals("Binary buffer length should be 36 bits", 36, buffer.bitLength());
        assertTrue("Byte array 2 should have been correctly added to the binary buffer", buffer.toHexString().equalsIgnoreCase("e638876e60"));
    }

    /**
     * Tests addition of bits
     */
    @Test
    public void testAppendBits3() throws WfCoreException {
        /* Setup */
        final byte[] byteArray1 = {(byte) 0xDD,(byte) 0xFF};    // 1101 1101 | 1111 1111
        final byte[] byteArray2 = {(byte) 0xBF};                // 1011 1111
        WfBinaryBuffer buffer = WfBinaryBuffer.create();

        /* Verify */
        assertEquals("Binary buffer length should be 0 bits", 0, buffer.bitLength());
        buffer.appendBits(byteArray1, 4);         // 1101 0000
        assertEquals("Binary buffer length should be 4 bits", 4, buffer.bitLength());
        assertTrue("Byte array 1 should have been correctly added to the binary buffer", buffer.toHexString().equalsIgnoreCase("d0"));
        buffer.appendBits(byteArray2, 3);         // 1101 1010
        assertEquals("Binary buffer length should be 7 bits", 7, buffer.bitLength());
        assertTrue("Byte array 2 should have been correctly added to the binary buffer", buffer.toHexString().equalsIgnoreCase("da"));
    }

    /**
     * Tests extraction of bits
     */
    @Test
    public void testExtractBits1() {
        /* Setup */
        final byte[] byteArray = {(byte) 0xDD,(byte) 0xFF};    // 110|1110111|111111
        final byte[] result = {(byte) 0xEE};                   //    |1110111|0
        WfBinaryBuffer buffer = WfBinaryBuffer.fromByteArray(byteArray);

        /* Verify */
        assertArrayEquals("Should have correctly extracted 7 bits from binary buffer", result, buffer.extractBits(3, 7));
    }

    /**
     * Tests extraction of bits
     */
    @Test
    public void testExtractBits2() {
        /* Setup */
        final byte[] byteArray = {(byte) 0xDD,(byte) 0xE7,(byte) 0xD0};    // 1101110111100|111|11010000
        final byte[] result = {(byte) 0xE0};                               //              |111|00000
        WfBinaryBuffer buffer = WfBinaryBuffer.fromByteArray(byteArray);

        /* Verify */
        assertArrayEquals("Should have correctly extracted 3 bits from binary buffer", result, buffer.extractBits(13, 3));
    }

    /**
     * Tests extraction of bits
     */
    @Test
    public void testExtractBits3() {
        /* Setup */
        final byte[] byteArray = {(byte) 0x95, (byte) 0xDD,(byte) 0xFF,(byte) 0xE7};    // 1001010111|0111011111|111111100111
        final byte[] result = {(byte) 0x77,(byte) 0xC0};                                //           |0111011111|000000
        WfBinaryBuffer buffer = WfBinaryBuffer.fromByteArray(byteArray);

        /* Verify */
        assertArrayEquals("Should have correctly extracted 10 bits from binary buffer", result, buffer.extractBits(10, 10));
    }

    /**
     * Tests extraction of bits
     */
    @Test
    public void testExtractBits4() {
        /* Setup */
        final byte[] byteArray = {(byte) 0x95, (byte) 0xDD,(byte) 0xFF,(byte) 0xE7};    // 100101011101|1101111111111110|0111
        final byte[] result = {(byte) 0xDF,(byte) 0xFE};                                //             |1101111111111110|
        WfBinaryBuffer buffer = WfBinaryBuffer.fromByteArray(byteArray);

        /* Verify */
        assertArrayEquals("Should have correctly extracted 10 bits from binary buffer", result, buffer.extractBits(12, 16));
    }

    /**
     * Tests addition / encoding of UTF field
     */
    @Test
    public void testAddFieldUTF() throws WfCoreException {
        WfBinaryBuffer buffer = WfBinaryBuffer.create();
        WfMessageField field = WfMessageField.define(FIELDNAME, null, UTF8, 0, -1);

        /* Verify */
        assertTrue("Should be able to set field value", field.set("text"));
        buffer.addMessageField(field);
        assertEquals("Binary buffer length should be equal to field length", field.bitLength(), buffer.bitLength());
        assertTrue("Message field (UTF) should be correctly encoded and added", buffer.toHexString().equalsIgnoreCase("74657874"));
    }

    /**
     * Tests addition / encoding of UTF field
     */
    @Test
    public void testExtractFieldUTF() throws WfCoreException {
        final byte[] byteArray = {(byte) 0x95, (byte) 0x74,(byte) 0x78,(byte) 0x74};  //  .......1 10|01 0001 11|11
        WfBinaryBuffer buffer = WfBinaryBuffer.fromByteArray(byteArray);
        WfMessageField field = WfMessageField.define(FIELDNAME, null, UTF8, 0, -1);

        /* Verify */
        assertEquals("Extracted message field (UTF) should contain the correct value", "txt", buffer.extractMessageField(field, 8).get());    
    }

    /**
     * Tests addition / encoding of binary field
     */
    @Test
    public void testAddFieldBin1() throws WfCoreException {
        /* Setup */
        WfBinaryBuffer buffer = WfBinaryBuffer.create();
        WfMessageField field = WfMessageField.define(FIELDNAME, null, BIN, 0, 2);

        /* Verify */
        assertTrue("Should be able to set field value", field.set("01"));
        buffer.addMessageField(field);
        assertEquals("Binary buffer length should be equal to field length", field.bitLength(), buffer.bitLength());
        assertTrue("Message field (bin) should be correctly encoded and added", buffer.toHexString().equalsIgnoreCase("40"));
    }

    /**
     * Tests addition / encoding of binary field
     */
    @Test
    public void testAddFieldBin2() throws WfCoreException {
        /* Setup */
        WfBinaryBuffer buffer = WfBinaryBuffer.create();
        WfMessageField field = WfMessageField.define(FIELDNAME, null, BIN, 0, 3);

        /* Verify */
        assertTrue("Should be able to set field value", field.set("101"));
        buffer.addMessageField(field);
        assertEquals("Binary buffer length should be equal to field length", field.bitLength(), buffer.bitLength());
        assertTrue("Message field (bin) should be correctly encoded and added", buffer.toHexString().equalsIgnoreCase("a0"));
    }

    /**
     * Tests addition / encoding of decimal field
     */
    @Test
    public void testAddFieldDec() throws WfCoreException {
        /* Setup */
        WfBinaryBuffer buffer = WfBinaryBuffer.create();
        WfMessageField field = WfMessageField.define(FIELDNAME, null, DEC, 0, 4);

        /* Verify */
        assertTrue("Should be able to set field value", field.set("1478"));
        buffer.addMessageField(field);
        assertEquals("Binary buffer length should be equal to field length", field.bitLength(), buffer.bitLength());
        assertTrue("Message field (dec) should be correctly encoded and added", buffer.toHexString().equalsIgnoreCase("1478"));
    }

    /**
     * Tests addition / encoding of decimal field
     */
    @Test
    public void testExtractFieldDec() throws WfCoreException {
        /* Setup */
        final byte[] byteArray = {(byte) 0x95, (byte) 0x91,(byte) 0xFF,(byte) 0xE7};  //  .......1 10|01 0001 11|11
        WfBinaryBuffer buffer = WfBinaryBuffer.fromByteArray(byteArray);
        WfMessageField field = WfMessageField.define(FIELDNAME, null, DEC, 0, 2);

        /* Verify */
        assertEquals("Extracted message field (dec) should contain the correct value", "47", buffer.extractMessageField(field, 10).get());
    }

    /**
     * Tests addition / encoding of hexadecimal field
     */
    @Test
    public void testAddFieldHex() throws WfCoreException {
        /* Setup */
        WfBinaryBuffer buffer = WfBinaryBuffer.create();
        WfMessageField field = WfMessageField.define(FIELDNAME, null, HEX, 0, 4);

        /* Verify */
        assertTrue("Should be able to set field value", field.set("3f8C"));
        buffer.addMessageField(field);
        assertEquals("Binary buffer length should be equal to field length", field.bitLength(), buffer.bitLength());
        assertTrue("Message field (hex) should be correctly encoded and added", buffer.toHexString().equalsIgnoreCase("3f8c"));
    }

    /**
     * Tests addition / encoding of hexadecimal field
     */
    @Test
    public void testExtractFieldHex() throws WfCoreException {
        /* Setup */
        final byte[] byteArray = {(byte) 0x95, (byte) 0xDD,(byte) 0xFF,(byte) 0xE7};  //  ........ 1|1011 1011 |111
        WfBinaryBuffer buffer = WfBinaryBuffer.fromByteArray(byteArray);
        WfMessageField field = WfMessageField.define(FIELDNAME, null, HEX, 0, 2);

        /* Verify */
        assertEquals("Extracted message field (hex) should contain the correct value", "bb", buffer.extractMessageField(field, 9).get());
    }

    /**
     * Tests addition / encoding of date-time field
     */
    @Test
    public void testAddFieldDateTime() throws WfCoreException {
        /* Setup */
        WfBinaryBuffer buffer = WfBinaryBuffer.create();
        WfMessageField field = WfMessageField.define(FIELDNAME, null, DATETIME, 0, -1);

        /* Verify */
        assertTrue("Should be able to set field value", field.set("2020-07-01T21:42:23Z"));
        buffer.addMessageField(field);
        assertEquals("Binary buffer length should be equal to field length", field.bitLength(), buffer.bitLength());
        assertTrue("Message field (datum) should be correctly encoded and added", buffer.toHexString().equalsIgnoreCase("20200701214223"));
    }
}
