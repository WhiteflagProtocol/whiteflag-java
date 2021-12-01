/*
 * Whiteflag Java Library tests
 */
package org.whiteflagprotocol.java.core;

import org.junit.Test;
import static org.junit.Assert.*;

/**
 * Whiteflag binary string test class
 */
public class WfBinaryBufferTest {

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
}
