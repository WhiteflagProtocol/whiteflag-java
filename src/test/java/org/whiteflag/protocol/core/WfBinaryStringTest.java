/*
 * Whiteflag Java Library tests
 */
package org.whiteflag.protocol.core;

import org.junit.Test;
import static org.junit.Assert.*;

/**
 * Whiteflag binary string test class
 */
public class WfBinaryStringTest {
    /* Test data */
    String binString = "10100111010100101010010111";
    
    /**
     * Tests construction of BinString
     */
    @Test
    public void testBinString1() {
        /* Test function */
        WfBinaryString wfBinString1 = new WfBinaryString(binString);
        assertEquals("Binary string should contain correct data", binString, wfBinString1.toBinString());
    }

    /**
     * Tests construction of BinString with invalid data
     */
    @Test
    public void testBinString2() {
        /* Test data */
        String binString2 = "020100111011001";
        WfBinaryString wfBinString2;
        /* Test function */
        try {
            wfBinString2 = new WfBinaryString(binString2);
            fail("Expected a IllegalArgumentException to be thrown");
            assertNotEquals("Binary string should not be created", binString2, wfBinString2.toBinString());
        } catch (IllegalArgumentException e) {
            assertTrue(e instanceof IllegalArgumentException);
        }
    }

    /**
     * Tests substring of a BinString
     */
    @Test
    public void testBinString3() {
        /* Test data */
        WfBinaryString wfBinString3 = new WfBinaryString(binString);
        /* Test function */
        assertEquals("Binary string should return correct substring", binString.subSequence(0, 8), wfBinString3.sub(0, 8).toBinString());
    }

    /**
     * Tests hexadecimal encoding of a BinString
     */
    @Test
    public void testBinStringHex1() {
        /* Test data */
        WfBinaryString wfBinaryStringH1 = new WfBinaryString(binString);
        /* Test function */
        assertEquals("Binary string should return correct hexadecimal string", "a752a5c0", wfBinaryStringH1.toHexString());
    }

    /**
     * Tests hexadecimal encoding of a BinString
     */
    @Test
    public void testBinStringHex2() {
        /* Test data */
        WfBinaryString wfBinaryStringH2 = new WfBinaryString("1");
        /* Test function */
        assertEquals("Binary string should return correct hexadecimal string", "80", wfBinaryStringH2.toHexString());
    }

    /**
     * Tests addition of a BinString to another
     */
    @Test
    public void testBinStringAdd() {
        /* Test data */
        String binStringA = "11111111";
        WfBinaryString wfBinaryString = new WfBinaryString(binString);
        WfBinaryString wfBinaryStringA = new WfBinaryString(binStringA);
        /* Test function */
        wfBinaryString.add(wfBinaryStringA);
        assertEquals("Binary string should include the added data", binString + binStringA, wfBinaryString.toBinString());
    }
}
