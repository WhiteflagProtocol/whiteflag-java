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
    public void testBinString1() throws WfCoreException {
        /* Test function */
        WfBinaryString wfBinString1 = new WfBinaryString(binString);
        assertEquals("Binary string should contain correct data", binString, wfBinString1.toString());
    }

    /**
     * Tests construction of BinString with invalid data
     */
    @Test
    public void testBinString2() throws WfCoreException {
        /* Test data */
        String binString2 = "020100111011001";
        WfBinaryString wfBinString2;
        /* Test function */
        try {
            wfBinString2 = new WfBinaryString(binString2);
            fail("Expected a WfCoreException to be thrown");
            assertNotEquals("Binary string should not be created", binString2, wfBinString2.toString());
        } catch (WfCoreException e) {
            assertTrue(e instanceof WfCoreException);
        }
    }

    /**
     * Tests substring of a BinString
     */
    @Test
    public void testBinString3() throws WfCoreException {
        /* Test data */
        WfBinaryString wfBinString3 = new WfBinaryString(binString);
        /* Test function */
        assertEquals("Binary string should return correct substring", binString.subSequence(0, 8), wfBinString3.substring(0, 8).toString());
    }

    /**
     * Tests hexadecimal encoding of a BinString
     */
    @Test
    public void testBinStringHex1() throws WfCoreException {
        /* Test data */
        WfBinaryString WfBinaryStringH1 = new WfBinaryString(binString);
        /* Test function */
        assertEquals("Binary string should return correct hexadecimal string", "a752a5c0", WfBinaryStringH1.toHexString());
    }

    /**
     * Tests hexadecimal encoding of a BinString
     */
    @Test
    public void testBinStringHex2() throws WfCoreException {
        /* Test data */
        WfBinaryString WfBinaryStringH2 = new WfBinaryString("1");
        /* Test function */
        assertEquals("Binary string should return correct hexadecimal string", "80", WfBinaryStringH2.toHexString());
    }

    /**
     * Tests addition of a BinString to another
     */
    @Test
    public void testBinStringAdd() throws WfCoreException {
        /* Test data */
        String binStringA = "11111111";
        WfBinaryString WfBinaryString = new WfBinaryString(binString);
        WfBinaryString WfBinaryStringA = new WfBinaryString(binStringA);
        /* Test function */
        WfBinaryString.add(WfBinaryStringA);
        assertEquals("Binary string should include the added data", binString + binStringA, WfBinaryString.toString());
    }
}
