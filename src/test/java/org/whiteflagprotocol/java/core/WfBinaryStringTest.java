/*
 * Whiteflag Java Library tests
 */
package org.whiteflagprotocol.java.core;

import org.junit.Test;
import static org.junit.Assert.*;

/**
 * Whiteflag binary string test class
 */
public class WfBinaryStringTest {
    /* Test data */
    private final String TESTDATA1 = "10100111010100101010010111";
    private final String TESTDATA2 = "10111101";
    
    /**
     * Tests construction of a binary string object
     */
    @Test
    public void testBinString1() {
        /* Setup */
        WfBinaryString binString = new WfBinaryString(TESTDATA1);

        /* Verify */
        assertEquals("Binary string should contain correct data", TESTDATA1, binString.toBinString());
    }

    /**
     * Tests binary value of provided hex string
     */
    @Test
    public void testBinString2() {
        /* Setup */
        WfBinaryString binString = new WfBinaryString();
        binString.setHexValue("0xbd");

        /* Verify */
        assertEquals("Binary string should return correct binary string with prefix", "0b" + TESTDATA2, binString.toBinString(true));
    }

    /**
     * Tests exception with construction of a binary string object with invalid data
     */
    @Test
    public void testBinString3() {
        /* Setup */
        WfBinaryString binString;

        /* Verify */
        try {
            binString = new WfBinaryString("020100111011001");
            fail("Expected a IllegalArgumentException to be thrown");
            assertNotEquals("Binary string should not be created", "020100111011001", binString.toBinString());
        } catch (IllegalArgumentException e) {
            assertTrue(e instanceof IllegalArgumentException);
        }
    }

    /**
     * Tests substring of a binary string object with prefix
     */
    @Test
    public void testBinString4() {
        /* Setup */
        WfBinaryString binString = new WfBinaryString();
        binString.setBinValue("0b" + TESTDATA1);

        /* Verify */
        assertEquals("Binary string should return correct substring", TESTDATA1.subSequence(0, 8), binString.sub(0, 8).toBinString());
    }

    /**
     * Tests binary value of provided hex string
     */
    @Test
    public void testBinString5() {
        /* Setup */
        WfBinaryString binString = new WfBinaryString();
        binString.setHexValue("0xf4a3");

        /* Verify */
        assertEquals("Binary string should return correct binary string", "1111010010100011", binString.toBinString(false));
    }

    /**
     * Tests hexadecimal encoding of a binary string object
     */
    @Test
    public void testBinStringHex1() {
        /* Setup */
        WfBinaryString binString = new WfBinaryString(TESTDATA1);
    
        /* Verify */
        assertEquals("Binary string should return correct hexadecimal string with prefix", "0xa752a5c0", binString.toHexString(true));
    }

    /**
     * Tests hexadecimal encoding of a binary string object
     */
    @Test
    public void testBinStringHex2() {
        /* Setup */
        WfBinaryString binString = new WfBinaryString(TESTDATA2);
    
        /* Verify */
        assertEquals("Binary string should return correct hexadecimal string", "bd", binString.toHexString());
    }

    /**
     * Tests hexadecimal encoding of a binary string object
     */
    @Test
    public void testBinStringHex3() {
        /* Setup */
        WfBinaryString binString = new WfBinaryString("1");

        /* Verify */
        assertEquals("Binary string should return correct hexadecimal string", "80", binString.toHexString());
    }

    /**
     * Tests addition of a binary string object to another binary string object
     */
    @Test
    public void testBinStringAppend() {
        /* Setup */
        WfBinaryString binString1 = new WfBinaryString(TESTDATA1);
        WfBinaryString binString2 = new WfBinaryString(TESTDATA2);
        binString1.append(binString2);

        /* Verify */
        assertEquals("Binary string should include the added data", TESTDATA1 + TESTDATA2, binString1.toBinString());
    }
}
