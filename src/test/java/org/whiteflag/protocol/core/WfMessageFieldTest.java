/*
 * Whiteflag Java Library tests
 */
package org.whiteflag.protocol.core;

import org.junit.Test;
import static org.junit.Assert.*;

/**
 * Whiteflag field test class
 */
public class WfMessageFieldTest {
    /* Field name */
    private final String FIELDNAME = "TESTFIELD";

    /* Regex field begin and end */
    private final String BEGIN = "^";
    private final String TOEND = "*$";
    private final String END = "$";

    /**
     * Tests compressed binary encoding of UTF-8 field
     */
    @Test
    public void testUtfEncoding() throws WfCoreException {
        /* Setup */
        WfMessageField field = new WfMessageField(FIELDNAME, BEGIN+WfMessageField.Encoding.UTF8.charset()+TOEND, WfMessageField.Encoding.UTF8, 0, -1);
        field.setValue("WF");

        /* Verify */
        assertEquals("UTF-8 field should be correctly binary encoded", "0101011101000110", field.encode().toBinString());
        assertEquals("UTF-8 field should be correctly hexadecimal encoded", "5746", field.encode().toHexString());
        assertEquals("Unencoded UTF-8 field should be 2 bytes", 2, field.byteLength());
        assertEquals("Encoded UTF-8 field should be 16 bits bytes", 16, field.bitLength());
    }
    /**
     * Tests compressed binary decoding of UTF-8 field
     */
    @Test
    public void testUtfDecoding() throws WfCoreException {
        /* Setup */
        WfMessageField field = new WfMessageField(FIELDNAME, BEGIN+WfMessageField.Encoding.UTF8.charset()+TOEND, WfMessageField.Encoding.UTF8, 0, -1);
        WfBinaryString binString = new WfBinaryString().setHexValue("5746");
        field.setValue(field.decode(binString));

        /* Verify */
        assertEquals("UTF-8 field should be correctly decoded", "WF", field.decode(binString));
        assertEquals("UTF-8 decoded field value should be correctly set", "WF", field.getValue());
    }
    /**
     * Tests compressed binary encoding of Binary field
     */
    @Test
    public void testBinEncoding() throws WfCoreException {
        /* Setup */
        WfMessageField field = new WfMessageField(FIELDNAME, BEGIN+WfMessageField.Encoding.BIN.charset()+TOEND, WfMessageField.Encoding.BIN, 0, 8);
        field.setValue("10111011");
        
        /* Verify */
        assertEquals("Binary field should be correctly binary encoded with prefix", "0b10111011", field.encode().toBinString(true));
        assertEquals("Unencoded Binary field should be 8 bytes", 8, field.byteLength());
        assertEquals("Encoded Binary field should be 8 bits", 8, field.bitLength());
    }
    /**
     * Tests compressed binary encoding of Decimal field
     */
    @Test
    public void testDecEncoding() throws WfCoreException {
        /* Setup */
        WfMessageField field = new WfMessageField(FIELDNAME, BEGIN+WfMessageField.Encoding.DEC.charset()+TOEND, WfMessageField.Encoding.DEC, 0, 3);
        field.setValue("123");
        
        /* Verify */
        assertEquals("Decimal field should be correctly binary encoded", "000100100011", field.encode().toBinString());
        assertEquals("Unencoded Decimal field should be 3 bytes", 3, field.byteLength());
        assertEquals("Encoded Decimal field should be 12 bits", 12, field.bitLength());
    }
    /**
     * Tests decoding of Decimal field
     */
    @Test
    public void testDecDecoding() throws WfCoreException {
        /* Setup */
        WfMessageField field = new WfMessageField(FIELDNAME, BEGIN+WfMessageField.Encoding.DEC.charset()+TOEND, WfMessageField.Encoding.DEC, 0, 3);
        WfBinaryString binString = new WfBinaryString().setBinValue("000100100011");
        field.setValue(field.decode(binString));
        
        /* Verify */
        assertEquals("Decimal field should be correctly decoded", "123", field.decode(binString));
        assertEquals("Decimal decoded field value should be correctly set", "123", field.getValue());
    }
    /**
     * Tests compressed binary encoding of Hexadecimal field
     */
    @Test
    public void testHexEncoding() throws WfCoreException {
        /* Setup */
        WfMessageField field = new WfMessageField(FIELDNAME, BEGIN+WfMessageField.Encoding.HEX.charset()+TOEND, WfMessageField.Encoding.HEX, 0, 2);
        field.setValue("3f");
        
        /* Verify */
        assertEquals("Hexadecimal field should be correctly binary encoded", "00111111", field.encode().toBinString());
        assertEquals("Hexadecimal field should be correctly binary encoded with prefix", "0x3f", field.encode().toHexString(true));
        assertEquals("Unencoded Hexadecimal field should be 2 bytes", 2, field.byteLength());
        assertEquals("Encoded Hexadecimal field should be 8 bits", 8, field.bitLength());
    }
    /**
     * Tests decoding of Hexadecimal field
     */
    @Test
    public void testHexDecoding() throws WfCoreException {
        /* Test function */
        WfMessageField field = new WfMessageField(FIELDNAME, BEGIN+WfMessageField.Encoding.HEX.charset()+TOEND, WfMessageField.Encoding.HEX, 0, 2);
        WfBinaryString binString = new WfBinaryString().setHexValue("0x3f");
        field.setValue(field.decode(binString));

        /* Verify */
        assertEquals("Hexadecimal field should be correctly decoded", "3f", field.decode(binString));
        assertEquals("Hexadecimal decoded field value should be correctly set", "3f", field.getValue());
    }
    /**
     * Tests compressed binary encoding of DateTime datum field
     */
    @Test
    public void testDateTimeEncoding() throws WfCoreException {
        /* Setup */
        WfMessageField field = new WfMessageField(FIELDNAME, BEGIN+WfMessageField.Encoding.DATETIME.charset()+END, WfMessageField.Encoding.DATETIME, 0, -1);
        field.setValue("2020-07-01T21:42:23Z");

        /* Verify */
        assertEquals("DateTime field should be correctly binary encoded", "00100000001000000000011100000001001000010100001000100011", field.encode().toBinString());
        assertEquals("Unencoded DateTime field should be 20 bytes", 20, field.byteLength());
        assertEquals("Encoded DateTime field should be 56 bits", 56, field.bitLength());
    }
        /**
     * Tests compressed binary encoding of DateTime datum field
     */
    @Test
    public void testDateTimeDecoding() throws WfCoreException {
        /* Setup */
        WfMessageField field = new WfMessageField(FIELDNAME, BEGIN+WfMessageField.Encoding.DATETIME.charset()+END, WfMessageField.Encoding.DATETIME, 0, -1);
        WfBinaryString binString = new WfBinaryString().setBinValue("00100000001000000000011100000001001000010100001000100011");
        field.setValue(field.decode(binString));

        /* Verify */
        assertEquals("DateTime field should be correctly decoded", "2020-07-01T21:42:23Z", field.decode(binString));
        assertEquals("DateTime decoded field value should be correctly set", "2020-07-01T21:42:23Z", field.getValue());
    }
    /**
     * Tests compressed binary encoding of Duration datum field
     */
    @Test
    public void testDurationEncoding() throws WfCoreException {
        /* Setup */
        WfMessageField field = new WfMessageField(FIELDNAME, BEGIN+WfMessageField.Encoding.DURATION.charset()+END, WfMessageField.Encoding.DURATION, 0, 10);
        field.setValue("P24D11H30M");

        /* Verify */
        assertEquals("Duration field should be correctly binary encoded", "001001000001000100110000", field.encode().toBinString());
        assertEquals("Unencoded Duration field should be 10 bytes", 10, field.byteLength());
        assertEquals("Encoded Duration field should be 24 bits", 24, field.bitLength());
    }
    /**
     * Tests compressed binary encoding of Duration field
     */
    @Test
    public void testDurationDecoding() throws WfCoreException {
        /* Test function */
        WfMessageField field = new WfMessageField(FIELDNAME, BEGIN+WfMessageField.Encoding.DURATION.charset()+END, WfMessageField.Encoding.DURATION, 0, 10);
        WfBinaryString binString = new WfBinaryString().setBinValue("001001000001000100110000");
        field.setValue(field.decode(binString));

        /* Verify */
        assertEquals("Duration field should be correctly decoded", "P24D11H30M", field.decode(binString));
        assertEquals("Duration decoded field value should be correctly set", "P24D11H30M", field.getValue());
    }
    /**
     * Tests compressed binary encoding of Latitude datum field
     */
    @Test
    public void testLatitudeEncoding() throws WfCoreException {
        /* Setup */
        WfMessageField field = new WfMessageField(FIELDNAME, BEGIN+WfMessageField.Encoding.LAT.charset()+END, WfMessageField.Encoding.LAT, 0, 9);
        field.setValue("+23.34244");

        /* Verify */
        assertEquals("Latitude field should be correctly binary encoded", "10010001100110100001001000100", field.encode().toBinString());
        assertEquals("Unencoded Latitude field should be 9 bytes", 9, field.byteLength());
        assertEquals("Encoded Latitude field should be 29 bits", 29, field.bitLength());
    }
    /**
     * Tests compressed binary encoding of Latitude datum field
     */
    @Test
    public void testLatitudeDecoding() throws WfCoreException {
        /* Setup */
        WfMessageField field = new WfMessageField(FIELDNAME, BEGIN+WfMessageField.Encoding.LAT.charset()+END, WfMessageField.Encoding.LAT, 0, 9);
        WfBinaryString binString = new WfBinaryString().setBinValue("10010001100110100001001000100");
        field.setValue(field.decode(binString));

        /* Verify */
        assertEquals("Latitude field should be correctly decoded", "+23.34244", field.decode(binString));
        assertEquals("Latitude decoded field value should be correctly set", "+23.34244", field.getValue());
    }
        /**
     * Tests compressed binary encoding of Longitude datum field
     */
    @Test
    public void testLongitudeEncoding() throws WfCoreException {
        /* Setup */
        WfMessageField field = new WfMessageField(FIELDNAME, BEGIN+WfMessageField.Encoding.LONG.charset()+END, WfMessageField.Encoding.LONG, 0, 10);
        field.setValue("-163.34245");

        /* Verify */
        assertEquals("Longitude field should be correctly binary encoded", "000010110001100110100001001000101", field.encode().toBinString());
        assertEquals("Unencoded Longitude field should be 9 bytes", 10, field.byteLength());
        assertEquals("Encoded Longitude field should be 29 bits", 33, field.bitLength());
    }
    /**
     * Tests compressed binary encoding of longitude datum field
     */
    @Test
    public void testLongitudeDecoding() throws WfCoreException {
        /* Setup */
        WfMessageField field = new WfMessageField(FIELDNAME, BEGIN+WfMessageField.Encoding.LONG.charset()+END, WfMessageField.Encoding.LONG, 0, 10);
        WfBinaryString binString = new WfBinaryString().setBinValue("000010110001100110100001001000101");
        field.setValue(field.decode(binString));

        /* Verify */
        assertEquals("Longitude field should be correctly decoded", "-163.34245", field.decode(binString));
        assertEquals("Longitude decoded field value should be correctly set", "-163.34245", field.getValue());
    }
}
