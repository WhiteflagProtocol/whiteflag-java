/*
 * Whiteflag Java Library tests
 */
package org.whiteflagprotocol.java.core;

import org.junit.Test;
import static org.junit.Assert.*;

/* Field encodings required for field definitions */
import static org.whiteflagprotocol.java.core.WfMessageCodec.Encoding.*;

/**
 * Whiteflag field test class
 */
public class WfMessageFieldTest {
    /* Fieldname */
    private final String FIELDNAME = "TESTFIELD";

    /**
     * Tests setting field value
     */
    @Test
    public void testSetFieldValue() {
        /* Setup */
        WfMessageField field = WfMessageField.define(FIELDNAME, null, UTF8, 0, -1);

        /* Verify */
        assertTrue("Should be able to set field value", field.set("some text"));
        assertTrue("Field should be set", field.isSet());
        assertFalse("Should not be able to set field value twice", field.set("another text"));
    }
    /**
     * Tests copying of a field
     */
    @Test
    public void testCopyField() {
        /* Setup */
        WfMessageField field1 = WfMessageField.define(FIELDNAME, null, UTF8, 0, -1);
        field1.set("first value");
        WfMessageField field2 = WfMessageField.from(field1, 7);

        /* Verify */
        assertFalse("The copy of the field should not have a set value", field2.isSet());
        assertTrue("Should be able to set value of the copy of the field", field2.set("second value"));
        assertTrue("The copy of the field should have a set value", field2.isSet());
        assertEquals("Field start byte should be 7", 7, field2.startByte);
        assertEquals("Field end byte should be -1", -1, field2.endByte);
    }
    /**
     * Tests compressed binary encoding of UTF-8 field
     */
    @Test
    public void testUtfEncoding() throws WfCoreException {
        /* Setup */
        WfMessageField field = WfMessageField.define(FIELDNAME, null, UTF8, 0, -1);
        field.set("WF");

        /* Verify */
        assertEquals("UTF-8 field should be correctly hexadecimal encoded", "5746", WfBinaryBuffer.convertToHexString(field.encode()));
        assertEquals("Unencoded UTF-8 field should be 2 bytes", 2, field.byteLength());
        assertEquals("Encoded UTF-8 field should be 16 bits bytes", 16, field.bitLength());
    }
    /**
     * Tests compressed binary decoding of UTF-8 field
     */
    @Test
    public void testUtfDecoding() throws WfCoreException {
        /* Setup */
        WfMessageField field = WfMessageField.define(FIELDNAME, null, UTF8, 0, -1);
        byte[] byteArray = WfBinaryBuffer.convertToByteArray("5746");
        final String result = "WF";

        /* Verify */
        assertEquals("UTF-8 field should be correctly decoded", result, field.decode(byteArray));
        assertEquals("UTF-8 decoded field value should be correctly set", result, field.get());
    }
    /**
     * Tests compressed binary encoding of Binary field
     */
    @Test
    public void testBinEncoding1() throws WfCoreException {
        /* Setup */
        WfMessageField field = WfMessageField.define(FIELDNAME, null, BIN, 0, 8);
        field.set("10111011");
        
        /* Verify */
        assertEquals("Binary field should be correctly binary encoded", "bb", WfBinaryBuffer.convertToHexString(field.encode()));
        assertEquals("Unencoded Binary field should be 8 bytes", 8, field.byteLength());
        assertEquals("Encoded Binary field should be 8 bits", 8, field.bitLength());
    }
    /**
     * Tests compressed binary decoding of Binary field
     */
    @Test
    public void testBinDecoding1() throws WfCoreException {
        /* Setup */
        WfMessageField field = WfMessageField.define(FIELDNAME, null, BIN, 1, 7);
        byte[] byteArray = WfBinaryBuffer.convertToByteArray("aa");
        final String result = "101010";
        
        /* Verify */
        assertEquals("Binary field should be correctly decoded", result, field.decode(byteArray));
        assertEquals("Binary decoded field value should be correctly set", result, field.get());
    }
    /**
     * Tests compressed binary encoding of Binary field
     */
    @Test
    public void testBinEncoding2() throws WfCoreException {
        /* Setup */
        WfMessageField field = WfMessageField.define(FIELDNAME, null, BIN, 4, 5);
        field.set("1");
        
        /* Verify */
        assertEquals("Binary field should be correctly binary encoded", "80", WfBinaryBuffer.convertToHexString(field.encode()));
        assertEquals("Unencoded Binary field should be 1 bytes", 1, field.byteLength());
        assertEquals("Encoded Binary field should be 1 bits", 1, field.bitLength());
    }
    /**
     * Tests compressed binary decoding of Binary field
     */
    @Test
    public void testBinDecoding2A() throws WfCoreException {
        /* Setup */
        WfMessageField field = WfMessageField.define(FIELDNAME, null, BIN, 4, 5);
        byte[] byteArray = WfBinaryBuffer.convertToByteArray("80");
        final String result = "1";
        
        /* Verify */
        assertEquals("Binary field should be correctly decoded", result, field.decode(byteArray));
        assertEquals("Binary decoded field value should be correctly set", result, field.get());
    }
    /**
     * Tests compressed binary decoding of Binary field
     */
    @Test
    public void testBinDecoding2B() throws WfCoreException {
        /* Setup */
        WfMessageField field = WfMessageField.define(FIELDNAME, null, BIN, 2, 3);
        byte[] byteArray = WfBinaryBuffer.convertToByteArray("7f");
        final String result = "0";
        
        /* Verify */
        assertEquals("Binary field should be correctly decoded", result, field.decode(byteArray));
        assertEquals("Binary decoded field value should be correctly set", result, field.get());
    }
    /**
     * Tests compressed binary encoding of Decimal field
     */
    @Test
    public void testDecEncoding() throws WfCoreException {
        /* Setup */
        WfMessageField field = WfMessageField.define(FIELDNAME, null, DEC, 0, 3);
        field.set("1230");
        
        /* Verify */
        assertEquals("Decimal field should be correctly binary encoded", "1230", WfBinaryBuffer.convertToHexString(field.encode()));
        assertEquals("Unencoded Decimal field should be 3 bytes", 3, field.byteLength());
        assertEquals("Encoded Decimal field should be 12 bits", 12, field.bitLength());
    }
    /**
     * Tests decoding of Decimal field
     */
    @Test
    public void testDecDecoding() throws WfCoreException {
        /* Setup */
        WfMessageField field = WfMessageField.define(FIELDNAME, null, DEC, 0, 3);
        byte[] byteArray = WfBinaryBuffer.convertToByteArray("1234");
        final String result = "123";

        /* Verify */
        assertEquals("Decimal field should be correctly decoded", result, field.decode(byteArray));
        assertEquals("Decimal decoded field value should be correctly set", result, field.get());
    }
    /**
     * Tests compressed binary encoding of Hexadecimal field
     */
    @Test
    public void testHexEncoding() throws WfCoreException {
        /* Setup */
        WfMessageField field = WfMessageField.define(FIELDNAME, null, HEX, 0, 2);
        field.set("3f");
        
        /* Verify */
        assertEquals("Hexadecimal field should be correctly binary encoded", "3f", WfBinaryBuffer.convertToHexString(field.encode()));
        assertEquals("Unencoded Hexadecimal field should be 2 bytes", 2, field.byteLength());
        assertEquals("Encoded Hexadecimal field should be 8 bits", 8, field.bitLength());
    }
    /**
     * Tests decoding of Hexadecimal field
     */
    @Test
    public void testHexDecoding() throws WfCoreException {
        /* Setup */
        WfMessageField field = WfMessageField.define(FIELDNAME, null, HEX, 0, 2);
        byte[] byteArray = WfBinaryBuffer.convertToByteArray("0x3f");
        final String result = "3f";

        /* Verify */
        assertEquals("Hexadecimal field should be correctly decoded", result, field.decode(byteArray));
        assertEquals("Hexadecimal decoded field value should be correctly set", result, field.get());
    }
    /**
     * Tests compressed binary encoding of DateTime datum field
     */
    @Test
    public void testDateTimeEncoding() throws WfCoreException {
        /* Setup */
        WfMessageField field = WfMessageField.define(FIELDNAME, null, DATETIME, 0, -1);
        field.set("2020-07-01T21:42:23Z");

        /* Verify */
        assertEquals("DateTime field should be correctly binary encoded", "20200701214223", WfBinaryBuffer.convertToHexString(field.encode()));
        assertEquals("Unencoded DateTime field should be 20 bytes", 20, field.byteLength());
        assertEquals("Encoded DateTime field should be 56 bits", 56, field.bitLength());
    }
    /**
     * Tests compressed binary encoding of DateTime datum field
     */
    @Test
    public void testDateTimeDecoding() throws WfCoreException {
        /* Setup */
        WfMessageField field = WfMessageField.define(FIELDNAME, null, DATETIME, 0, -1);
        byte[] byteArray = WfBinaryBuffer.convertToByteArray("20200701214223");
        final String result = "2020-07-01T21:42:23Z";

        /* Verify */
        assertEquals("DateTime field should be correctly decoded", result, field.decode(byteArray));
        assertEquals("DateTime decoded field value should be correctly set", result, field.get());
    }
    /**
     * Tests compressed binary encoding of Duration datum field
     */
    @Test
    public void testDurationEncoding() throws WfCoreException {
        /* Setup */
        WfMessageField field = WfMessageField.define(FIELDNAME, null, DURATION, 0, 10);
        field.set("P24D11H30M");

        /* Verify */
        assertEquals("Duration field should be correctly binary encoded", "241130", WfBinaryBuffer.convertToHexString(field.encode()));
        assertEquals("Unencoded Duration field should be 10 bytes", 10, field.byteLength());
        assertEquals("Encoded Duration field should be 24 bits", 24, field.bitLength());
    }
    /**
     * Tests compressed binary encoding of Duration field
     */
    @Test
    public void testDurationDecoding() throws WfCoreException {
        /* Test function */
        WfMessageField field = WfMessageField.define(FIELDNAME, null, DURATION, 0, 10);
        byte[] byteArray = WfBinaryBuffer.convertToByteArray("241130");
        final String result = "P24D11H30M";

        /* Verify */
        assertEquals("Duration field should be correctly decoded", result, field.decode(byteArray));
        assertEquals("Duration decoded field value should be correctly set", result, field.get());
    }
    /**
     * Tests compressed binary encoding of Latitude datum field
     */
    @Test
    public void testLatitudeEncoding() throws WfCoreException {
        /* Setup */
        WfMessageField field = WfMessageField.define(FIELDNAME, null, LAT, 0, 9);
        field.set("+23.34244"); // 1001 0001 1001 1010 0001 0010 0010 0000

        /* Verify */
        assertEquals("Latitude field should be correctly binary encoded", "919a1220", WfBinaryBuffer.convertToHexString(field.encode()));
        assertEquals("Unencoded Latitude field should be 9 bytes", 9, field.byteLength());
        assertEquals("Encoded Latitude field should be 29 bits", 29, field.bitLength());
    }
    /**
     * Tests compressed binary encoding of Latitude datum field
     */
    @Test
    public void testLatitudeDecoding() throws WfCoreException {
        /* Setup */
        WfMessageField field = WfMessageField.define(FIELDNAME, null, LAT, 0, 9);
        byte[] byteArray = WfBinaryBuffer.convertToByteArray("919a1220");
        final String result = "+23.34244";

        /* Verify */
        assertEquals("Latitude field should be correctly decoded", result, field.decode(byteArray));
        assertEquals("Latitude decoded field value should be correctly set", result, field.get());
    }
    /**
     * Tests compressed binary encoding of Longitude datum field
     */
    @Test
    public void testLongitudeEncoding() throws WfCoreException {
        /* Setup */
        WfMessageField field = WfMessageField.define(FIELDNAME, null, LONG, 0, 10);
        field.set("-163.34245");   // 0000 1011 0001 1001 1010 0001 0010 0010 1000

        /* Verify */
        assertEquals("Longitude field should be correctly binary encoded", "0b19a12280", WfBinaryBuffer.convertToHexString(field.encode()));
        assertEquals("Unencoded Longitude field should be 9 bytes", 10, field.byteLength());
        assertEquals("Encoded Longitude field should be 29 bits", 33, field.bitLength());
    }
    /**
     * Tests compressed binary encoding of longitude datum field
     */
    @Test
    public void testLongitudeDecoding1() throws WfCoreException {
        /* Setup */
        WfMessageField field = WfMessageField.define(FIELDNAME, null, LONG, 0, 10);
        byte[] byteArray = WfBinaryBuffer.convertToByteArray("8b19a12380");
        final String result = "+163.34247";

        /* Verify */
        assertEquals("Longitude field should be correctly decoded", result, field.decode(byteArray));
        assertEquals("Longitude decoded field value should be correctly set", result, field.get());
    }
    /**
     * Tests compressed binary encoding of longitude datum field
     */
    @Test
    public void testLongitudeDecoding2() throws WfCoreException {
        /* Setup */
        WfMessageField field = WfMessageField.define(FIELDNAME, null, LONG, 0, 10);
        byte[] byteArray = WfBinaryBuffer.convertToByteArray("0319a12380");
        final String result = "-063.34247";

        /* Verify */
        assertEquals("Longitude field should be correctly decoded", result, field.decode(byteArray));
        assertEquals("Longitude decoded field value should be correctly set", result, field.get());
    }
}
