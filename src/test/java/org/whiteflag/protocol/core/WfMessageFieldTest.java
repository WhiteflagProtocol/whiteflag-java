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
    /* Test data */
    String utf = "WF";
    String bin = "10111011";
    String dec = "123";
    String hex = "3f";
    String duration = "P24D11H30M";
    String datetime = "2020-07-01T21:42:23Z";
    String lat = "+23.34244";

    /**
     * Tests compressed binary encoding of utf-8 field
     */
    @Test
    public void testUtfEncoding() throws WfCoreException {
        /* Test function */
        WfMessageField field = new WfMessageField("utf", "^"+WfMessageField.Encoding.UTF8.charset()+"*$", WfMessageField.Encoding.UTF8, 0, 16);
        field.setValue(utf);
        assertEquals("UTF field should be correctly binary encoded", "0101011101000110", field.encode().toBinString());
        assertEquals("UTF field should be correctly hexadecimal encoded", "5746", field.encode().toHexString());
    }
    /**
     * Tests compressed binary encoding of binary field
     */
    @Test
    public void testBinEncoding() throws WfCoreException {
        /* Test function */
        WfMessageField field = new WfMessageField("bin", "^"+WfMessageField.Encoding.BIN.charset()+"*$", WfMessageField.Encoding.BIN, 0, 8);
        field.setValue(bin);
        assertEquals("Binary field should be correctly binary encoded", "0b10111011", field.encode().toBinString(true));
    }
    /**
     * Tests compressed binary encoding of decimal field
     */
    @Test
    public void testDecEncoding() throws WfCoreException {
        /* Test function */
        WfMessageField field = new WfMessageField("dec", "^"+WfMessageField.Encoding.DEC.charset()+"*$", WfMessageField.Encoding.DEC, 0, 12);
        field.setValue(dec);
        assertEquals("Decimal field should be correctly binary encoded", "000100100011", field.encode().toBinString());
    }
    /**
     * Tests compressed binary encoding of hexadecimal field
     */
    @Test
    public void testHexEncoding() throws WfCoreException {
        /* Test function */
        WfMessageField field = new WfMessageField("hex", "^"+WfMessageField.Encoding.HEX.charset()+"*$", WfMessageField.Encoding.HEX, 0, 8);
        field.setValue(hex);
        assertEquals("Hexadecimal field should be correctly binary encoded", "00111111", field.encode().toBinString());
        assertEquals("Hexadecimal field should be correctly binary encoded", "0x3f", field.encode().toHexString(true));
    }
    /**
     * Tests compressed binary encoding of time datum field
     */
    @Test
    public void testTimeDatumEncoding() throws WfCoreException {
        /* Test function */
        WfMessageField field = new WfMessageField("datetime", "^"+WfMessageField.Encoding.DATETIME.charset()+"$", WfMessageField.Encoding.DATETIME, 0, -1);
        field.setValue(datetime);
        assertEquals("Datum field should be correctly binary encoded", "00100000001000000000011100000001001000010100001000100011", field.encode().toBinString());
    }
    /**
     * Tests compressed binary encoding of duration datum field
     */
    @Test
    public void testDurationDatumEncoding() throws WfCoreException {
        /* Test function */
        WfMessageField field = new WfMessageField("duration", "^"+WfMessageField.Encoding.DURATION.charset()+"$", WfMessageField.Encoding.DURATION, 0, 24);
        field.setValue(duration);
        assertEquals("Datum field should be correctly binary encoded", "001001000001000100110000", field.encode().toBinString());
    }
    /**
     * Tests compressed binary encoding of latitude datum field
     */
    @Test
    public void testLatitudeDatumEncoding() throws WfCoreException {
        /* Test function */
        WfMessageField field = new WfMessageField("latitude", "^"+WfMessageField.Encoding.LAT.charset()+"$", WfMessageField.Encoding.LAT, 0, 29);
        field.setValue(lat);
        assertEquals("Datum field should be correctly binary encoded", "10010001100110100001001000100", field.encode().toBinString());
    }
}
