/*
 * Whiteflag Java Library tests
 */
package org.whiteflag.protocol;

import org.junit.Test;
import static org.junit.Assert.*;

import org.whiteflag.protocol.core.*;

/**
 * Whiteflag authentication message test class
 */
public class WfMessageTest {
    /**
     * Tests for correctly constructed cryptographic message with header and body object
     */
    @Test
    public void testCryptoMessage1() throws WfCoreException {
        /* Test data */
        WfMessage wfMessage;
        String[] fieldValues = { "WF", "1", "0", "0", "K", "0", "0000000000000000000000000000000000000000000000000000000000000000", "11", "d426bbe111221675e333f30ef608b1aa6e60a47080dd33cb49e96395894ef42f" };
        try {
            wfMessage = new WfMessage.Creator().compile(fieldValues);
        } catch (WfCoreException e) {
            throw e;
        }

        /* Test function */
        assertEquals("Prefix should be correctly set", fieldValues[0], wfMessage.getFieldValue("Prefix"));
        assertEquals("Version number should be correctly set", fieldValues[1], wfMessage.getFieldValue("Version"));
        assertEquals("Encryption indicator should be correctly set", fieldValues[2], wfMessage.getFieldValue("EncryptionIndicator"));
        assertEquals("Duress indicator should be correctly set", fieldValues[3], wfMessage.getFieldValue("DuressIndicator"));
        assertEquals("Message code should be correctly set", fieldValues[4], wfMessage.getFieldValue("MessageCode"));
        assertEquals("Reference indicator should be correctly set", fieldValues[5], wfMessage.getFieldValue("ReferenceIndicator"));
        assertEquals("Referenced message should be correctly set", fieldValues[6], wfMessage.getFieldValue("ReferencedMessage"));
        assertEquals("Cryptographic data type should be correctly set", fieldValues[7], wfMessage.getFieldValue("CryptoDataType"));
        assertEquals("Cryptographic data should be correctly set", fieldValues[8], wfMessage.getFieldValue("CryptoData"));
        assertTrue("Message should be valid", wfMessage.isValid());
    }
    /**
     * Tests for correctly constructed authentication message with header and body object
     */
    @Test
    public void testAuthMessage1() throws WfCoreException {
        /* Test data */
        WfMessage wfMessage;
        String[] fieldValues = { "WF", "1", "0", "0", "A", "0", "0000000000000000000000000000000000000000000000000000000000000000", "1", "b01218a30dd3c23d050af254bfcce31a715fecdff6a23fd59609612e6e0ef263"};
        try {
            wfMessage = new WfMessage.Creator().compile(fieldValues);
        } catch (WfCoreException e) {
            throw e;
        }

        /* Test function */
        assertEquals("Prefix should be correctly set", fieldValues[0], wfMessage.getFieldValue("Prefix"));
        assertEquals("Version number should be correctly set", fieldValues[1], wfMessage.getFieldValue("Version"));
        assertEquals("Encryption indicator should be correctly set", fieldValues[2], wfMessage.getFieldValue("EncryptionIndicator"));
        assertEquals("Duress indicator should be correctly set", fieldValues[3], wfMessage.getFieldValue("DuressIndicator"));
        assertEquals("Message code should be correctly set", fieldValues[4], wfMessage.getFieldValue("MessageCode"));
        assertEquals("Reference indicator should be correctly set", fieldValues[5], wfMessage.getFieldValue("ReferenceIndicator"));
        assertEquals("Referenced message should be correctly set", fieldValues[6], wfMessage.getFieldValue("ReferencedMessage"));
        assertEquals("Verification method type should be correctly set", fieldValues[7], wfMessage.getFieldValue("VerificationMethod"));
        assertEquals("Verification data should be correctly set", fieldValues[8], wfMessage.getFieldValue("VerificationData"));
        assertTrue("Message should be valid", wfMessage.isValid());
    }
    /**
     * Tests serialization of authentication message
     */
    @Test
    public void testAuthMessageSerialization1() throws WfCoreException {
        /* Test data */
        WfMessage wfMessage;
        String[] fieldValues = { "WF", "1", "0", "0", "A", "0", "0000000000000000000000000000000000000000000000000000000000000000", "1", "b01218a30dd3c23d050af254bfcce31a715fecdff6a23fd59609612e6e0ef263"};
        try {
            wfMessage = new WfMessage.Creator().compile(fieldValues);
        } catch (WfCoreException e) {
            throw e;
        }
        /* Test function */
        String wfMessageSerialized = wfMessage.serialize();
        assertEquals("Serialization should be correct",
                     "WF100A000000000000000000000000000000000000000000000000000000000000000001b01218a30dd3c23d050af254bfcce31a715fecdff6a23fd59609612e6e0ef263",
                     wfMessageSerialized);
    }
    /**
     * Tests serialization of authentication message
     */
    @Test
    public void testAuthMessageDeserialization1() throws WfCoreException {
        /* Test data */
        String serializedMessage = "WF100A000000000000000000000000000000000000000000000000000000000000000001b01218a30dd3c23d050af254bfcce31a715fecdff6a23fd59609612e6e0ef263";
        WfMessage wfMessage = new WfMessage.Creator().deserialize(serializedMessage);

        /* Test function */
        assertEquals("Prefix should be correctly set", serializedMessage.substring(0, 2), wfMessage.getFieldValue("Prefix"));
        assertEquals("Version number should be correctly set", serializedMessage.substring(2, 3), wfMessage.getFieldValue("Version"));
        assertEquals("Encryption indicator should be correctly set", serializedMessage.substring(3, 4), wfMessage.getFieldValue("EncryptionIndicator"));
        assertEquals("Duress indicator should be correctly set", serializedMessage.substring(4, 5), wfMessage.getFieldValue("DuressIndicator"));
        assertEquals("Message code should be correctly set", serializedMessage.substring(5, 6), wfMessage.getFieldValue("MessageCode"));
        assertEquals("Reference indicator should be correctly set", serializedMessage.substring(6, 7), wfMessage.getFieldValue("ReferenceIndicator"));
        assertEquals("Referenced message should be correctly set", serializedMessage.substring(7, 71), wfMessage.getFieldValue("ReferencedMessage"));
        assertEquals("Verification method type should be correctly set", serializedMessage.substring(71, 72), wfMessage.getFieldValue("VerificationMethod"));
        assertEquals("Verification data should be correctly set", serializedMessage.substring(72), wfMessage.getFieldValue("VerificationData"));
        assertTrue("Message should be valid", wfMessage.isValid());
    }
    /**
     * Tests invalid message data
     */
    @Test
    public void testInvalidMessage1() throws WfCoreException {
        /* Test data */
        String[] fieldValues = { "WF", "1", "0", "0", "X", "0", "0000000000000000000000000000000000000000000000000000000000000000", "1", "b01218a30dd3c23d050af254bfcce31a715fecdff6a23fd59609612e6e0ef263"};
        try {
            WfMessage wfMessage = new WfMessage.Creator().compile(fieldValues);
            fail("Expected a WfCoreException to be thrown");
            assertFalse("Message should not be valid", wfMessage.isValid());
        } catch (WfCoreException e) {
            assertTrue(e instanceof WfCoreException);
        }
    }
    /**
     * Tests serialization of authentication message
     */
    @Test
    public void testSignSignalMessageEncoding() throws WfCoreException {
        /* Test data */
        WfMessage wfMessage;
        String[] fieldValues = { "WF", "1", "0", "1", "M", "4", "3efb4e0cfa83122b242634254c1920a769d615dfcc4c670bb53eb6f12843c3ae",
                                 "80", "2013-08-31T04:29:15Z", "P00D00H00M", "22", "+30.79658", "-037.82602", "8765", "3210", "042"
                                };
        try {
            wfMessage = new WfMessage.Creator().compile(fieldValues);
        } catch (WfCoreException e) {
            throw e;
        }
        /* Test function */
        String wfMessageEncoded = wfMessage.encode();
        assertEquals("Encoding should be correct",
                     "57463130a6a1f7da7067d41891592131a12a60c9053b4eb0aefe6263385da9f5b789421e1d7401009841882148a800000114c1e596006f04c050eca6420084",
                     wfMessageEncoded);
    }
}
