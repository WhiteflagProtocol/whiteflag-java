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
    public void testCryptoMessageCompilation() throws WfCoreException {
        /* Test data */
        WfMessage wfMessage;
        String[] fieldValues = { "WF", "1", "0", "0", "K", "0", "0000000000000000000000000000000000000000000000000000000000000000", "11", "d426bbe111221675e333f30ef608b1aa6e60a47080dd33cb49e96395894ef42f" };
        try {
            wfMessage = WfMessage.Creator.compile(fieldValues);
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
    public void testAuthMessageCompilation() throws WfCoreException {
        /* Test data */
        WfMessage wfMessage;
        String[] fieldValues = { "WF", "1", "0", "0", "A", "0", "0000000000000000000000000000000000000000000000000000000000000000", "1", "b01218a30dd3c23d050af254bfcce31a715fecdff6a23fd59609612e6e0ef263"};
        try {
            wfMessage = WfMessage.Creator.compile(fieldValues);
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
    public void testAuthMessageSerialization() throws WfCoreException {
        /* Test data */
        WfMessage wfMessage;
        String[] fieldValues = { "WF", "1", "0", "0", "A", "0", "0000000000000000000000000000000000000000000000000000000000000000",
                                 "2", "b01218a30dd3c23d050af254bfcce31a715fecdff6a23fd59609612e6e0ef263"};
        try {
            wfMessage = WfMessage.Creator.compile(fieldValues);
        } catch (WfCoreException e) {
            throw e;
        }
        /* Test function */
        String wfMessageSerialized = wfMessage.serialize();
        assertEquals("Serialization should be correct",
                     "WF100A000000000000000000000000000000000000000000000000000000000000000002b01218a30dd3c23d050af254bfcce31a715fecdff6a23fd59609612e6e0ef263",
                     wfMessageSerialized);
    }
    /**
     * Tests deserialization of authentication message
     */
    @Test
    public void testAuthMessageDeserialization() throws WfCoreException {
        /* Test data */
        String serializedMessage = "WF100A000000000000000000000000000000000000000000000000000000000000000001https://organisation.int/whiteflag";
        WfMessage wfMessage = WfMessage.Creator.deserialize(serializedMessage);

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
     * Tests decoding of authentication message
     */
    @Test
    public void testAuthMessageDecoding() throws WfCoreException {
        /* Test data */
        WfMessage wfMessage;
        String wfMessageEncoded = "5746313020800000000000000000000000000000000000000000000000000000000000000000b43a3a38399d1797b7b933b0b734b9b0ba34b7b71734b73a17bbb434ba32b33630b380";
        String[] fieldValues = { "WF", "1", "0", "0", "A", "0", "0000000000000000000000000000000000000000000000000000000000000000",
                                 "1", "https://organisation.int/whiteflag" };
        try {
            wfMessage = WfMessage.Creator.decode(wfMessageEncoded);
        } catch (WfCoreException e) {
            throw e;
        }
        /* Test function */
        assertTrue("Message should be valid", wfMessage.isValid());
        assertEquals("Prefix should be correctly set", fieldValues[0], wfMessage.getFieldValue("Prefix"));
        assertEquals("Version number should be correctly set", fieldValues[1], wfMessage.getFieldValue("Version"));
        assertEquals("Encryption indicator should be correctly set", fieldValues[2], wfMessage.getFieldValue("EncryptionIndicator"));
        assertEquals("Duress indicator should be correctly set", fieldValues[3], wfMessage.getFieldValue("DuressIndicator"));
        assertEquals("Message code should be correctly set", fieldValues[4], wfMessage.getFieldValue("MessageCode"));
        assertEquals("Reference indicator should be correctly set", fieldValues[5], wfMessage.getFieldValue("ReferenceIndicator"));
        assertEquals("Referenced message should be correctly set", fieldValues[6], wfMessage.getFieldValue("ReferencedMessage"));
        assertEquals("Subject code should be correctly set", fieldValues[7], wfMessage.getFieldValue("VerificationMethod"));
        assertEquals("DateTime should be correctly set", fieldValues[8], wfMessage.getFieldValue("VerificationData"));
    }
    /**
     * Tests invalid message data
     */
    @Test
    public void testInvalidMessage() throws WfCoreException {
        /* Test data */
        String[] fieldValues = { "WF", "1", "0", "0", "X", "0", "0000000000000000000000000000000000000000000000000000000000000000", "1", "b01218a30dd3c23d050af254bfcce31a715fecdff6a23fd59609612e6e0ef263"};
        try {
            WfMessage wfMessage = WfMessage.Creator.compile(fieldValues);
            fail("Expected a WfCoreException to be thrown");
            assertFalse("Message should not be valid", wfMessage.isValid());
        } catch (WfCoreException e) {
            assertTrue(e instanceof WfCoreException);
        }
    }
    /**
     * Tests serialization of sign/signal message
     */
    @Test
    public void testSignSignalMessageEncoding() throws WfCoreException {
        /* Test data */
        WfMessage wfMessage;
        String[] fieldValues = { "WF", "1", "0", "1", "M", "4", "3efb4e0cfa83122b242634254c1920a769d615dfcc4c670bb53eb6f12843c3ae",
                                 "80", "2013-08-31T04:29:15Z", "P00D00H00M", "22", "+30.79658", "-037.82602", "8765", "3210", "042"
                                };
        try {
            wfMessage = WfMessage.Creator.compile(fieldValues);
        } catch (WfCoreException e) {
            throw e;
        }
        /* Test function */
        String wfMessageEncoded = wfMessage.encode();
        assertEquals("Encoding should be correct",
                     "57463130a6a1f7da7067d41891592131a12a60c9053b4eb0aefe6263385da9f5b789421e1d7401009841882148a800000114c1e596006f04c050eca6420084",
                     wfMessageEncoded);
    }
    /**
     * Tests decoding of sign/signal message
     */
    @Test
    public void testSignSignalMessageDecoding() throws WfCoreException {
        /* Test data */
        WfMessage wfMessage;
        String wfMessageEncoded = "57463130a6a1f7da7067d41891592131a12a60c9053b4eb0aefe6263385da9f5b789421e1d7401009841882148a800000114c1e596006f04c050eca6420084";
        String[] fieldValues = { "WF", "1", "0", "1", "M", "4", "3efb4e0cfa83122b242634254c1920a769d615dfcc4c670bb53eb6f12843c3ae",
                                 "80", "2013-08-31T04:29:15Z", "P00D00H00M", "22", "+30.79658", "-037.82602", "8765", "3210", "042"
                                };
        try {
            wfMessage = WfMessage.Creator.decode(wfMessageEncoded);
        } catch (WfCoreException e) {
            throw e;
        }
        /* Test function */
        assertTrue("Message should be valid", wfMessage.isValid());
        assertEquals("Prefix should be correctly set", fieldValues[0], wfMessage.getFieldValue("Prefix"));
        assertEquals("Version number should be correctly set", fieldValues[1], wfMessage.getFieldValue("Version"));
        assertEquals("Encryption indicator should be correctly set", fieldValues[2], wfMessage.getFieldValue("EncryptionIndicator"));
        assertEquals("Duress indicator should be correctly set", fieldValues[3], wfMessage.getFieldValue("DuressIndicator"));
        assertEquals("Message code should be correctly set", fieldValues[4], wfMessage.getFieldValue("MessageCode"));
        assertEquals("Reference indicator should be correctly set", fieldValues[5], wfMessage.getFieldValue("ReferenceIndicator"));
        assertEquals("Referenced message should be correctly set", fieldValues[6], wfMessage.getFieldValue("ReferencedMessage"));
        assertEquals("Subject code should be correctly set", fieldValues[7], wfMessage.getFieldValue("SubjectCode"));
        assertEquals("DateTime should be correctly set", fieldValues[8], wfMessage.getFieldValue("DateTime"));
        assertEquals("Duration should be correctly set", fieldValues[9], wfMessage.getFieldValue("Duration"));
        assertEquals("Object code  should be correctly set", fieldValues[10], wfMessage.getFieldValue("ObjectType"));
        assertEquals("Latitude should be correctly set", fieldValues[11], wfMessage.getFieldValue("ObjectLatitude"));
        assertEquals("Longitude should be correctly set", fieldValues[12], wfMessage.getFieldValue("ObjectLongitude"));
        assertEquals("Size dimention 1 should be correctly set", fieldValues[13], wfMessage.getFieldValue("ObjectSizeDim1"));
        assertEquals("Size dimention 2 should be correctly set", fieldValues[14], wfMessage.getFieldValue("ObjectSizeDim2"));
        assertEquals("Orientation should be correctly set", fieldValues[15], wfMessage.getFieldValue("ObjectOrientation"));
    }
    /**
     * Tests serialization of free text message
     */
    @Test
    public void testFreeTextMessage() throws WfCoreException {
        /* Test data */
        WfMessage wfMessage1 = WfMessage.Creator.deserialize("WF100F5f6c1e1ed8950b137bb9e0edcf21593d62c03a7fb39dacfd554c593f72c8942dfWhiteflag test message!");
        assertTrue("Message should be valid", wfMessage1.isValid());
        WfMessage wfMessage2 = WfMessage.Creator.decode("57463130232fb60f0f6c4a8589bddcf076e790ac9eb1601d3fd9ced67eaaa62c9fb9644a16fabb434ba32b33630b3903a32b9ba1036b2b9b9b0b3b2908");
        assertTrue("Message should be valid", wfMessage2.isValid());
        /* Test function */
        assertEquals("Prefix should be identical", wfMessage1.getFieldValue("Prefix"), wfMessage2.getFieldValue("Prefix"));
        assertEquals("Version number should be identical", wfMessage1.getFieldValue("Version"), wfMessage2.getFieldValue("Version"));
        assertEquals("Encryption indicator should be identical", wfMessage1.getFieldValue("EncryptionIndicator"), wfMessage2.getFieldValue("EncryptionIndicator"));
        assertEquals("Duress indicator should be identical", wfMessage1.getFieldValue("DuressIndicator"), wfMessage2.getFieldValue("DuressIndicator"));
        assertEquals("Message code should be identical", wfMessage1.getFieldValue("MessageCode"), wfMessage2.getFieldValue("MessageCode"));
        assertEquals("Reference indicator should be identical", wfMessage1.getFieldValue("ReferenceIndicator"), wfMessage2.getFieldValue("ReferenceIndicator"));
        assertEquals("Referenced message should be identical", wfMessage1.getFieldValue("ReferencedMessage"), wfMessage2.getFieldValue("ReferencedMessage"));
        assertEquals("Subject code should be identical", wfMessage1.getFieldValue("Text"), wfMessage2.getFieldValue("Text"));
    }
}
