/*
 * Whiteflag Java Library tests
 */
package org.whiteflag.protocol;

import org.junit.Test;
import static org.junit.Assert.*;

/**
 * Whiteflag message test class
 */
public class WfMessageTest {
    /**
     * Tests for correctly constructed cryptographic message with header and body object
     */
    @Test
    public void testCryptoMessageCompilation() throws WfException {
        /* Setup */
        final String[] fieldValues = { "WF", "1", "0", "0", "K", "0", "0000000000000000000000000000000000000000000000000000000000000000",
                                       "11", "d426bbe111221675e333f30ef608b1aa6e60a47080dd33cb49e96395894ef42f"
                                    };
        WfMessage message;
        try {
            message = WfMessage.Creator.compile(fieldValues);
        } catch (WfException e) {
            throw e;
        }

        /* Verify */
        assertTrue("Message should be valid", message.isValid());
        assertEquals("Prefix should be correctly set", fieldValues[0], message.getFieldValue("Prefix"));
        assertEquals("Version number should be correctly set", fieldValues[1], message.getFieldValue("Version"));
        assertEquals("Encryption indicator should be correctly set", fieldValues[2], message.getFieldValue("EncryptionIndicator"));
        assertEquals("Duress indicator should be correctly set", fieldValues[3], message.getFieldValue("DuressIndicator"));
        assertEquals("Message code should be correctly set", fieldValues[4], message.getFieldValue("MessageCode"));
        assertEquals("Reference indicator should be correctly set", fieldValues[5], message.getFieldValue("ReferenceIndicator"));
        assertEquals("Referenced message should be correctly set", fieldValues[6], message.getFieldValue("ReferencedMessage"));
        assertEquals("Cryptographic data type should be correctly set", fieldValues[7], message.getFieldValue("CryptoDataType"));
        assertEquals("Cryptographic data should be correctly set", fieldValues[8], message.getFieldValue("CryptoData"));
    }
    /**
     * Tests for correctly constructed authentication message with header and body object
     */
    @Test
    public void testAuthMessageCompilation() throws WfException {
        /* Setup */
        final String[] fieldValues = { "WF", "1", "0", "0", "A", "0", "0000000000000000000000000000000000000000000000000000000000000000",
                                       "1", "b01218a30dd3c23d050af254bfcce31a715fecdff6a23fd59609612e6e0ef263"
                                    };
        WfMessage message;
        try {
            message = WfMessage.Creator.compile(fieldValues);
        } catch (WfException e) {
            throw e;
        }

        /* Verify */
        assertTrue("Message should be valid", message.isValid());
        assertEquals("Prefix should be correctly set", fieldValues[0], message.getFieldValue("Prefix"));
        assertEquals("Version number should be correctly set", fieldValues[1], message.getFieldValue("Version"));
        assertEquals("Encryption indicator should be correctly set", fieldValues[2], message.getFieldValue("EncryptionIndicator"));
        assertEquals("Duress indicator should be correctly set", fieldValues[3], message.getFieldValue("DuressIndicator"));
        assertEquals("Message code should be correctly set", fieldValues[4], message.getFieldValue("MessageCode"));
        assertEquals("Reference indicator should be correctly set", fieldValues[5], message.getFieldValue("ReferenceIndicator"));
        assertEquals("Referenced message should be correctly set", fieldValues[6], message.getFieldValue("ReferencedMessage"));
        assertEquals("Verification method type should be correctly set", fieldValues[7], message.getFieldValue("VerificationMethod"));
        assertEquals("Verification data should be correctly set", fieldValues[8], message.getFieldValue("VerificationData"));
    }
    /**
     * Tests serialization of authentication message
     */
    @Test
    public void testAuthMessageSerialization() throws WfException {
        /* Setup */
        final String messageSerialized = "WF100A000000000000000000000000000000000000000000000000000000000000000002b01218a30dd3c23d050af254bfcce31a715fecdff6a23fd59609612e6e0ef263";
        final String[] fieldValues = { "WF", "1", "0", "0", "A", "0", "0000000000000000000000000000000000000000000000000000000000000000",
                                       "2", "b01218a30dd3c23d050af254bfcce31a715fecdff6a23fd59609612e6e0ef263"
                                    };
        WfMessage message;
        try {
            message = WfMessage.Creator.compile(fieldValues);
        } catch (WfException e) {
            throw e;
        }

        /* Verify */
        assertTrue("Message should be valid", message.isValid());
        assertEquals("Serialization should be correct", messageSerialized, message.serialize());
        assertEquals("Serialization from cache should be correct", messageSerialized, message.serialize());
    }
    /**
     * Tests deserialization of authentication message
     */
    @Test
    public void testAuthMessageDeserialization() throws WfException {
        /* Setup */
        final String messageSerialized = "WF100A000000000000000000000000000000000000000000000000000000000000000001https://organisation.int/whiteflag";
        WfMessage message = WfMessage.Creator.deserialize(messageSerialized);

        /* Verify */
        assertEquals("Prefix should be correctly set", messageSerialized.substring(0, 2), message.getFieldValue("Prefix"));
        assertEquals("Version number should be correctly set", messageSerialized.substring(2, 3), message.getFieldValue("Version"));
        assertEquals("Encryption indicator should be correctly set", messageSerialized.substring(3, 4), message.getFieldValue("EncryptionIndicator"));
        assertEquals("Duress indicator should be correctly set", messageSerialized.substring(4, 5), message.getFieldValue("DuressIndicator"));
        assertEquals("Message code should be correctly set", messageSerialized.substring(5, 6), message.getFieldValue("MessageCode"));
        assertEquals("Reference indicator should be correctly set", messageSerialized.substring(6, 7), message.getFieldValue("ReferenceIndicator"));
        assertEquals("Referenced message should be correctly set", messageSerialized.substring(7, 71), message.getFieldValue("ReferencedMessage"));
        assertEquals("Verification method type should be correctly set", messageSerialized.substring(71, 72), message.getFieldValue("VerificationMethod"));
        assertEquals("Verification data should be correctly set", messageSerialized.substring(72), message.getFieldValue("VerificationData"));
        assertTrue("Message should be valid", message.isValid());
    }
    /**
     * Tests decoding of authentication message
     */
    @Test
    public void testAuthMessageDecoding() throws WfException {
        /* Setup */
        final String[] fieldValues = { "WF", "1", "0", "0", "A", "0", "0000000000000000000000000000000000000000000000000000000000000000",
                                       "1", "https://organisation.int/whiteflag"
                                    };
        WfMessage message;
        try {
            message = WfMessage.Creator.decode("5746313020800000000000000000000000000000000000000000000000000000000000000000b43a3a38399d1797b7b933b0b734b9b0ba34b7b71734b73a17bbb434ba32b33630b380");
        } catch (WfException e) {
            throw e;
        }

        /* Verify */
        assertTrue("Message should be valid", message.isValid());
        assertEquals("Prefix should be correctly set", fieldValues[0], message.getFieldValue("Prefix"));
        assertEquals("Version number should be correctly set", fieldValues[1], message.getFieldValue("Version"));
        assertEquals("Encryption indicator should be correctly set", fieldValues[2], message.getFieldValue("EncryptionIndicator"));
        assertEquals("Duress indicator should be correctly set", fieldValues[3], message.getFieldValue("DuressIndicator"));
        assertEquals("Message code should be correctly set", fieldValues[4], message.getFieldValue("MessageCode"));
        assertEquals("Reference indicator should be correctly set", fieldValues[5], message.getFieldValue("ReferenceIndicator"));
        assertEquals("Referenced message should be correctly set", fieldValues[6], message.getFieldValue("ReferencedMessage"));
        assertEquals("Subject code should be correctly set", fieldValues[7], message.getFieldValue("VerificationMethod"));
        assertEquals("DateTime should be correctly set", fieldValues[8], message.getFieldValue("VerificationData"));
    }
    /**
     * Tests invalid message data
     */
    @Test
    public void testInvalidMessage() throws WfException {
        /* Setup */
        final String[] fieldValues = { "WF", "1", "0", "0", "X", "0", "0000000000000000000000000000000000000000000000000000000000000000",
                                       "1", "b01218a30dd3c23d050af254bfcce31a715fecdff6a23fd59609612e6e0ef263"
                                    };
        /* Verify */
        try {
            WfMessage message = WfMessage.Creator.compile(fieldValues);
            fail("Expected a WfException to be thrown");
            assertFalse("Message should not be valid", message.isValid());
        } catch (WfException e) {
            assertTrue(e instanceof WfException);
        }
    }
    /**
     * Tests serialization of sign/signal message
     */
    @Test
    public void testSignSignalMessageEncoding() throws WfException {
        /* Setup */

        final String messageEncoded = "57463130a6a1f7da7067d41891592131a12a60c9053b4eb0aefe6263385da9f5b789421e1d7401009841882148a800000114c1e596006f04c050eca6420084";
        final String[] fieldValues = { "WF", "1", "0", "1", "M", "4", "3efb4e0cfa83122b242634254c1920a769d615dfcc4c670bb53eb6f12843c3ae",
                                       "80", "2013-08-31T04:29:15Z", "P00D00H00M", "22", "+30.79658", "-037.82602", "8765", "3210", "042"
                                    };
        WfMessage message;
        try {
            message = WfMessage.Creator.compile(fieldValues);
        } catch (WfException e) {
            throw e;
        }

        /* Verify */
        assertTrue("Message should be valid", message.isValid());
        assertEquals("Encoding should be correct", messageEncoded, message.encode());
        assertEquals("Encoding from chache should be correct", messageEncoded, message.encode());
    }
    /**
     * Tests decoding of sign/signal message
     */
    @Test
    public void testSignSignalMessageDecoding() throws WfException {
        /* Setup */
        final String messageEncoded = "57463130a6a1f7da7067d41891592131a12a60c9053b4eb0aefe6263385da9f5b789421e1d7401009841882148a800000114c1e596006f04c050eca6420084";
        final String[] fieldValues = { "WF", "1", "0", "1", "M", "4", "3efb4e0cfa83122b242634254c1920a769d615dfcc4c670bb53eb6f12843c3ae",
                                       "80", "2013-08-31T04:29:15Z", "P00D00H00M", "22", "+30.79658", "-037.82602", "8765", "3210", "042"
                                    };
        WfMessage message;
        try {
            message = WfMessage.Creator.decode(messageEncoded);
        } catch (WfException e) {
            throw e;
        }
        /* Verify */
        assertTrue("Message should be valid", message.isValid());
        assertEquals("Prefix should be correctly set", fieldValues[0], message.getFieldValue("Prefix"));
        assertEquals("Version number should be correctly set", fieldValues[1], message.getFieldValue("Version"));
        assertEquals("Encryption indicator should be correctly set", fieldValues[2], message.getFieldValue("EncryptionIndicator"));
        assertEquals("Duress indicator should be correctly set", fieldValues[3], message.getFieldValue("DuressIndicator"));
        assertEquals("Message code should be correctly set", fieldValues[4], message.getFieldValue("MessageCode"));
        assertEquals("Reference indicator should be correctly set", fieldValues[5], message.getFieldValue("ReferenceIndicator"));
        assertEquals("Referenced message should be correctly set", fieldValues[6], message.getFieldValue("ReferencedMessage"));
        assertEquals("Subject code should be correctly set", fieldValues[7], message.getFieldValue("SubjectCode"));
        assertEquals("DateTime should be correctly set", fieldValues[8], message.getFieldValue("DateTime"));
        assertEquals("Duration should be correctly set", fieldValues[9], message.getFieldValue("Duration"));
        assertEquals("Object code  should be correctly set", fieldValues[10], message.getFieldValue("ObjectType"));
        assertEquals("Latitude should be correctly set", fieldValues[11], message.getFieldValue("ObjectLatitude"));
        assertEquals("Longitude should be correctly set", fieldValues[12], message.getFieldValue("ObjectLongitude"));
        assertEquals("Size dimention 1 should be correctly set", fieldValues[13], message.getFieldValue("ObjectSizeDim1"));
        assertEquals("Size dimention 2 should be correctly set", fieldValues[14], message.getFieldValue("ObjectSizeDim2"));
        assertEquals("Orientation should be correctly set", fieldValues[15], message.getFieldValue("ObjectOrientation"));
    }
    /**
     * Tests serialization of free text message
     */
    @Test
    public void testFreeTextMessage() throws WfException {
        /* Setup */
        WfMessage message1 = WfMessage.Creator.deserialize("WF100F5f6c1e1ed8950b137bb9e0edcf21593d62c03a7fb39dacfd554c593f72c8942dfWhiteflag test message!");
        WfMessage message2 = WfMessage.Creator.decode("57463130232fb60f0f6c4a8589bddcf076e790ac9eb1601d3fd9ced67eaaa62c9fb9644a16fabb434ba32b33630b3903a32b9ba1036b2b9b9b0b3b2908");

        /* Verify */
        assertTrue("Message should be valid", message1.isValid());
        assertTrue("Message should be valid", message2.isValid());
        assertEquals("Prefix should be identical", message1.getFieldValue("Prefix"), message2.getFieldValue("Prefix"));
        assertEquals("Version number should be identical", message1.getFieldValue("Version"), message2.getFieldValue("Version"));
        assertEquals("Encryption indicator should be identical", message1.getFieldValue("EncryptionIndicator"), message2.getFieldValue("EncryptionIndicator"));
        assertEquals("Duress indicator should be identical", message1.getFieldValue("DuressIndicator"), message2.getFieldValue("DuressIndicator"));
        assertEquals("Message code should be identical", message1.getFieldValue("MessageCode"), message2.getFieldValue("MessageCode"));
        assertEquals("Reference indicator should be identical", message1.getFieldValue("ReferenceIndicator"), message2.getFieldValue("ReferenceIndicator"));
        assertEquals("Referenced message should be identical", message1.getFieldValue("ReferencedMessage"), message2.getFieldValue("ReferencedMessage"));
        assertEquals("Subject code should be identical", message1.getFieldValue("Text"), message2.getFieldValue("Text"));
    }
}
