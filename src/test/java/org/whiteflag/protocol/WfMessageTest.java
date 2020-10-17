/*
 * Whiteflag Java Library tests
 */
package org.whiteflag.protocol;

import org.junit.Test;
import static org.junit.Assert.*;

/* Message types required for checking correct message types */
import static org.whiteflag.protocol.core.WfMessageType.*;

/**
 * Whiteflag message representation test class
 */
public class WfMessageTest {
    /**
     * Tests creating a new JSON message
     */
    @Test
    public void testNewMessage() throws WfException {
        /* Setup */
        WfMessage message;
        try {
            message = WfMessage.Creator.type("S");
        } catch (WfException e) {
            throw e;
        }

        /* Verify */
        assertEquals("Message type should be correct", S, message.type);
        assertEquals("Number of fields should be correct", 16, message.getNoFields());
        assertEquals("Number of fields should be equal to number of field names in set", message.getFieldNames().size(), message.getNoFields());
        assertFalse("Message should not be valid without valid field values", message.isValid());
        assertEquals("Prefix should be correctly set", "WF", message.get("Prefix"));
        assertEquals("Version number should be correctly set", "1", message.get("Version"));
        assertEquals("Message code should be correctly set", "S", message.get("MessageCode"));
        assertTrue("Should be able to set field value", message.header.set("EncryptionIndicator", "1"));
        assertFalse("Should not be able to set field value twice", message.header.set("EncryptionIndicator", "2"));
        assertTrue("Should be able to set field value", message.body.set("SubjectCode", "10"));
        assertFalse("Should not be able to set field value twice", message.body.set("SubjectCode", "20"));
        assertTrue("Should be able to set field value", message.set("ObjectType", "21"));
        assertFalse("Should not be able to set field value twice", message.set("ObjectType", "22"));
        assertFalse("Should not be able to set value of non existing field", message.body.set("ReferenceIndicator", "1"));
        assertFalse("Should not be able to set value of non existing field", message.set("NoField", "00"));
    }
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
        assertEquals("Message type should be correct", K, message.type);
        assertEquals("Number of fields should be equal to number of provided fields", fieldValues.length, message.getNoFields());
        assertEquals("Number of fields should be equal to number of field names in set", message.getFieldNames().size(), message.getNoFields());
        assertEquals("Prefix should be correctly set", fieldValues[0], message.get("Prefix"));
        assertEquals("Version number should be correctly set", fieldValues[1], message.get("Version"));
        assertEquals("Encryption indicator should be correctly set", fieldValues[2], message.get("EncryptionIndicator"));
        assertFalse("Should not be able to change duress indicator field", message.header.set("DuressIndicator", "1"));
        assertEquals("Duress indicator should be correctly set", fieldValues[3], message.get("DuressIndicator"));
        assertFalse("Should not be able to change message code field", message.header.set("MessageCode", "Q"));
        assertEquals("Message code should be correctly set", fieldValues[4], message.get("MessageCode"));
        assertEquals("Reference indicator should be correctly set", fieldValues[5], message.get("ReferenceIndicator"));
        assertEquals("Referenced message should be correctly set", fieldValues[6], message.get("ReferencedMessage"));
        assertEquals("Cryptographic data type should be correctly set", fieldValues[7], message.get("CryptoDataType"));
        assertEquals("Cryptographic data should be correctly set", fieldValues[8], message.get("CryptoData"));
        assertTrue("Message should be valid", message.isValid());
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
        assertEquals("Message type should be correct", A, message.type);
        assertEquals("Number of fields should be equal to number of provided fields", fieldValues.length, message.getNoFields());
        assertEquals("Number of fields should be equal to number of field names in set", message.getFieldNames().size(), message.getNoFields());
        assertEquals("Prefix should be correctly set", fieldValues[0], message.get("Prefix"));
        assertEquals("Version number should be correctly set", fieldValues[1], message.get("Version"));
        assertEquals("Encryption indicator should be correctly set", fieldValues[2], message.get("EncryptionIndicator"));
        assertEquals("Duress indicator should be correctly set", fieldValues[3], message.get("DuressIndicator"));
        assertEquals("Message code should be correctly set", fieldValues[4], message.get("MessageCode"));
        assertEquals("Reference indicator should be correctly set", fieldValues[5], message.get("ReferenceIndicator"));
        assertEquals("Referenced message should be correctly set", fieldValues[6], message.get("ReferencedMessage"));
        assertEquals("Verification method type should be correctly set", fieldValues[7], message.get("VerificationMethod"));
        assertEquals("Verification data should be correctly set", fieldValues[8], message.get("VerificationData"));
        assertTrue("Message should be valid", message.isValid());
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
        assertEquals("Message type should be correct", A, message.type);
        assertEquals("Message type should be correct", fieldValues[4], message.type.getCode());
        assertEquals("Number of fields should be equal to number of provided fields", fieldValues.length, message.getNoFields());
        assertEquals("Number of fields should be equal to number of field names in set", message.getFieldNames().size(), message.getNoFields());
        assertEquals("Serialization should be correct", messageSerialized, message.serialize());
        assertEquals("Serialization from cache should be correct", messageSerialized, message.serialize());
        assertTrue("Message should be valid", message.isValid());
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
        assertEquals("Message type should be correct", A, message.type);
        assertEquals("Number of fields should be equal to number of provided fields", 9, message.getNoFields());
        assertEquals("Number of fields should be equal to number of field names in set", message.getFieldNames().size(), message.getNoFields());
        assertEquals("Prefix should be correctly set", messageSerialized.substring(0, 2), message.get("Prefix"));
        assertEquals("Version number should be correctly set", messageSerialized.substring(2, 3), message.get("Version"));
        assertEquals("Encryption indicator should be correctly set", messageSerialized.substring(3, 4), message.get("EncryptionIndicator"));
        assertEquals("Duress indicator should be correctly set", messageSerialized.substring(4, 5), message.get("DuressIndicator"));
        assertEquals("Message code should be correctly set", messageSerialized.substring(5, 6), message.get("MessageCode"));
        assertEquals("Reference indicator should be correctly set", messageSerialized.substring(6, 7), message.get("ReferenceIndicator"));
        assertEquals("Referenced message should be correctly set", messageSerialized.substring(7, 71), message.get("ReferencedMessage"));
        assertEquals("Verification method type should be correctly set", messageSerialized.substring(71, 72), message.get("VerificationMethod"));
        assertEquals("Verification data should be correctly set", messageSerialized.substring(72), message.get("VerificationData"));
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
        assertEquals("Message type should be correct", A, message.type);
        assertEquals("Number of fields should be equal to number of provided fields", fieldValues.length, message.getNoFields());
        assertEquals("Number of fields should be equal to number of field names in set", message.getFieldNames().size(), message.getNoFields());
        assertEquals("Prefix should be correctly set", fieldValues[0], message.get("Prefix"));
        assertEquals("Version number should be correctly set", fieldValues[1], message.get("Version"));
        assertEquals("Encryption indicator should be correctly set", fieldValues[2], message.get("EncryptionIndicator"));
        assertEquals("Duress indicator should be correctly set", fieldValues[3], message.get("DuressIndicator"));
        assertEquals("Message code should be correctly set", fieldValues[4], message.get("MessageCode"));
        assertEquals("Reference indicator should be correctly set", fieldValues[5], message.get("ReferenceIndicator"));
        assertEquals("Referenced message should be correctly set", fieldValues[6], message.get("ReferencedMessage"));
        assertEquals("Subject code should be correctly set", fieldValues[7], message.get("VerificationMethod"));
        assertEquals("DateTime should be correctly set", fieldValues[8], message.get("VerificationData"));
        assertTrue("Message should be valid", message.isValid());
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
        assertEquals("Message type should be correct", M, message.type);
        assertEquals("Number of fields should be equal to number of provided fields", fieldValues.length, message.getNoFields());
        assertEquals("Number of fields should be equal to number of field names in set", message.getFieldNames().size(), message.getNoFields());
        assertEquals("Encoding should be correct", messageEncoded, message.encode());
        assertEquals("Encoding from chache should be correct", messageEncoded, message.encode());
        assertTrue("Message should be valid", message.isValid());
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
        assertEquals("Message type should be correct", M, message.type);
        assertEquals("Number of fields should be equal to number of provided fields", fieldValues.length, message.getNoFields());
        assertEquals("Number of fields should be equal to number of field names in set", message.getFieldNames().size(), message.getNoFields());
        assertEquals("Prefix should be correctly set", fieldValues[0], message.get("Prefix"));
        assertFalse("Should not be able to change version field", message.header.set("Version", "2"));
        assertEquals("Version number should be correctly set", fieldValues[1], message.get("Version"));
        assertEquals("Encryption indicator should be correctly set", fieldValues[2], message.get("EncryptionIndicator"));
        assertEquals("Duress indicator should be correctly set", fieldValues[3], message.get("DuressIndicator"));
        assertEquals("Message code should be correctly set", fieldValues[4], message.get("MessageCode"));
        assertEquals("Reference indicator should be correctly set", fieldValues[5], message.get("ReferenceIndicator"));
        assertEquals("Referenced message should be correctly set", fieldValues[6], message.get("ReferencedMessage"));
        assertEquals("Subject code should be correctly set", fieldValues[7], message.get("SubjectCode"));
        assertEquals("DateTime should be correctly set", fieldValues[8], message.get("DateTime"));
        assertEquals("Duration should be correctly set", fieldValues[9], message.get("Duration"));
        assertEquals("Object code  should be correctly set", fieldValues[10], message.get("ObjectType"));
        assertEquals("Latitude should be correctly set", fieldValues[11], message.get("ObjectLatitude"));
        assertEquals("Longitude should be correctly set", fieldValues[12], message.get("ObjectLongitude"));
        assertEquals("Size dimention 1 should be correctly set", fieldValues[13], message.get("ObjectSizeDim1"));
        assertEquals("Size dimention 2 should be correctly set", fieldValues[14], message.get("ObjectSizeDim2"));
        assertEquals("Orientation should be correctly set", fieldValues[15], message.get("ObjectOrientation"));
        assertTrue("Message should be valid", message.isValid());
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
        assertEquals("Message type should be correct", F, message1.type);
        assertEquals("Message type should be correct", F, message2.type);
        assertEquals("Number of fields should be equal", message1.getNoFields(), message2.getNoFields());
        assertEquals("Prefix should be identical", message1.get("Prefix"), message2.get("Prefix"));
        assertEquals("Version number should be identical", message1.get("Version"), message2.get("Version"));
        assertFalse("Should not be able to change encryption indicator field", message1.header.set("EncryptionIndicator", "2"));
        assertEquals("Encryption indicator should be identical", message1.get("EncryptionIndicator"), message2.get("EncryptionIndicator"));
        assertEquals("Duress indicator should be identical", message1.get("DuressIndicator"), message2.get("DuressIndicator"));
        assertEquals("Message code should be identical", message1.get("MessageCode"), message2.get("MessageCode"));
        assertEquals("Reference indicator should be identical", message1.get("ReferenceIndicator"), message2.get("ReferenceIndicator"));
        assertEquals("Referenced message should be identical", message1.get("ReferencedMessage"), message2.get("ReferencedMessage"));
        assertFalse("Should not be able to change text field", message2.body.set("Text", "alternate text"));
        assertEquals("Text fields should be identical", message1.get("Text"), message2.get("Text"));
        assertTrue("Message should be valid", message1.isValid());
        assertTrue("Message should be valid", message2.isValid());
    }
    /**
     * Tests JSON serialization
     */
    @Test
    public void testJsonSerialization() throws WfException {
        /* Setup */
        WfMessage message1 = WfMessage.Creator.deserialize("WF100F5f6c1e1ed8950b137bb9e0edcf21593d62c03a7fb39dacfd554c593f72c8942dfWhiteflag test message!");
        String jsonMessageStr = message1.toJson();
        WfMessage message2 = WfMessage.Creator.deserializeJson(jsonMessageStr);

        /* Verify */
        assertEquals("Message type should be identical", message1.type, message2.type);
        assertEquals("Number of fields should be equal", message1.getNoFields(), message2.getNoFields());
        assertEquals("Prefix should be identical", message1.get("Prefix"), message2.get("Prefix"));
        assertEquals("Version number should be identical", message1.get("Version"), message2.get("Version"));
        assertFalse("Should not be able to change encryption indicator field", message1.header.set("EncryptionIndicator", "2"));
        assertEquals("Encryption indicator should be identical", message1.get("EncryptionIndicator"), message2.get("EncryptionIndicator"));
        assertEquals("Duress indicator should be identical", message1.get("DuressIndicator"), message2.get("DuressIndicator"));
        assertEquals("Message code should be identical", message1.get("MessageCode"), message2.get("MessageCode"));
        assertEquals("Reference indicator should be identical", message1.get("ReferenceIndicator"), message2.get("ReferenceIndicator"));
        assertEquals("Referenced message should be identical", message1.get("ReferencedMessage"), message2.get("ReferencedMessage"));
        assertFalse("Should not be able to change text field", message2.body.set("Text", "alternate text"));
        assertEquals("Text fields should be identical", message1.get("Text"), message2.get("Text"));
        assertTrue("Message should be valid", message1.isValid());
        assertTrue("Message should be valid", message2.isValid());
    }
    /**
     * Tests JSON deserialization
     */
    @Test
    public void testJsonDeserialization() throws WfException {
        /* Setup */
        String messageStr = "WF100F5f6c1e1ed8950b137bb9e0edcf21593d62c03a7fb39dacfd554c593f72c8942dfWhiteflag test message!";
        String jsonMessageStr = "{\"MetaHeader\":{},\"MessageHeader\":{\"Prefix\":\"WF\",\"Version\":\"1\",\"EncryptionIndicator\":\"0\",\"DuressIndicator\":\"0\",\"MessageCode\":\"F\",\"ReferenceIndicator\":\"5\",\"ReferencedMessage\":\"f6c1e1ed8950b137bb9e0edcf21593d62c03a7fb39dacfd554c593f72c8942df\"},\"MessageBody\":{\"Text\":\"Whiteflag test message!\"}}";
        WfMessage message = WfMessage.Creator.deserializeJson(jsonMessageStr);

        /* Verify */
        assertEquals("Should have no metadata", null, message.getMetadata("transactionHash"));
        assertEquals("Number of fields should be equal to number of field names in set", message.getFieldNames().size(), message.getNoFields());
        assertEquals("Prefix should be correctly set", "WF", message.get("Prefix"));
        assertTrue("Version field should be present", message.getFieldNames().contains("Version"));
        assertTrue("Encryption indicator field should be present", message.getFieldNames().contains("EncryptionIndicator"));
        assertEquals("Free text should be correctly set", "Whiteflag test message!", message.body.get("Text"));
        assertFalse("Should not be able to change text field", message.body.set("Text", "alternate text"));
        assertEquals("Serialization should be correct", messageStr, message.toString());
    }
}
