/*
 * Whiteflag Java Library tests
 */
package org.whiteflagprotocol.java;

import org.junit.Test;
import static org.junit.Assert.*;

import org.whiteflagprotocol.java.core.WfBinaryBuffer;
import org.whiteflagprotocol.java.crypto.WfECDHKeyPair;
import org.whiteflagprotocol.java.crypto.WfEncryptionKey;
import org.whiteflagprotocol.java.crypto.WfCryptoException;
import org.whiteflagprotocol.java.util.WfJsonMessage;
import org.whiteflagprotocol.java.util.WfJsonValidator;

/* Message types required for checking correct message types */
import static org.whiteflagprotocol.java.core.WfMessageType.*;

/**
 * Whiteflag message representation test class
 */
@SuppressWarnings("java:S1192")
public class WfMessageTest {
    /**
     * Tests creating a new JSON message
     */
    @Test
    public void testNewMessage() throws WfException {
        /* Setup */
        WfMessage message = WfMessage.create("S");

        /* Verify message*/
        assertEquals("Message type should be correct", S, message.getType());
        assertEquals("Number of fields should be correct", 16, message.getNoFields());
        assertEquals("Number of fields should be equal to number of field names in set", message.getFieldNames().size(), message.getNoFields());
        assertFalse("Message should not be valid without valid field values", message.isValid());

        /* Verify pre-set fields */ 
        assertEquals("Prefix should be correctly set", "WF", message.get("Prefix"));
        assertEquals("Version number should be correctly set", "1", message.get("Version"));
        assertEquals("Message code should be correctly set", "S", message.get("MessageCode"));

        /* Verify header fields */
        assertTrue("Should be able to set field value", message.set("EncryptionIndicator", "1"));
        assertTrue("Value should be valid", message.isValid("EncryptionIndicator", "2"));
        assertFalse("Should not be able to set field value twice", message.set("EncryptionIndicator", "2"));
        assertTrue("Field should be valid", message.isValid("EncryptionIndicator"));
        assertFalse("Should not be able to set value of non existing field", message.set("ObjectType", "1"));

        /* Verify body fields */
        assertTrue("Should be able to set field value", message.set("SubjectCode", "10"));
        assertFalse("Should not be able to set field value twice", message.set("SubjectCode", "20"));
        assertTrue("Should be able to set field value", message.set("ObjectType", "21"));
        assertFalse("Should not be able to set field value twice", message.set("ObjectType", "22"));
        assertFalse("Should not be able to set value of non existing field", message.set("NoField", "00"));

        /* Verify metadata */
        assertEquals("Metadata should be added", null, message.addMetadata("transactionHash", "a1b2c3"));
        assertEquals("Metadata cannot be added twice", "a1b2c3", message.addMetadata("transactionHash", "d4e5f6"));
        assertEquals("Metadata should be added", null, message.addMetadata("originatorAddress", "abc123"));
        assertEquals("Metadata should return correct value", "abc123", message.getMetadata("originatorAddress"));
        assertEquals("Metadata should have two keys", 2, message.getMetadataKeys().size());
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
        WfMessage message = WfMessage.compile(fieldValues);

        /* Verify message */
        assertEquals("Message type should be correct", K, message.getType());
        assertEquals("Number of fields should be equal to number of provided fields", fieldValues.length, message.getNoFields());
        assertEquals("Number of fields should be equal to number of field names in set", message.getFieldNames().size(), message.getNoFields());
        assertEquals("Prefix should be correctly set", fieldValues[0], message.get("Prefix"));
        assertEquals("Version number should be correctly set", fieldValues[1], message.get("Version"));
        assertEquals("Encryption indicator should be correctly set", fieldValues[2], message.get("EncryptionIndicator"));
        assertFalse("Should not be able to change duress indicator field", message.set("DuressIndicator", "1"));
        assertEquals("Duress indicator should be correctly set", fieldValues[3], message.get("DuressIndicator"));
        assertFalse("Should not be able to change message code field", message.set("MessageCode", "Q"));
        assertEquals("Message code should be correctly set", fieldValues[4], message.get("MessageCode"));
        assertEquals("Reference indicator should be correctly set", fieldValues[5], message.get("ReferenceIndicator"));
        assertEquals("Referenced message should be correctly set", fieldValues[6], message.get("ReferencedMessage"));
        assertEquals("Cryptographic data type should be correctly set", fieldValues[7], message.get("CryptoDataType"));
        assertEquals("Cryptographic data should be correctly set", fieldValues[8], message.get("CryptoData"));
        assertTrue("Message should be valid", message.isValid());

        /* Verify some field data */
        assertFalse("Value should not be valid", message.isValid("DuressIndicator", "2"));
        assertFalse("Value should not be valid", message.isValid("ReferencedMessage", "wrong datatype"));
        assertFalse("Value should not be valid", message.isValid("CryptoDataType", "123"));
        assertTrue("Value should be valid", message.isValid("CryptoDataType", "0A"));
        
        /* Verify JSON */
        assertTrue("The JSON representation should validate against Whiteflag JSON schema",
                    WfJsonValidator.validateMessage(message.toJsonMessage()));
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
        WfMessage message = WfMessage.compile(fieldValues);

        /* Verify */
        assertEquals("Message type should be correct", A, message.getType());
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
        
        /* Verify JSON */
        assertTrue("The JSON representation should validate against Whiteflag JSON schema",
                    WfJsonValidator.validateMessage(message.toJsonMessage()));
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
        WfMessage message = WfMessage.compile(fieldValues);

        /* Verify */
        assertEquals("Message type should be correct", A, message.getType());
        assertEquals("Message type should be correct", fieldValues[4], message.getType().getCode());
        assertEquals("Number of fields should be equal to number of provided fields", fieldValues.length, message.getNoFields());
        assertEquals("Number of fields should be equal to number of field names in set", message.getFieldNames().size(), message.getNoFields());
        assertEquals("Serialization should be correct", messageSerialized, message.serialize());
        assertEquals("Serialization from cache should be correct", messageSerialized, message.serialize());
        assertTrue("Message should be valid", message.isValid());
        
        /* Verify JSON */
        assertTrue("The JSON representation should validate against Whiteflag JSON schema",
                    WfJsonValidator.validateMessage(message.toJsonMessage()));
    }
    /**
     * Tests deserialization of authentication message
     */
    @Test
    public void testAuthMessageDeserialization() throws WfException {
        /* Setup */
        final String messageSerialized = "WF100A000000000000000000000000000000000000000000000000000000000000000001https://organisation.int/whiteflag";
        WfMessage message = WfMessage.deserialize(messageSerialized);

        /* Verify */
        assertEquals("Message type should be correct", A, message.getType());
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
        
        /* Verify JSON */
        assertTrue("The JSON representation should validate against Whiteflag JSON schema",
                    WfJsonValidator.validateMessage(message.toJsonMessage()));
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
        WfMessage message = WfMessage.decode("5746313020800000000000000000000000000000000000000000000000000000000000000000b43a3a38399d1797b7b933b0b734b9b0ba34b7b71734b73a17bbb434ba32b33630b380");

        /* Verify */
        assertEquals("Message type should be correct", A, message.getType());
        assertEquals("Number of fields should be equal to number of provided fields", fieldValues.length, message.getNoFields());
        assertEquals("Number of fields should be equal to number of field names in set", message.getFieldNames().size(), message.getNoFields());
        assertEquals("Prefix should be correctly set", fieldValues[0], message.get("Prefix"));
        assertEquals("Version number should be correctly set", fieldValues[1], message.get("Version"));
        assertEquals("Encryption indicator should be correctly set", fieldValues[2], message.get("EncryptionIndicator"));
        assertEquals("Duress indicator should be correctly set", fieldValues[3], message.get("DuressIndicator"));
        assertEquals("Message code should be correctly set", fieldValues[4], message.get("MessageCode"));
        assertEquals("Reference indicator should be correctly set", fieldValues[5], message.get("ReferenceIndicator"));
        assertEquals("Referenced message should be correctly set", fieldValues[6], message.get("ReferencedMessage"));
        assertEquals("Verification method should be correctly set", fieldValues[7], message.get("VerificationMethod"));
        assertEquals("Verification data should be correctly set", fieldValues[8], message.get("VerificationData"));
        assertTrue("Message should be valid", message.isValid());

        /* Verify JSON */
        assertTrue("The JSON representation should validate against Whiteflag JSON schema",
                    WfJsonValidator.validateMessage(message.toJsonMessage()));
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
            WfMessage message = WfMessage.compile(fieldValues);
            fail("Expected a WfException to be thrown");
            assertFalse("Message should not be valid", message.isValid());
        } catch (WfException e) {
            assertEquals("Should throw a metadata error", WfException.ErrorType.WF_FORMAT_ERROR, e.errorType);
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
        WfMessage message = WfMessage.compile(fieldValues);

        /* Verify */
        assertEquals("Metadata should be added", null, message.addMetadata("transactionHash", "a1b2c3"));
        assertEquals("Metadata cannot be added twice", "a1b2c3", message.addMetadata("transactionHash", "d4e5f6"));
        assertEquals("Metadata should be added", null, message.addMetadata("originatorAddress", "abc123"));
        assertEquals("Metadata should return correct value", "abc123", message.getMetadata("originatorAddress"));
        assertEquals("Metadata should have two keys", 2, message.getMetadataKeys().size());
        assertEquals("Message type should be correct", M, message.getType());
        assertEquals("Number of fields should be equal to number of provided fields", fieldValues.length, message.getNoFields());
        assertEquals("Number of fields should be equal to number of field names in set", message.getFieldNames().size(), message.getNoFields());
        assertEquals("Encoding should be correct", messageEncoded, message.encode().toHexString());
        assertEquals("Encoding from cache should be correct", messageEncoded, message.encode().toHexString());
        assertTrue("Message should be valid", message.isValid());

        /* Verify JSON */
        assertTrue("The JSON representation should validate against Whiteflag JSON schema",
                    WfJsonValidator.validateMessage(message.toJsonMessage()));
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
        WfMessage message = WfMessage.decode(messageEncoded);

        /* Verify */
        assertEquals("Message type should be correct", M, message.getType());
        assertEquals("Number of fields should be equal to number of provided fields", fieldValues.length, message.getNoFields());
        assertEquals("Number of fields should be equal to number of field names in set", message.getFieldNames().size(), message.getNoFields());
        assertEquals("Prefix should be correctly set", fieldValues[0], message.get("Prefix"));
        assertFalse("Should not be able to change version field", message.set("Version", "2"));
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

        /* Verify JSON */
        assertTrue("The JSON representation should validate against Whiteflag JSON schema",
                    WfJsonValidator.validateMessage(message.toJsonMessage()));
    }
    /**
     * Tests test message
     */
    @Test
    @SuppressWarnings("java:S5961")
    public void testTestMessage() throws WfException {
        /* Setup */
        final String messageSerialized = "WF101T33efb4e0cfa83122b242634254c1920a769d615dfcc4c670bb53eb6f12843c3aeM802013-08-31T04:29:15ZP00D00H00M22+30.79658-037.8260287653210042";
        final String[] fieldValues = { "WF", "1", "0", "1", "T", "3", "3efb4e0cfa83122b242634254c1920a769d615dfcc4c670bb53eb6f12843c3ae",
                                       "M", "80", "2013-08-31T04:29:15Z", "P00D00H00M", "22", "+30.79658", "-037.82602", "8765", "3210", "042"
                                    };
        WfMessage message = WfMessage.compile(fieldValues);
        byte[] messageEncoded = message.encode().toByteArray();
        WfMessage messageDecoded = WfMessage.decode(messageEncoded);

        /* Verify */
        assertEquals("Should have no metadata", null, message.getMetadata("transactionHash"));
        assertEquals("Message type should be correct", T, message.getType());
        assertEquals("Decoded message type should be correct", T, messageDecoded.getType());
        assertEquals("Serialization should be correct", messageSerialized, message.serialize());
        assertEquals("Cached serialization should be correct", messageSerialized, message.serialize());
        assertEquals("Decoded message serialization should be correct", messageSerialized, messageDecoded.serialize());
        assertEquals("Number of fields should be equal to number of provided fields", fieldValues.length, message.getNoFields());
        assertEquals("Number of fields should be equal to number of decoded field names in set", message.getFieldNames().size(), messageDecoded.getNoFields());
        assertEquals("Prefix should be correctly set", fieldValues[0], message.get("Prefix"));
        assertEquals("Prefix should be correctly set in decoded message", fieldValues[0], messageDecoded.get("Prefix"));
        assertEquals("Version number should be correctly set", fieldValues[1], message.get("Version"));
        assertEquals("Encryption indicator should be correctly set", fieldValues[2], message.get("EncryptionIndicator"));
        assertEquals("Duress indicator should be correctly set", fieldValues[3], message.get("DuressIndicator"));
        assertEquals("Message code should be correctly set", fieldValues[4], message.get("MessageCode"));
        assertEquals("Reference indicator should be correctly set", fieldValues[5], message.get("ReferenceIndicator"));
        assertFalse("Should not be able to change reference indicator field", message.set("ReferenceIndicator", "6"));
        assertEquals("Referenced message should be correctly set", fieldValues[6], message.get("ReferencedMessage"));
        assertEquals("Referenced message should be correctly set in decoded message", fieldValues[6], messageDecoded.get("ReferencedMessage"));
        assertEquals("Pseudo message code should be correctly set", fieldValues[7], message.get("PseudoMessageCode"));
        assertEquals("Pseudo message code should be correctly set in decoded message", fieldValues[7], messageDecoded.get("PseudoMessageCode"));
        assertEquals("Subject code should be correctly set", fieldValues[8], message.get("SubjectCode"));
        assertEquals("DateTime should be correctly set", fieldValues[9], message.get("DateTime"));
        assertEquals("Duration should be correctly set", fieldValues[10], message.get("Duration"));
        assertEquals("Object code  should be correctly set", fieldValues[11], message.get("ObjectType"));
        assertEquals("Latitude should be correctly set", fieldValues[12], message.get("ObjectLatitude"));
        assertEquals("Longitude should be correctly set", fieldValues[13], message.get("ObjectLongitude"));
        assertEquals("Size dimention 1 should be correctly set", fieldValues[14], message.get("ObjectSizeDim1"));
        assertEquals("Size dimention 2 should be correctly set", fieldValues[15], message.get("ObjectSizeDim2"));
        assertEquals("Orientation should be correctly set in decoded message", fieldValues[16], messageDecoded.get("ObjectOrientation"));
        assertTrue("Message should be valid", message.isValid());
        assertTrue("Decoded message should be valid", messageDecoded.isValid());

        /* Verify metadata */
        assertEquals("Metadata should be added", null, messageDecoded.addMetadata("transactionHash", "a1b2c3"));
        assertEquals("Metadata cannot be added twice", "a1b2c3", messageDecoded.addMetadata("transactionHash", "d4e5f6"));
        assertEquals("Metadata should be added", null, messageDecoded.addMetadata("originatorAddress", "abc123"));
        assertEquals("Metadata should return correct value", "abc123", messageDecoded.getMetadata("originatorAddress"));
        assertEquals("Metadata should have two keys", 2, messageDecoded.getMetadataKeys().size());

        /* Verify JSON */
        // Whiteflag test messages do not validate against schema because serialization is not compliant
    }
    /**
     * Tests compilation of request message
     */
    @Test
    @SuppressWarnings("java:S5961")
    public void testRequestMessage() throws WfException {
        /* Setup */
        final String messageSerialized = "WF101Q13efb4e0cfa83122b242634254c1920a769d615dfcc4c670bb53eb6f12843c3ae802013-08-31T04:29:15ZP01D00H00M22+31.79658-033.826028799321000010022003";
        final String[] fieldValues = { "WF", "1", "0", "1", "Q", "1", "3efb4e0cfa83122b242634254c1920a769d615dfcc4c670bb53eb6f12843c3ae",
                                       "80", "2013-08-31T04:29:15Z", "P01D00H00M", "22", "+31.79658", "-033.82602", "8799", "3210", "000",
                                       "10", "02", "20", "03"
                                    };
        WfMessage message = WfMessage.compile(fieldValues);
        String messageEncoded = message.encode().toHexString();
        WfMessage messageDecoded = WfMessage.decode(messageEncoded);

        /* Verify */
        assertEquals("Message type should be correct", Q, message.getType());
        assertEquals("Decoded message type should be correct", Q, messageDecoded.getType());
        assertEquals("Serialization should be correct", messageSerialized, message.serialize());
        assertEquals("Serialization from cache should be identical", messageDecoded.serialize(), message.serialize());
        assertEquals("Number of fields should be equal to number of provided fields", fieldValues.length, message.getNoFields());
        assertEquals("Number of decoded fields should be equal to number of original field names in set", messageDecoded.getFieldNames().size(), message.getNoFields());
        assertEquals("Prefix should be correctly set", fieldValues[0], message.get("Prefix"));
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
        assertEquals("Request object type 1 is correctly set", fieldValues[16], message.get("ObjectType1"));
        assertEquals("Request object type 1 quantity is correctly set", fieldValues[17], message.get("ObjectType1Quant"));
        assertEquals("Request object type 2 is correctly set", fieldValues[18], message.get("ObjectType2"));
        assertEquals("Request object type 2 quantity is correctly set", fieldValues[19], message.get("ObjectType2Quant"));
        assertTrue("Message header should be valid", message.isValid());
        assertTrue("Message body be valid", message.isValid());
        assertTrue("Message should be valid", message.isValid());
        assertTrue("Decoded message should be valid", messageDecoded.isValid());

        /* Verify JSON */
        // Whiteflag request messages do not validate against schema because serialization is not compliant
    }
    /**
     * Tests serialization of free text message
     */
    @Test
    public void testFreeTextMessage() throws WfException {
        /* Setup */
        WfMessage message1 = WfMessage.deserialize("WF100F5f6c1e1ed8950b137bb9e0edcf21593d62c03a7fb39dacfd554c593f72c8942dfWhiteflag test message!");
        WfMessage message2 = WfMessage.decode("57463130232fb60f0f6c4a8589bddcf076e790ac9eb1601d3fd9ced67eaaa62c9fb9644a16fabb434ba32b33630b3903a32b9ba1036b2b9b9b0b3b2908");

        /* Verify */
        assertEquals("Message type should be correct", F, message1.getType());
        assertEquals("Message type should be correct", F, message2.getType());
        assertEquals("Number of fields should be equal", message1.getNoFields(), message2.getNoFields());
        assertEquals("Prefix should be identical", message1.get("Prefix"), message2.get("Prefix"));
        assertEquals("Version number should be identical", message1.get("Version"), message2.get("Version"));
        assertFalse("Should not be able to change encryption indicator field", message1.set("EncryptionIndicator", "2"));
        assertEquals("Encryption indicator should be identical", message1.get("EncryptionIndicator"), message2.get("EncryptionIndicator"));
        assertEquals("Duress indicator should be identical", message1.get("DuressIndicator"), message2.get("DuressIndicator"));
        assertEquals("Message code should be identical", message1.get("MessageCode"), message2.get("MessageCode"));
        assertEquals("Reference indicator should be identical", message1.get("ReferenceIndicator"), message2.get("ReferenceIndicator"));
        assertEquals("Referenced message should be identical", message1.get("ReferencedMessage"), message2.get("ReferencedMessage"));
        assertFalse("Should not be able to change text field", message2.set("Text", "alternate text"));
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
        WfMessage message1 = WfMessage.deserialize("WF100F5f6c1e1ed8950b137bb9e0edcf21593d62c03a7fb39dacfd554c593f72c8942dfWhiteflag test message!");
        String jsonMessageStr = message1.toJsonString();
        WfMessage message2 = WfMessage.deserializeJson(jsonMessageStr);

        /* Verify */
        assertEquals("Message type should be identical", message1.getType(), message2.getType());
        assertEquals("Number of fields should be equal", message1.getNoFields(), message2.getNoFields());
        assertEquals("Prefix should be identical", message1.get("Prefix"), message2.get("Prefix"));
        assertEquals("Version number should be identical", message1.get("Version"), message2.get("Version"));
        assertFalse("Should not be able to change encryption indicator field", message1.set("EncryptionIndicator", "2"));
        assertEquals("Encryption indicator should be identical", message1.get("EncryptionIndicator"), message2.get("EncryptionIndicator"));
        assertEquals("Duress indicator should be identical", message1.get("DuressIndicator"), message2.get("DuressIndicator"));
        assertEquals("Message code should be identical", message1.get("MessageCode"), message2.get("MessageCode"));
        assertEquals("Reference indicator should be identical", message1.get("ReferenceIndicator"), message2.get("ReferenceIndicator"));
        assertEquals("Referenced message should be identical", message1.get("ReferencedMessage"), message2.get("ReferencedMessage"));
        assertFalse("Should not be able to change text field", message2.set("Text", "alternate text"));
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
        final String messageStr = "WF100F5f6c1e1ed8950b137bb9e0edcf21593d62c03a7fb39dacfd554c593f72c8942dfWhiteflag test message!";
        final String jsonMessageStr = "{\"MetaHeader\":{},\"MessageHeader\":{\"Prefix\":\"WF\",\"Version\":\"1\",\"EncryptionIndicator\":\"0\",\"DuressIndicator\":\"0\",\"MessageCode\":\"F\",\"ReferenceIndicator\":\"5\",\"ReferencedMessage\":\"f6c1e1ed8950b137bb9e0edcf21593d62c03a7fb39dacfd554c593f72c8942df\"},\"MessageBody\":{\"Text\":\"Whiteflag test message!\"}}";
        WfMessage message = WfMessage.deserializeJson(jsonMessageStr);

        /* Verify */
        assertEquals("Should have no metadata", null, message.getMetadata("transactionHash"));
        assertEquals("Number of fields should be equal to number of field names in set", message.getFieldNames().size(), message.getNoFields());
        assertEquals("Prefix should be correctly set", "WF", message.get("Prefix"));
        assertTrue("Version field should be present", message.getFieldNames().contains("Version"));
        assertTrue("Encryption indicator field should be present", message.getFieldNames().contains("EncryptionIndicator"));
        assertEquals("Free text should be correctly set", "Whiteflag test message!", message.get("Text"));
        assertFalse("Should not be able to change text field", message.set("Text", "alternate text"));
        assertEquals("Serialization should be correct", messageStr, message.toString());
    }
    /**
     * Tests message encryption
     */
    @Test
    public void testMessageEncryption1() throws WfException {
        /* Setup */
        final String encodedMsg = "5746313223000000000088888889111111119999999a22222222aaaaaaab33333333bbbbbbbb0983098309830983118b118b118b118b1993199319931993219b219b219b219b29a329a329a329a331ab31ab31ab31a9b1b9b1b9b1b9b1b9c1c9c1c9c1c9c1c8";
        final String encryptedMsg = "574631326d7658e7d17479677a0de95076989fcd7825b709349b143f2b17644e5cb2c8ded5c7f18d77447cf9dc2115e0c1c81d717b57fadaeedf27bfef8926448ff666d3d9a65168827c94b393974ebbe6b7f0599e184bfd1ace3569117c23ae17c5640f2f2d";

        WfDummyAccount originator = new WfDummyAccount(true);
        WfDummyAccount recipient = new WfDummyAccount(false);
        originator.setAddress("007a0baf6f84f0fa7402ea972686e56d50b707c9b67b108866");
        recipient.setSharedKey(new WfEncryptionKey("32676187ba7badda85ea63a69870a7133909f1999774abb2eed251073616a6e7"));

        WfMessage message = WfMessage.decode(encodedMsg).copy();    // Must copy, otherwise encoded message is already set
        message.setOriginator(originator);
        message.setRecipient(recipient);
        message.setInitVector("40aa85015d24e4601448c1ba8d7bf1aa");

        /* Verification */
        assertTrue("We should be the originator ourselves", originator.isSelf());
        assertFalse("They should be the rescipient", recipient.isSelf());
        assertEquals("Message should be correctly encrypted", encryptedMsg, message.encode().toHexString());
    }
    /**
     * Tests message encryption and decryption with pre-shared key
     */
    @Test
    public void testMessageEncryption2() throws WfException {
        /* Setup */
        WfDummyAccount originator = new WfDummyAccount(true);
        WfDummyAccount recipient = new WfDummyAccount(false);
        originator.setAddress("ac000cdbe3c49955b218f8397ddfe533a32a4269658712a2f4a82e8b448e");
        recipient.setSharedKey(new WfEncryptionKey("b50cf705febdc9b6b2f7af10fa0955c1a5b454d6941494536d75d7810010a90d"));

        final String messageStr = "WF120F5f6c1e1ed8950b137bb9e0edcf21593d62c03a7fb39dacfd554c593f72c8942dfWhiteflag test message!";
        WfMessage message1 = WfMessage.deserialize(messageStr);
        message1.setOriginator(originator);
        message1.setRecipient(recipient);

        /* Encryption */
        WfBinaryBuffer encryptedMsg = message1.encode();
        byte[] initVector = message1.getInitVector();
        WfMessage message2 = WfMessage.decrypt(encryptedMsg, originator, recipient, initVector);

        /* Verification */
        assertEquals("Serialized decrypted message should be identical to original", messageStr, message2.serialize());
        assertEquals("ReferencedMessage field should be identical", message1.get("ReferencedMessage"), message2.get("ReferencedMessage"));
        assertEquals("Text field should be identical", message1.get("Text"), message2.get("Text"));
    }
    /**
     * Tests message encryption and decryption with negotiated key
     */
    @Test
    public void testMessageEncryption3() throws WfException, WfCryptoException {
        /* Setup */
        WfDummyAccount originator = new WfDummyAccount(false);
        WfDummyAccount recipient = new WfDummyAccount(true);
        originator.setAddress("b77b1cdb02efe1acccf0e277021cb303117bd83c689ea8a64fc549229dba");
        originator.setEcdhPublicKey(new WfECDHKeyPair().getPublicKey());
        recipient.setEcdhKeyPair(new WfECDHKeyPair());

        final String messageStr = "WF111Q13efb4e0cfa83122b242634254c1920a769d615dfcc4c670bb53eb6f12843c3ae802013-08-31T04:29:15ZP01D00H00M22+31.79658-033.826028799321000010022003";
        WfMessage message1 = WfMessage.deserialize(messageStr);
        message1.setOriginator(originator);
        message1.setRecipient(recipient);

        /* Encryption */
        WfBinaryBuffer encryptedMsg = message1.encrypt();
        byte[] initVector = message1.getInitVector();
        WfMessage message2 = WfMessage.decrypt(encryptedMsg, originator, recipient, initVector);

        /* Verification */
        assertEquals("Serialized decrypted message should be identical to original", messageStr, message2.serialize());
        assertEquals("ReferencedMessage field should be identical", message1.get("ReferencedMessage"), message2.get("ReferencedMessage"));
        assertEquals("DateTime field should be identical", message1.get("DateTime"), message2.get("DateTime"));
    }
    /**
     * Tests message encryption and decryption missing metadata
     */
    @Test
    @SuppressWarnings("unused")
    public void testMessageEncryption4() throws WfException, WfCryptoException {
        /* Setup */
        WfDummyAccount originator = new WfDummyAccount(false);
        WfDummyAccount recipient = new WfDummyAccount(true);
        originator.setAddress("b77b1cdb02efe1acccf0e277021cb303117bd83c689ea8a64fc549229dba");
        recipient.setEcdhKeyPair(new WfECDHKeyPair());

        WfMessage message = WfMessage.deserialize("WF111Q13efb4e0cfa83122b242634254c1920a769d615dfcc4c670bb53eb6f12843c3ae802013-08-31T04:29:15ZP01D00H00M22+31.79658-033.826028799321000010022003");
        message.setOriginator(originator);
        message.setRecipient(recipient);

        /* Encryption: orginator public ECDH key has not been set! */
        try {
            WfBinaryBuffer encryptedMsg = message.encode();
            byte[] initVector = message.getInitVector();
            fail("Expected a WfException to be thrown");
        } catch (WfException e) {
            assertEquals("Should throw a metadata error", WfException.ErrorType.WF_METADATA_ERROR, e.errorType);
        }
    }
}
