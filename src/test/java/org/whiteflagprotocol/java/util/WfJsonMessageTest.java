/*
 * Whiteflag Java Library tests
 */
package org.whiteflagprotocol.java.util;

import org.junit.Test;
import static org.junit.Assert.*;

import com.fasterxml.jackson.databind.JsonNode;

/**
 * Whiteflag JSON message test class
 */
public class WfJsonMessageTest {
    /**
     * Tests creating a new message
     */
    @Test
    public void testJsonDeserialization() throws WfUtilException {
        /* Setup */
        String jsonStr1A = "{\"MetaHeader\": { },\n\"MessageHeader\": {\"Prefix\":\"WF\",\"Version\":\"1\",\"EncryptionIndicator\":\"0\",\"DuressIndicator\":\"0\",\"MessageCode\":\"F\",\"ReferenceIndicator\":\"5\",\"ReferencedMessage\":\"f6c1e1ed8950b137bb9e0edcf21593d62c03a7fb39dacfd554c593f72c8942df\"},\n\"MessageBody\":{\"Text\": \"Whiteflag test message!\"} } ";
        String jsonStr1B = "{\"MetaHeader\":{},\"MessageHeader\":{\"Prefix\":\"WF\",\"Version\":\"1\",\"EncryptionIndicator\":\"0\",\"DuressIndicator\":\"0\",\"MessageCode\":\"F\",\"ReferenceIndicator\":\"5\",\"ReferencedMessage\":\"f6c1e1ed8950b137bb9e0edcf21593d62c03a7fb39dacfd554c593f72c8942df\"},\"MessageBody\":{\"Text\":\"Whiteflag test message!\"}}";
        WfJsonMessage jsonMessage = WfJsonMessage.create(jsonStr1A);
        String jsonStr2 = jsonMessage.toJsonString();

        /* Verify */
        assertEquals("What goes in must come out", jsonStr1B, jsonStr2);
        assertEquals("Should have no metadata", null, jsonMessage.getMetadata().get("transactionHash"));
        assertEquals("Prefix should be correctly set", "WF", jsonMessage.getHeader().get("Prefix"));
        assertEquals("Version number should be correctly set", "1", jsonMessage.getHeader().get("Version"));
        assertEquals("Message code should be correctly set", "F", jsonMessage.getHeader().get("MessageCode"));
        assertEquals("Free text should be correctly set", "Whiteflag test message!", jsonMessage.getBody().get("Text"));
        assertTrue("JSON message should validate against JSON message schema", WfJsonValidator.validateMessage(jsonMessage));
    }
    /**
     * Tests creating a new message without MetaHeader
     */
    @Test
    public void testJsonWithoutMetaheader() throws WfUtilException {
        /* Setup */
        WfJsonMessage jsonMessage = WfJsonMessage.create("{\"MessageHeader\":{\"Prefix\":\"WF\",\"Version\":\"1\",\"EncryptionIndicator\":\"0\",\"DuressIndicator\":\"0\",\"MessageCode\":\"F\",\"ReferenceIndicator\":\"5\",\"ReferencedMessage\":\"f6c1e1ed8950b137bb9e0edcf21593d62c03a7fb39dacfd554c593f72c8942df\"},\"MessageBody\":{\"Text\":\"Whiteflag test message!\"}}");

        /* Verify */
        assertEquals("Should have no metadata", null, jsonMessage.getMetadata());
        assertEquals("Prefix should be correctly set", "WF", jsonMessage.getHeader().get("Prefix"));
        assertEquals("Version number should be correctly set", "1", jsonMessage.getHeader().get("Version"));
        assertEquals("Message code should be correctly set", "F", jsonMessage.getHeader().get("MessageCode"));
        assertEquals("Free text should be correctly set", "Whiteflag test message!", jsonMessage.getBody().get("Text"));
        assertTrue("JSON message should validate against JSON message schema", WfJsonValidator.validateMessage(jsonMessage));
    }
    /**
     * Tests creating a new message with polluted data
     */
    @Test
    public void testJsonWithPollutedData() throws WfUtilException {
        /* Setup */
        WfJsonMessage jsonMessage = WfJsonMessage.create("{\"Object\":{\"foo\":\"bar\"},\"MessageHeader\":{\"Prefix\":\"WF\",\"EncryptionIndicator\":\"0\",\"DuressIndicator\":\"0\",\"MessageCode\":\"F\",\"ReferenceIndicator\":\"5\",\"ReferencedMessage\":\"f6c1e1ed8950b137bb9e0edcf21593d62c03a7fb39dacfd554c593f72c8942df\",\"Version\":\"1\"},\"MessageBody\":{\"Text\":\"Whiteflag test message!\",\"MoreText\":\"JSON test string!\"}}");

        /* Verify */
        assertEquals("Should have no metadata", null, jsonMessage.getMetadata());
        assertEquals("Prefix should be correctly set", "WF", jsonMessage.getHeader().get("Prefix"));
        assertEquals("Version number should be correctly set", "1", jsonMessage.getHeader().get("Version"));
        assertEquals("Message code should be correctly set", "F", jsonMessage.getHeader().get("MessageCode"));
        assertEquals("Free text should be correctly set", "Whiteflag test message!", jsonMessage.getBody().get("Text"));
        assertFalse("JSON message should NOT validate against JSON message schema", WfJsonValidator.validateMessage(jsonMessage));
    }
    /**
     * Tests creating a new message with metadata
     */
    @Test
    public void testJsonMetaheader() throws WfUtilException {
        /* Setup */
        WfJsonMessage jsonMessage = WfJsonMessage.create("{\"MessageHeader\":{\"Prefix\":\"WF\",\"Version\":\"1\",\"EncryptionIndicator\":\"0\",\"DuressIndicator\":\"0\",\"MessageCode\":\"F\",\"ReferenceIndicator\":\"5\",\"ReferencedMessage\":\"f6c1e1ed8950b137bb9e0edcf21593d62c03a7fb39dacfd554c593f72c8942df\"},\"MessageBody\":{\"Text\":\"Whiteflag test message!\"},\"MetaHeader\":{\"transactionHash\":\"a1b2c3\"}}");

        /* Verify */
        assertEquals("Metadata should be correct", "a1b2c3", jsonMessage.getMetadata().get("transactionHash"));
        assertEquals("Prefix should be correctly set", "WF", jsonMessage.getHeader().get("Prefix"));
        assertEquals("Version number should be correctly set", "1", jsonMessage.getHeader().get("Version"));
        assertEquals("Message code should be correctly set", "F", jsonMessage.getHeader().get("MessageCode"));
        assertEquals("Free text should be correctly set", "Whiteflag test message!", jsonMessage.getBody().get("Text"));
        assertTrue("JSON message should validate against JSON message schema", WfJsonValidator.validateMessage(jsonMessage));
    }
    /**
     * Tests creating a new message with metadata
     */
    @Test
    public void testAnotherJson() throws WfUtilException {
        /* Setup */
        WfJsonMessage jsonMessage = WfJsonMessage.create("{\"MetaHeader\":{\"blockchain\":\"ethereum\",\"originatorAddress\":\"8a1bf0e6df25fe3736adba6cfaa6aec4bf9afc20\"},\"MessageHeader\":{\"Prefix\":\"WF\",\"Version\":\"1\",\"EncryptionIndicator\":\"0\",\"DuressIndicator\":\"0\",\"MessageCode\":\"S\",\"ReferenceIndicator\":\"0\",\"ReferencedMessage\":\"4c1b8331ba07cf644d4d5065f5c954fe1370e3b0fa0cab37f3b87261f153673d\"},\"MessageBody\":{\"SubjectCode\":\"10\",\"DateTime\":\"2018-01-01T00:00:00Z\",\"Duration\":\"P00D00H00M\",\"ObjectType\":\"11\",\"ObjectLatitude\":\"+50.00000\",\"ObjectLongitude\":\"+005.00000\",\"ObjectSizeDim1\":\"0003\",\"ObjectSizeDim2\":\"0016\",\"ObjectOrientation\":\"000\"}}");

        /* Verify */
        assertEquals("Metadata should be correct", "8a1bf0e6df25fe3736adba6cfaa6aec4bf9afc20", jsonMessage.getMetadata().get("originatorAddress"));
        assertEquals("Referenced should be correctly set", "4c1b8331ba07cf644d4d5065f5c954fe1370e3b0fa0cab37f3b87261f153673d", jsonMessage.getHeader().get("ReferencedMessage"));
        assertEquals("Message code should be correctly set", "S", jsonMessage.getHeader().get("MessageCode"));
        assertEquals("DateTime should be correctly set", "2018-01-01T00:00:00Z", jsonMessage.getBody().get("DateTime"));
        assertEquals("Object size dimension 1 should be correctly set", "0003", jsonMessage.getBody().get("ObjectSizeDim1"));
        assertEquals("Object size dimension 2 correctly set", "0016", jsonMessage.getBody().get("ObjectSizeDim2"));
        assertTrue("JSON message should validate against JSON message schema", WfJsonValidator.validateMessage(jsonMessage));
    }
    /**
     * Tests creating a JSON node object
     */
    @Test
    public void testJsonObject() throws WfUtilException {
        WfJsonMessage jsonMessage = WfJsonMessage.create("{\"MessageHeader\":{\"Prefix\":\"WF\",\"Version\":\"1\",\"EncryptionIndicator\":\"0\",\"DuressIndicator\":\"0\",\"MessageCode\":\"F\",\"ReferenceIndicator\":\"5\",\"ReferencedMessage\":\"f6c1e1ed8950b137bb9e0edcf21593d62c03a7fb39dacfd554c593f72c8942df\"},\"MessageBody\":{\"Text\":\"Object mapping test\"}}");
        JsonNode jsonNode = jsonMessage.toJsonNode();

        /* Verify */
        assertEquals("Data should be correctly transformed", jsonMessage.getBody().get("Text"), jsonNode.at("/MessageBody/Text").textValue());
        assertTrue("JSON message should validate against JSON message schema", WfJsonValidator.validateMessage(jsonMessage));
        assertTrue("JSON node should validate against JSON message schema", WfJsonValidator.validateNode(jsonNode));
    }
}
