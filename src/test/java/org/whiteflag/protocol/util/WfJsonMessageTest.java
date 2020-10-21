/*
 * Whiteflag Java Library tests
 */
package org.whiteflag.protocol.util;

import org.junit.Test;
import static org.junit.Assert.*;

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
        String jsonStr2 = jsonMessage.toJson();

        /* Verify */
        assertEquals("What goes in must come out", jsonStr1B, jsonStr2);
        assertEquals("Should have no metadata", null, jsonMessage.getMetadata().get("transactionHash"));
        assertEquals("Prefix should be correctly set", "WF", jsonMessage.getHeader().get("Prefix"));
        assertEquals("Version number should be correctly set", "1", jsonMessage.getHeader().get("Version"));
        assertEquals("Message code should be correctly set", "F", jsonMessage.getHeader().get("MessageCode"));
        assertEquals("Free text should be correctly set", "Whiteflag test message!", jsonMessage.getBody().get("Text"));
    }
    /**
     * Tests creating a new message without MetaHeader
     */
    @Test
    public void testJsonWithoutMetaheader() throws WfUtilException {
        /* Setup */
        String jsonStr1 = "{\"MessageHeader\":{\"Prefix\":\"WF\",\"Version\":\"1\",\"EncryptionIndicator\":\"0\",\"DuressIndicator\":\"0\",\"MessageCode\":\"F\",\"ReferenceIndicator\":\"5\",\"ReferencedMessage\":\"f6c1e1ed8950b137bb9e0edcf21593d62c03a7fb39dacfd554c593f72c8942df\"},\"MessageBody\":{\"Text\":\"Whiteflag test message!\"}}";
        WfJsonMessage jsonMessage = WfJsonMessage.create(jsonStr1);

        /* Verify */
        assertEquals("Should have no metadata", null, jsonMessage.getMetadata());
        assertEquals("Prefix should be correctly set", "WF", jsonMessage.getHeader().get("Prefix"));
        assertEquals("Version number should be correctly set", "1", jsonMessage.getHeader().get("Version"));
        assertEquals("Message code should be correctly set", "F", jsonMessage.getHeader().get("MessageCode"));
        assertEquals("Free text should be correctly set", "Whiteflag test message!", jsonMessage.getBody().get("Text"));
    }
    /**
     * Tests creating a new message with polluted data
     */
    @Test
    public void testJsonWithPollutedData() throws WfUtilException {
        /* Setup */
        String jsonStr1 = "{\"Object\":{\"foo\":\"bar\"},\"MessageHeader\":{\"Prefix\":\"WF\",\"EncryptionIndicator\":\"0\",\"DuressIndicator\":\"0\",\"MessageCode\":\"F\",\"ReferenceIndicator\":\"5\",\"ReferencedMessage\":\"f6c1e1ed8950b137bb9e0edcf21593d62c03a7fb39dacfd554c593f72c8942df\",\"Version\":\"1\"},\"MessageBody\":{\"Text\":\"Whiteflag test message!\",\"MoreText\":\"JSON test string!\"}}";
        WfJsonMessage jsonMessage = WfJsonMessage.create(jsonStr1);

        /* Verify */
        assertEquals("Should have no metadata", null, jsonMessage.getMetadata());
        assertEquals("Prefix should be correctly set", "WF", jsonMessage.getHeader().get("Prefix"));
        assertEquals("Version number should be correctly set", "1", jsonMessage.getHeader().get("Version"));
        assertEquals("Message code should be correctly set", "F", jsonMessage.getHeader().get("MessageCode"));
        assertEquals("Free text should be correctly set", "Whiteflag test message!", jsonMessage.getBody().get("Text"));
    }
    /**
     * Tests creating a new message with metadata
     */
    @Test
    public void testJsonMetaheader() throws WfUtilException {
        /* Setup */
        String jsonStr1 = "{\"MessageHeader\":{\"Prefix\":\"WF\",\"Version\":\"1\",\"EncryptionIndicator\":\"0\",\"DuressIndicator\":\"0\",\"MessageCode\":\"F\",\"ReferenceIndicator\":\"5\",\"ReferencedMessage\":\"f6c1e1ed8950b137bb9e0edcf21593d62c03a7fb39dacfd554c593f72c8942df\"},\"MessageBody\":{\"Text\":\"Whiteflag test message!\"},\"MetaHeader\":{\"transactionHash\":\"a1b2c3\"}}";
        WfJsonMessage jsonMessage = WfJsonMessage.create(jsonStr1);

        /* Verify */
        assertEquals("Metadata should be correct", "a1b2c3", jsonMessage.getMetadata().get("transactionHash"));
        assertEquals("Prefix should be correctly set", "WF", jsonMessage.getHeader().get("Prefix"));
        assertEquals("Version number should be correctly set", "1", jsonMessage.getHeader().get("Version"));
        assertEquals("Message code should be correctly set", "F", jsonMessage.getHeader().get("MessageCode"));
        assertEquals("Free text should be correctly set", "Whiteflag test message!", jsonMessage.getBody().get("Text"));
    }
}
