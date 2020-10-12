/*
 * Whiteflag Java Library tests
 */
package org.whiteflag.protocol.util;

import org.junit.Test;
import static org.junit.Assert.*;

import java.util.Map;
import com.fasterxml.jackson.core.JsonProcessingException;

/**
 * Whiteflag JSON message test class
 */
public class WfJsonMessageTest {
    /**
     * Tests creating a new message
     */
    @Test
    public void testJsonDeserialization() throws JsonProcessingException {
        /* Setup */
        String jsonMessageStr1A = "{\"MetaHeader\": { },\n\"MessageHeader\": {\"Prefix\":\"WF\",\"Version\":\"1\",\"EncryptionIndicator\":\"0\",\"DuressIndicator\":\"0\",\"MessageCode\":\"F\",\"ReferenceIndicator\":\"5\",\"ReferencedMessage\":\"f6c1e1ed8950b137bb9e0edcf21593d62c03a7fb39dacfd554c593f72c8942df\"},\n\"MessageBody\":{\"Text\": \"Whiteflag test message!\"} } ";
        String jsonMessageStr1B = "{\"MetaHeader\":{},\"MessageHeader\":{\"Prefix\":\"WF\",\"Version\":\"1\",\"EncryptionIndicator\":\"0\",\"DuressIndicator\":\"0\",\"MessageCode\":\"F\",\"ReferenceIndicator\":\"5\",\"ReferencedMessage\":\"f6c1e1ed8950b137bb9e0edcf21593d62c03a7fb39dacfd554c593f72c8942df\"},\"MessageBody\":{\"Text\":\"Whiteflag test message!\"}}";
        WfJsonMessage jsonMessage = WfJsonMessage.create(jsonMessageStr1A);
        String jsonMessageStr2 = jsonMessage.toJson();

        /* Test */
        assertEquals("What goes in must come out", jsonMessageStr1B, jsonMessageStr2);
        assertEquals("Prefix should be correctly set", "WF", jsonMessage.getHeaderField("Prefix"));
        assertEquals("Free text should be correctly set", "Whiteflag test message!", jsonMessage.getBodyField("Text"));
    }
}
