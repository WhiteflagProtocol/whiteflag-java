/*
 * Whiteflag Java Library tests
 */
package org.whiteflag.protocol.util;

import org.junit.Test;
import static org.junit.Assert.*;

import java.util.Map;
import com.github.cliftonlabs.json_simple.JsonException;

/**
 * Whiteflag JSON message test class
 */
public class WfJsonMessageTest {
    /**
     * Tests creating a new message
     */
    @Test
    public void testJsonDeserialization() throws JsonException {
        /* Setup */
        String jsonMessageStr = "{\"MetaHeader\":{},\"MessageHeader\":{\"Prefix\":\"WF\",\"Version\":\"1\",\"EncryptionIndicator\":\"0\",\"DuressIndicator\":\"0\",\"MessageCode\":\"F\",\"ReferenceIndicator\":\"5\",\"ReferencedMessage\":\"f6c1e1ed8950b137bb9e0edcf21593d62c03a7fb39dacfd554c593f72c8942df\"},\"MessageBody\":{\"Text\":\"Whiteflag test message!\"}}";
        WfJsonMessage jsonMessage;
        try {
            jsonMessage = new WfJsonMessage(jsonMessageStr);
        } catch (JsonException e) {
            throw e;
        }
        Map<String, String> header = jsonMessage.getHeader();
        Map<String, String> body = jsonMessage.getBody();

        /* Test */
        assertEquals("Prefix should be correctly set", "WF", header.get("Prefix"));
        assertEquals("Version number should be correctly set", "1", header.get("Version"));
        assertEquals("Encryption indicator should be correctly set", "0", header.get("EncryptionIndicator"));
        assertEquals("Duress indicator should be correctly set", "0", header.get("DuressIndicator"));
        assertEquals("Message code should be correctly set", "F", header.get("MessageCode"));
        assertEquals("Reference indicator should be correctly set", "5", header.get("ReferenceIndicator"));
        assertEquals("Referenced message should be correctly set", "f6c1e1ed8950b137bb9e0edcf21593d62c03a7fb39dacfd554c593f72c8942df", header.get("ReferencedMessage"));
        assertEquals("Free text should be correctly set", "Whiteflag test message!", body.get("Text"));
    }
}