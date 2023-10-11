/*
 * Whiteflag Java Library tests
 */
package org.whiteflagprotocol.java.util;

import org.junit.Test;
import static org.junit.Assert.*;

/* Field encodings required for field definitions */
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;

/**
 * Whiteflag JSON validator utility test class
 */
public class WfJsonValidatorTest {

    /**
     * Static JSON mapper
     */
    private static final ObjectMapper mapper = new ObjectMapper();
    static {
        mapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
    }

    /**
     * Tests JSON message validation against JSON message schema
     */
    @Test
    public void testJsonMessageValidation1() throws JsonProcessingException {
        /* Create JSON node for valid Free Text message */
        JsonNode jsonMessageNode = mapper.readTree("{\"MessageHeader\":{\"Prefix\":\"WF\",\"Version\":\"1\",\"EncryptionIndicator\":\"0\",\"DuressIndicator\":\"0\",\"MessageCode\":\"F\",\"ReferenceIndicator\":\"5\",\"ReferencedMessage\":\"f6c1e1ed8950b137bb9e0edcf21593d62c03a7fb39dacfd554c593f72c8942df\"},\"MessageBody\":{\"Text\":\"Whiteflag test message!\"},\"MetaHeader\":{\"transactionHash\":\"a1b2c3\"}}");

        /* Verify */
        assertTrue("JSON message should validate against JSON message schema", WfJsonValidator.validateJsonNode(jsonMessageNode));
    }

    /**
     * Tests JSON message validation against JSON message schema
     */
    @Test
    public void testJsonMessageValidation2() throws JsonProcessingException {
        /* Create JSON nodes for invalid Free Text message */
        JsonNode jsonMessageNode1 = mapper.readTree("{\"MessageHeader\":{\"Prefix\":\"WF\",\"Version\":\"1\",\"EncryptionIndicator\":\"0\",\"DuressIndicator\":\"3\",\"MessageCode\":\"F\",\"ReferenceIndicator\":\"5\",\"ReferencedMessage\":\"f6c1e1ed8950b137bb9e0edcf21593d62c03a7fb39dacfd554c593f72c8942df\"},\"MessageBody\":{\"Text\":\"Whiteflag test message!\"},\"MetaHeader\":{\"transactionHash\":\"a1b2c3\"}}");
        JsonNode jsonMessageNode2 = mapper.readTree("{\"MessageHeader\":{\"Prefix\":\"WF\",\"Version\":\"1\",\"EncryptionIndicator\":\"0\",\"DuressIndicator\":\"0\",\"MessageCode\":\"P\",\"ReferenceIndicator\":\"5\",\"ReferencedMessage\":\"f6c1e1ed8950b137bb9e0edcf21593d62c03a7fb39dacfd554c593f72c8942df\"},\"MessageBody\":{\"Text\":\"Whiteflag test message!\"},\"MetaHeader\":{\"transactionHash\":\"a1b2c3\"}}");
        JsonNode jsonMessageNode3 = mapper.readTree("{\"MessageHeader\":{\"Prefix\":\"WF\",\"Version\":\"1\",\"EncryptionIndicator\":\"0\",\"DuressIndicator\":\"0\",\"MessageCode\":\"X\",\"ReferenceIndicator\":\"5\",\"ReferencedMessage\":\"f6c1e1ed8950b137bb9e0edcf21593d62c03a7fb39dacfd554c593f72c8942df\"},\"MessageBody\":{\"Text\":\"Whiteflag test message!\"},\"MetaHeader\":{\"transactionHash\":\"a1b2c3\"}}");

        /* Verify */
        assertFalse("JSON message should NOT validate against JSON message schema (invalid DuressIndicator)", WfJsonValidator.validateJsonNode(jsonMessageNode1));
        assertFalse("JSON message should NOT validate against JSON message schema (invalid MessageBody)", WfJsonValidator.validateJsonNode(jsonMessageNode2));
        assertFalse("JSON message should NOT validate against JSON message schema (invalid MessageCode)", WfJsonValidator.validateJsonNode(jsonMessageNode3));
    }
}