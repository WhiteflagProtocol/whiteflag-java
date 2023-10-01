/*
 * Whiteflag Java Library tests
 */
package org.whiteflagprotocol.java.util;

import org.junit.Test;
import static org.junit.Assert.*;

/* Field encodings required for field definitions */
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ObjectNode;

/**
 * Whiteflag message validator test class
 */
public class WfMessageSchemaTest {

    /**
     * Static JSON mapper
     */
    private static final ObjectMapper mapper = new ObjectMapper();
    static {
        mapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
    }

    /**
     * Tests loading of Whiteflag message schema from resource file
     */
    @Test
    public void testSchemaResourceLoading() {
        /* Get Whiteflag message schema */
        JsonNode schema = WfMessageSchema.root;

        /* Verify */
        assertTrue("Whiteflag message schema should contain JSON schema objects", (schema instanceof ObjectNode));
        assertEquals("Whiteflag message schema should have the correct identifier", schema.get("$id").asText(), "https://standard.whiteflagprotocol.org/v1/wf-message.schema.json");
    }

    /**
     * Tests retrieving of message type description
     */
    @Test
    public void testMessageSpecification() {
        /* Get name description for authentication messages */
        String invalid = WfMessageSchema.getMessageTypeName("undefined");
        String name = WfMessageSchema.getMessageTypeName("A");
        String description = WfMessageSchema.getMessageTypeDescription("A");

        /* Verify */
        assertEquals("Invalid message code should return null", invalid, null);
        assertEquals("Whiteflag authentication message should have title", name, "Authentication");
        assertNotEquals("Whiteflag authentication message should have description", description, "");
    }

    /**
     * Tests JSON message validation against JSON message schema
     */
    @Test
    public void testJsonMessageValidation1() throws JsonProcessingException {
        /* Create JSON node for Free Text message */
        String jsonMessageStr = "{\"MessageHeader\":{\"Prefix\":\"WF\",\"Version\":\"1\",\"EncryptionIndicator\":\"0\",\"DuressIndicator\":\"0\",\"MessageCode\":\"F\",\"ReferenceIndicator\":\"5\",\"ReferencedMessage\":\"f6c1e1ed8950b137bb9e0edcf21593d62c03a7fb39dacfd554c593f72c8942df\"},\"MessageBody\":{\"Text\":\"Whiteflag test message!\"},\"MetaHeader\":{\"transactionHash\":\"a1b2c3\"}}";
        JsonNode jsonMessageNode = mapper.readTree(jsonMessageStr);

        /* Verify */
        assertTrue("JSON message should validate against JSON message schema", WfMessageSchema.validateNode(jsonMessageNode));
    }

    /**
     * Tests JSON message validation against JSON message schema
     */
    @Test
    public void testJsonMessageValidation2() throws JsonProcessingException {
        /* Create JSON nodes for invalid Free Text message */
        String jsonMessageStr1 = "{\"MessageHeader\":{\"Prefix\":\"WF\",\"Version\":\"1\",\"EncryptionIndicator\":\"0\",\"DuressIndicator\":\"3\",\"MessageCode\":\"F\",\"ReferenceIndicator\":\"5\",\"ReferencedMessage\":\"f6c1e1ed8950b137bb9e0edcf21593d62c03a7fb39dacfd554c593f72c8942df\"},\"MessageBody\":{\"Text\":\"Whiteflag test message!\"},\"MetaHeader\":{\"transactionHash\":\"a1b2c3\"}}";
        String jsonMessageStr2 = "{\"MessageHeader\":{\"Prefix\":\"WF\",\"Version\":\"1\",\"EncryptionIndicator\":\"0\",\"DuressIndicator\":\"0\",\"MessageCode\":\"P\",\"ReferenceIndicator\":\"5\",\"ReferencedMessage\":\"f6c1e1ed8950b137bb9e0edcf21593d62c03a7fb39dacfd554c593f72c8942df\"},\"MessageBody\":{\"Text\":\"Whiteflag test message!\"},\"MetaHeader\":{\"transactionHash\":\"a1b2c3\"}}";
        String jsonMessageStr3 = "{\"MessageHeader\":{\"Prefix\":\"WF\",\"Version\":\"1\",\"EncryptionIndicator\":\"0\",\"DuressIndicator\":\"0\",\"MessageCode\":\"X\",\"ReferenceIndicator\":\"5\",\"ReferencedMessage\":\"f6c1e1ed8950b137bb9e0edcf21593d62c03a7fb39dacfd554c593f72c8942df\"},\"MessageBody\":{\"Text\":\"Whiteflag test message!\"},\"MetaHeader\":{\"transactionHash\":\"a1b2c3\"}}";
        JsonNode jsonMessageNode1 = mapper.readTree(jsonMessageStr1);
        JsonNode jsonMessageNode2 = mapper.readTree(jsonMessageStr2);
        JsonNode jsonMessageNode3 = mapper.readTree(jsonMessageStr3);

        /* Verify */
        assertFalse("JSON message should NOT validate against JSON message schema (invalid DuressIndicator)", WfMessageSchema.validateNode(jsonMessageNode1));
        assertFalse("JSON message should NOT validate against JSON message schema (invalid MessageBody)", WfMessageSchema.validateNode(jsonMessageNode2));
        assertFalse("JSON message should NOT validate against JSON message schema (invalid MessageCode)", WfMessageSchema.validateNode(jsonMessageNode3));
    }
}