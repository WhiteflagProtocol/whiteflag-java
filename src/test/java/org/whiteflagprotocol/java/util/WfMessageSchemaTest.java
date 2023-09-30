/*
 * Whiteflag Java Library tests
 */
package org.whiteflagprotocol.java.util;

import org.junit.Test;
import static org.junit.Assert.*;

/* Field encodings required for field definitions */
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ObjectNode;

/**
 * Whiteflag message validator test class
 */
public class WfMessageSchemaTest {
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
        /* Get description for authentication messages */
        String invalid = WfMessageSchema.getMessageTypeName("undefined");
        String name = WfMessageSchema.getMessageTypeName("A");
        String description = WfMessageSchema.getMessageTypeDescription("A");

        /* Verify */
        assertEquals("Invalid message code should return null", invalid, null);
        assertEquals("Whiteflag authentication message should have title", name, "Authentication");
        assertNotEquals("Whiteflag authentication message should have description", description, "");
    }
}