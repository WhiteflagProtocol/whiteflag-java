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
 * Whiteflag JSON message schema test class
 */
public class WfJsonSchemaTest {

    /**
     * Tests loading of Whiteflag message schema from resource file
     */
    @Test
    public void testResourceLoading() {
        /* Get Whiteflag message schema */
        JsonNode schema = WfJsonSchema.root;

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
        String invalid = WfJsonSchema.getMessageTypeName("undefined");
        String name = WfJsonSchema.getMessageTypeName("A");
        String description = WfJsonSchema.getMessageTypeDescription("A");

        /* Verify */
        assertEquals("Invalid message code should return null", invalid, null);
        assertEquals("Whiteflag authentication message should have title", name, "Authentication");
        assertNotEquals("Whiteflag authentication message should have description", description, "");
    }
}