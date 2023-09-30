/*
 * Whiteflag Java Library
 */
package org.whiteflagprotocol.java.util;

import java.io.IOException;
import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.stream.Collectors;
import java.util.Iterator;
import java.util.MissingResourceException;
import java.util.Objects;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.MissingNode;

/* Required error types */
import static org.whiteflagprotocol.java.util.WfUtilException.ErrorType.WF_JSON_ERROR;

/**
 * Whiteflag message schema utility class
 *
 * <p> This unitlity class is used by other classes to validate Whiteflag
 * messages in accordance with the Whiteflag specification. It uses
 * the Whiteflag JSON message schema from resources. This helps to lessen
 * the amount of hardcoded Whiteflag logic and make this implementation 
 * as much version independent and error free as possible.
 * 
 * @since 1.2
 */
public final class WfMessageSchema {

    /* PROPERTIES */

    /**
     *  The Whiteflag message schema resource location
     */
    private static final String RESOURCEFILE = "protocol/v1/WfMessageSchema.json";
    /**
     *  The Whiteflag message schema
     */
    protected static final JsonNode root;           // message schema root
    protected static final JsonNode properties;     // message properties
    protected static final JsonNode definitions;    // message definitions
    protected static final JsonNode specifications; // protocol specifications

    /* STATIC CODE */

    /**
     * Load Whiteflag message schema from resource file
     */
    static {
        try {
            root = mapWfSchema(loadWfSchemaResource(RESOURCEFILE));
        } catch (Exception e) {
            throw new RuntimeException("Cannot statically load Whiteflag message schema", e);
        }
        properties = root.at("/properties");
        definitions = root.at("/definitions");
        specifications = root.at("/specifications");
    }

    /* CONSTRUCTOR */

    /** 
     * Prevents this utility class to be instantiated
     */
    private WfMessageSchema() {
        throw new IllegalStateException("Cannot instantiate Whiteflag message schema utility class");
    }

    /* PUBLIC METHODS */

    /**
     * Get the name of the message type
     * @param messageCode the code indicating the message type
     * @return the name of the message type or null if unable to retrieve
     */
    public static final String getMessageTypeName(String messageCode) {
        JsonNode specification =  getMessageSpecification(messageCode);
        if (specification.isMissingNode()) return null;
        return specification.path("title").textValue();
    }

    /**
     * Get the description of the message type
     * @param messageCode the code indicating the message type
     * @return the description of the message type or null if unable to retrieve
     */
    public static final String getMessageTypeDescription(String messageCode) {
        JsonNode specification =  getMessageSpecification(messageCode);
        if (specification.isMissingNode()) return null;
        return specification.path("description").textValue();
    }

    /* PROTECTED METHODS */

    /**
     * Get the JSON specification of the message type
     * @param messageCode code indicating the message type
     * @return JSON description of the message type or null if not available
     */
    protected static final JsonNode getMessageSpecification(String messageCode) {
        Iterator<JsonNode> i = specifications.path("MessageCode").elements();
        while(i.hasNext()) {
            JsonNode currentNode = i.next();
            String code = currentNode.path("const").textValue();
            if (code != null && Objects.equals(code, messageCode)) {
                return currentNode;
            }
        }
        return MissingNode.getInstance();
    }

    /* PRIVATE METHODS */

    /**
     * Loads Whiteflag message schema resource file into string
     * @param resource the name of the Whiteflag message schema resource file
     * @return JSON string with the Whiteflag message schema
     */
    private static final String loadWfSchemaResource(final String resource) {
        ClassLoader classloader = Thread.currentThread().getContextClassLoader();
        try(InputStream stream = classloader.getResourceAsStream(resource);
            BufferedReader reader = new BufferedReader(new InputStreamReader(stream))) {
            return reader.lines().collect(Collectors.joining(System.lineSeparator()));
        } catch (IOException e) {
            throw new MissingResourceException("Cannot load Whiteflag message schema from resource " + RESOURCEFILE, "", "");
        }
    }

    /**
     * Converts Whiteflag message schema JSON string to JSON node object
     * @param jsonSchemaString a JSON string with the Whiteflag message schema
     * @return JsonNode class representing the Whiteflag message schema
     */
    private static final JsonNode mapWfSchema(final String jsonSchemaString) throws WfUtilException {
        ObjectMapper mapper = new ObjectMapper();
        try {
            return mapper.readTree(jsonSchemaString);
        } catch (JsonProcessingException e) {
            throw new WfUtilException("Resource " + RESOURCEFILE + " does not contain a valid Whiteflag message schema", e, WF_JSON_ERROR);
        }
    }
}