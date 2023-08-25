/*
 * Whiteflag Java Library
 */
package org.whiteflagprotocol.java.core;

import java.io.IOException;
import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.stream.Collectors;
import java.util.MissingResourceException;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;

/**
 * Whiteflag message validator unitlity class
 *
 * <p> This unitlity class is used by other classes to define and validate
 * Whiteflag messages in accordance with the Whiteflag specification. It uses
 * the Whiteflag message schema from resources, so that this Whiteflag
 * implentation is not hardcoded, and therefore version independent and error
 * free as much as possible.
 * 
 * @since 1.2
 */
public final class WfMessageValidator {

    static {
        // Loading Whiteflag message schema from resource file
        WfMessageSchema = mapWfSchema(loadWfSchemaResource());
    }

    /* PROPERTIES */

    /**
     *  The Whiteflag message schema resource location
     */
    private static final String WFSCHEMARESOURCE = "protocol/v1/WfMessageSchema.json";

    /**
     *  The Whiteflag message schema
     */
    protected static final JsonNode WfMessageSchema;

    /* CONSTRUCTOR */

    /** 
     * Prevents this utility class to be instantiated
     */
    private WfMessageValidator() {
        throw new IllegalStateException("Cannot instantiate Whiteflag message validator utility class");
    }

    /* PROTECTED METHODS */
    /**
     * Loads Whiteflag message schema resource file into string
     * @return JSON string with the Whiteflag message schema
     */
    protected static final String loadWfSchemaResource() {
        ClassLoader classloader = Thread.currentThread().getContextClassLoader();
        try (InputStream stream = classloader.getResourceAsStream(WFSCHEMARESOURCE);
             BufferedReader reader = new BufferedReader(new InputStreamReader(stream))) {
            return reader.lines().collect(Collectors.joining(System.lineSeparator()));
        } catch (IOException e) {
            throw new MissingResourceException("Cannot load Whiteflag message schema from resource " + WFSCHEMARESOURCE,"","");
        }
    }

    /**
     * Converts Whiteflag message schema JSON string to JSON node object
     * @param jsonSchemaString a JSON string with the Whiteflag message schema
     * @return JsonNode class representing the Whiteflag message schema
     */
    protected static final JsonNode mapWfSchema(final String jsonSchemaString) {
        ObjectMapper mapper = new ObjectMapper();
        try {
            return mapper.readTree(jsonSchemaString);
        } catch (JsonProcessingException e) {
            throw new MissingResourceException("Resource " + WFSCHEMARESOURCE + " does not contain a valid Whiteflag message schema","","");
        }
    }
}
