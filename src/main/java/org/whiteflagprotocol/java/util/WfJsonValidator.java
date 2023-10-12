/*
 * Whiteflag Java Library
 */
package org.whiteflagprotocol.java.util;

import java.util.Set;
import java.util.HashSet;

import com.fasterxml.jackson.databind.JsonNode;
import com.networknt.schema.ValidationMessage;

/**
 * Whiteflag JSON validator utility class
 *
 * <p> This utility class is used by other classes to validate Whiteflag
 * JSON messages against the Whiteflag JSON message schema.
 * 
 * @since 1.2
 */
public final class WfJsonValidator {

    /* CONSTRUCTOR */

    /** 
     * Prevents this utility class to be instantiated
     */
    private WfJsonValidator() {
        throw new IllegalStateException("Cannot instantiate Whiteflag JSON validator utility class");
    }

    /* PUBLIC METHODS */

    /**
     * Validates a Whiteflag JSON message against the Whiteflag JSON message schema
     * @param jsonMessage Whiteflag JSON message
     * @return TRUE if message is valid, else FALSE
     */
    public static final boolean validateMessage(WfJsonMessage jsonMessage) {
        return validateNode(jsonMessage.toJsonNode());
    }

    /**
     * Validates a JSON node against the Whiteflag JSON message schema
     * @param jsonNode a JSON node
     * @return TRUE if message is valid, else FALSE
     */
    public static final boolean validateNode(JsonNode jsonNode) {
        if (WfJsonSchema.schema.validate(jsonNode).size() > 0) return false;
        return true;
    }

    /**
     * Inspects a Whiteflag JSON message for errors against the Whiteflag JSON message schema
     * @param jsonMessage Whiteflag JSON message
     * @return a set of errors found in the Whiteflag JSON message
     */
    public static final Set<String> inspectMessage(WfJsonMessage jsonMessage) {
        return inspectNode(jsonMessage.toJsonNode());
    }

    /**
     * Inspects a JSON node for errors against the Whiteflag JSON message schema
     * @param jsonNode a JSON node
     * @return a set of errors found in the JSON node
     */
    public static final Set<String> inspectNode(JsonNode jsonNode) {
        Set<String> errors = new HashSet<>();
        for (ValidationMessage error : WfJsonSchema.schema.validate(jsonNode)) {
            errors.add(error.getMessage());
        }
        return errors;
    }
}