/*
 * Whiteflag Java Library
 */
package org.whiteflagprotocol.java.util;

import java.util.Set;

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
        return validateJsonNode(jsonMessage.toJsonNode());
    }

    /**
     * Validates a JSON node against the Whiteflag JSON message schema
     * @param jsonNode a JSON node
     * @return TRUE if message is valid, else FALSE
     */
    public static final boolean validateJsonNode(JsonNode jsonNode) {
        Set<ValidationMessage> errors = WfJsonSchema.schema.validate(jsonNode);
        if (errors.size() > 0) return false;
        return true;
    }

    /**
     * Inspects a Whiteflag JSON message for errors against the Whiteflag JSON message schema
     * @param jsonMessage Whiteflag JSON message
     * @return a set of errors found in the Whiteflag JSON message
     */
    public static final Set<ValidationMessage> inspectMessage(WfJsonMessage jsonMessage) {
        return inspectJsonNode(jsonMessage.toJsonNode());
    }

    /**
     * Inspects a JSON node for errors against the Whiteflag JSON message schema
     * @param jsonNode a JSON node
     * @return a set of errors found in the JSON node
     */
    public static final Set<ValidationMessage> inspectJsonNode(JsonNode jsonNode) {
        //TODO: convert Set of error objects to Set of strings.
        return WfJsonSchema.schema.validate(jsonNode);
    }
}