/*
 * Whiteflag Java Library
 */
package org.whiteflagprotocol.java.util;

import java.util.Map;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.annotation.JsonGetter;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonPropertyOrder;
import com.fasterxml.jackson.annotation.JsonInclude;

/* Required error types */
import static org.whiteflagprotocol.java.util.WfUtilException.ErrorType.WF_JSON_ERROR;

/**
 * Whiteflag JSON message representation class
 * 
 * <p> This class represents a Whiteflag message represented in the
 * JavaScript Object Notation (JSON) format, in accordance with Annex B
 * of the Whiteflag specification. The basic JSON structure is as follows:
 * 
 * <code> { "MetaHeader" : {...}, "MessageHeader": {...}, "MessageBody": {...} }</code>
 * 
 * @wfref Annex B. JSON Schema of Whiteflag Messages
 * 
 * @since 1.0
 */
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonPropertyOrder({ "MetaHeader", "MessageHeader", "MessageBody" })
public class WfJsonMessage {

    /* PROPERTIES */

    /**
     * Static JSON mapper
     */
    private static final ObjectMapper mapper = new ObjectMapper();
    static {
        mapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
    }

    /**
     * Optional container object for implementation specific metadata about the message or the underlying blockchain
     */
    @JsonProperty("MetaHeader")
    private Map<String, String> metadata;

    /**
     * Container object for the message header fields, which are identical for all message types
     */
    @JsonProperty("MessageHeader")
    private Map<String, String> header;

    /**
     * Container object for the message body, which depends on the message type
     */
    @JsonProperty("MessageBody")
    private Map<String, String> body;

    /* CONSTRUCTORS */

    /**
     * Creates a new empty JSON representation of a Whiteflag message
     */
    @SuppressWarnings("unused")
    private WfJsonMessage() {
        /* Required for Json annotation declarations */
    }

    /**
     * Creates a new JSON representation of a Whiteflag message
     * @param metadata the key-to-value mapping of metadata
     * @param header the fieldname-to-value mapping of the message header
     * @param body the fieldname-to-value mapping of message body
     */
    public WfJsonMessage(Map<String, String> metadata, Map<String, String> header, Map<String, String> body) {
        this.metadata = metadata;
        this.header = header;
        this.body = body;
    }

    /* STATIC FACTORY METHODS */

    /**
     * Creates a new JSON representation of a Whiteflag message from a serialized JSON string
     * @param jsonStr a JSON representation of a Whiteflag message
     * @return a JSON message
     * @throws WfUtilException if JSON is invalid
     */
    public static WfJsonMessage create(String jsonStr) throws WfUtilException {
        WfJsonMessage jsonMessage;
        try {
            jsonMessage = mapper.readValue(jsonStr, WfJsonMessage.class);
        } catch (JsonProcessingException e) {
            throw new WfUtilException("Could not convert JSON string to message", e, WF_JSON_ERROR);
        }
        return jsonMessage;
    }

    /* PUBLIC METHODS */

    /**
     * Gets the message metadata
     * @return the key-to-value mapping of the message metadata
     */
    @JsonGetter("MetaHeader")
    public Map<String, String> getMetadata() {
        return metadata;
    }

    /**
     * Gets the message header
     * @return the fieldname-to-value mapping of the message header
     */
    @JsonGetter("MessageHeader")
    public Map<String, String> getHeader() {
        return header;
    }

    /**
     * Gets the message body
     * @return the fieldname-to-value mapping of the message body
     */
    @JsonGetter("MessageBody")
    public Map<String, String> getBody() {
        return body;
    }

    /**
     * Returns this Whiteflag JSON message object as a JSON string
     * @return the serialized JSON string of the message, or an empty JSON object
     */
    @Override
    public String toString() {
        try {
            return this.toJsonString(); 
        } catch (WfUtilException e) {
            return "{}";
        }
    }

    /**
     * Creates a JSON string representation of the Whiteflag JSON message
     * @return the serialized JSON string of the message
     * @throws WfUtilException if no valid JSON serialization can be created
     */
    public String toJsonString() throws WfUtilException {
        try {
            return mapper.writeValueAsString(this);
        } catch (JsonProcessingException e) {
            throw new WfUtilException("Could not convert message to JSON string", e, WF_JSON_ERROR);
        }
    }

    /**
     * Creates a JSON node object representation of the Whiteflag JSON message
     * @since 1.2
     * @return the JSON node object representation of the message
     */
    public JsonNode toJsonNode() {
        return mapper.valueToTree(this);
    }
}
