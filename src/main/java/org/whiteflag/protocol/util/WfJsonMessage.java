/*
 * Whiteflag Java Library
 */
package org.whiteflag.protocol.util;

import java.util.Map;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.annotation.JsonGetter;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonPropertyOrder;

/**
 * Whiteflag JSON message representation
 * 
 * </p> This object represents a Whiteflag message represented in the
 * JavaScript Object Notation (JSON) format, in accordance with Annex B
 * of the Whiteflag specification. The basic structure is as follows:
 * <code> { "MetaHeader" : {}, "MessageHeader": {}, "MessageBody": {} }</code>
 */
@JsonPropertyOrder({ "MetaHeader", "MessageHeader", "MessageBody" })
public class WfJsonMessage {

    /* PROPERTIES */

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
    private WfJsonMessage() {};

    /**
     * Creates a new JSON representation of a Whiteflag message
     * @param metadata the key-to-value mapping of metadata
     * @param header the field name-to-value mapping of the message header
     * @param body the field name-to-value mapping of message body
     */
    public WfJsonMessage(Map<String, String> metadata, Map<String, String> header, Map<String, String> body) {
        this.metadata = metadata;
        this.header = header;
        this.body = body;
    }

    /* PUBLIC METHODS: object operations */

    /**
     * Creates a serialized JSON representation of a Whiteflag message
     * @return String with the serialized JSON representation
     * @throws JsonProcessingException if no valid JSON serialization can be created
     */
    public String toJson() throws JsonProcessingException {
        return new ObjectMapper().writeValueAsString(this);
    }

    /**
     * Creates a new JSON representation of a Whiteflag message from a serialized JSON string
     * @param jsonStr String with a JSON representation of a Whiteflag message
     * @throws JsonProcessingException if JSON is invalid
     */
    public static WfJsonMessage create(String jsonStr) throws JsonProcessingException {
        ObjectMapper mapper = new ObjectMapper();
        mapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
        return mapper.readValue(jsonStr, WfJsonMessage.class);
    }

    /* PUBLIC METHODS: getters for mappings */

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
     * @return the field name-to-value mapping of the message header
     */
    @JsonGetter("MessageHeader")
    public Map<String, String> getHeader() {
        return header;
    }

    /**
     * Gets the message body
     * @return the field name-to-value mapping of the message body
     */
    @JsonGetter("MessageBody")
    public Map<String, String> getBody() {
        return body;
    }
}
