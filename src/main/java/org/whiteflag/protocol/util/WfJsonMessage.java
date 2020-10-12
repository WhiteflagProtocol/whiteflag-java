/*
 * Whiteflag Java Library
 */
package org.whiteflag.protocol.util;

import java.util.Map;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * Whiteflag JSON message representation
 * 
 * </p> This object represents a Whiteflag message represented in the
 * JavaScript Object Notation (JSON) format, in accordance with Annex B
 * of the Whiteflag specification. The basic structure is as follows:
 * <code> { "MetaHeader" : {}, "MessageHeader": {}, "MessageBody": {} }</code>
 */
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
     * @throws JsonProcessingException if no valdi JSON serialization can be created
     */
    public String toJson() throws JsonProcessingException {
        return new ObjectMapper().writeValueAsString(this);
    }

    /**
     * Creates a new JSON representation of a Whiteflag message from a serialized JSON string
     * @param jsonMessageStr String with a JSON representation of a Whiteflag message
     * @throws JsonProcessingException if JSON is invalid
     */
    public static WfJsonMessage create(String jsonMessageStr) throws JsonProcessingException {
        ObjectMapper mapper = new ObjectMapper();
        mapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
        return mapper.readValue(jsonMessageStr, WfJsonMessage.class);
    }

    /* PUBLIC METHODS: getters for mappings */

    /**
     * Gets the message metadata
     * @return the key-to-value mapping of the message metadata
     */
    public Map<String, String> getMetadata() {
        return metadata;
    }

    /**
     * Gets the message header
     * @return the field name-to-value mapping of the message header
     */
    public Map<String, String> getHeader() {
        return header;
    }

    /**
     * Gets the message body
     * @return the field name-to-value mapping of the message body
     */
    public Map<String, String> getBody() {
        return body;
    }

    /* PUBLIC METHODS: getters for values */

    /**
     * Gets metadata by key
     * @param key String with the key name
     * @return String with the value
     */
    public String getMetadata(String key) {
        return metadata.get(key);
    }

    /**
     * Gets header field value by field name
     * @param fieldname String header field name
     * @return String with the header field value
     */
    public String getHeaderField(String fieldname) {
        return header.get(fieldname);
    }

    /**
     * Gets body field value by field name
     * @param fieldname String body field name
     * @return String with the body field value
     */
    public String getBodyField(String fieldname) {
        return body.get(fieldname);
    }
}
