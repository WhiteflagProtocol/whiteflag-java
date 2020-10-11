/*
 * Whiteflag Java Library
 */
package org.whiteflag.protocol.util;

import java.util.Map;
import com.github.cliftonlabs.json_simple.Jsoner;
import com.github.cliftonlabs.json_simple.JsonObject;
import com.github.cliftonlabs.json_simple.JsonException;

/**
 * Whiteflag JSON message representation
 * 
 * </p> This obejct represents a Whiteflag message represented in the
 * JavaScript Object Notation (JSON) format, in accordance with Annex B
 * of the Whiteflag specification. The basic structure is as follows:
 * <code> {
 *   "MetaHeader" : {},
 *   "MessageHeader": {},
 *   "MessageBody": {}
 * }</code>
 */
public class WfJsonMessage extends JsonObject {

    /* PROPERTIES */

    /**
     * Serial version UID
     */
    private static final long serialVersionUID = 1L;

    /* Main object names */
    /**
     * Name of the optional container object for implementation specific metadata about the message or the underlying blockchain
     */
    public static final String MESSAGE_METADATA = "MetaHeader";
    /**
     * Name of the container object  for the message header fields, which are identical for all message types
     */
    public static final String MESSAGE_HEADER = "MessageHeader";

    /**
     * Name of the container object for the message body, which depends on the message type
     */
    public static final String MESSAGE_BODY = "MessageBody"; 

    /* CONSTRUCTOR */

    /**
     * Creates a new JSON representation of a Whiteflag message from a serialized JSON string
     * @param jsonMessageStr String with a JSON representation of a Whiteflag message
     */
    public WfJsonMessage(String jsonMessageStr) throws JsonException {
        super((JsonObject) Jsoner.deserialize(jsonMessageStr));
    }

    /**
     * Creates a new JSON representation of a Whiteflag message
     * @param header a field name-to-value mapping of the message header
     * @param body a field name-to-value mapping of the message body
     */
    public WfJsonMessage(Map<String, String> header,
                         Map<String, String> body) {
        super();
        this.put(MESSAGE_HEADER, new JsonObject(header));
        this.put(MESSAGE_BODY, new JsonObject(body));
    }

    /**
     * Creates a new JSON representation of a Whiteflag message
     * @param metadata a key-to-value mapping of the message metadata
     * @param header a field name-to-value mapping of the message header
     * @param body a field name-to-value mapping of the message body
     */
    public WfJsonMessage(Map<String, String> metadata,
                         Map<String, String> header,
                         Map<String, String> body) {
        this(header, body);
        this.put(MESSAGE_METADATA, new JsonObject(metadata));
    }

    /* PUBLIC METHODS: getters */

    /**
     * Gets the message metadata
     * @return the key-to-value mapping of the message metadata
     */
    public Map<String, String> getMetadata() {
        return (Map<String,String>) this.get(MESSAGE_METADATA);
    }

    /**
     * Gets the message header
     * @return the field name-to-value mapping of the message header
     */
    public Map<String, String> getHeader() {
        return (Map<String,String>) this.get(MESSAGE_HEADER);
    }

    /**
     * Gets the message body
     * @return the field name-to-value mapping of the message body
     */
    public Map<String, String> getBody() {
        return (Map<String,String>) this.get(MESSAGE_BODY);
    }
}
