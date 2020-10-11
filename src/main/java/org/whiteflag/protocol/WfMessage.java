/*
 * Whiteflag Java Library
 */
package org.whiteflag.protocol;

import java.util.Set;
import java.util.Map;
import java.util.HashMap;
import com.github.cliftonlabs.json_simple.JsonException;

/* Required Whiteflag core and util classes */
import org.whiteflag.protocol.core.*;
import org.whiteflag.protocol.util.WfJsonMessage;

/**
 * Whiteflag message class
 * 
 * </p> This is a class representing a Whiteflag message. It contains
 * all methods to handle a Whiteflag message, e.g. to encode, decode, etc. It
 * also provides a nested static class to create Whiteflag messages.
 */
public class WfMessage extends WfMessageCore {

    /* PROPERTIES */

    /**
     * Contains implementation specific message metadata
     */
    private Map<String, String> metadata = new HashMap<>();

    /**
     * Contains the cached serialzed and encoded message
     */
    private String messageEncoded;
    private String messageSerialized;

    /* CONSTRUCTORS */

    /**
     * Creates a Whiteflag message by calling the super constructor with an array of field values
     * @param type the {@link WfMessageType} of the message
     * @param header the {@link WfMessageSegment} message header
     * @param body the {@link WfMessageSegment} message body
     */
    private WfMessage(final WfMessageType type, final WfMessageSegment header, final WfMessageSegment body) {
        super(type, header, body);
    }

    /* PUBLIC METHODS: getters & setters */

    /**
     * Adds metadata to the Whiteflag message if not already existing
     * @return null if successful, otherwise the value of the already existing key
     */
    public String addMetadata(final String key, final String value) {
        return metadata.putIfAbsent(key, value);
    }

    /**
     * Returns the requested metadata value of the Whiteflag message
     * @return a string with the value of the requested metadata key
     */
    public String getMetadata(final String key) {
        return metadata.get(key);
    }

    /**
     * Returns metadata keys of the Whiteflag message
     * @return a string set with all metadata keys
     */
    public Set<String> getMetadataKeys() {
        return metadata.keySet();
    }

    /* PUBLIC METHODS: operations */

    /**
     * Returns the cached serialized message, or else it serialzes and caches Whiteflag message
     * @return String with the serialized message, i.e. the concatinated string of field values
     * @throws WfException if any of the field does not contain valid data
     */
    @Override
    public String serialize() throws WfException {
        try {
            if (messageSerialized == null) {
                messageSerialized = super.serialize();
            }
        } catch (WfCoreException e) {
            throw new WfException(e.getMessage(), WfException.ErrorType.WF_FORMAT_ERROR);
        }
        return messageSerialized;
    }

    /**
     * Returns the cached encoded message, or else it encodes and caches Whiteflag message without 0x prefix
     * @return hexadecimal string with the encoded Whiteflag message
     * @throws WfException if any field does not contain valid data
     */
    @Override
    public String encode() throws WfException {
        return encode(false);
    }

    /**
     * Returns the cached encoded message, or else it encodes and caches Whiteflag message
     * @param prefix if TRUE, the resulting string gets a 0x prefix (or whatever has been cached)
     * @return hexadecimal string with the encoded Whiteflag message
     * @throws WfException if any field does not contain valid data
     */
    @Override
    public String encode(final Boolean prefix) throws WfException {
        try {
            if (messageEncoded == null) {
                messageEncoded = super.encode(prefix);
            }
        } catch (WfCoreException e) {
            throw new WfException(e.getMessage(), WfException.ErrorType.WF_FORMAT_ERROR);
        }
        return messageEncoded;
    }

    /**
     * Returns the serialised JSON representation of the Whiteflag message 
     * @return String with the serialised JSON representation
     */
    public String toJson() {
        return new WfJsonMessage(metadata, header.toMap(), body.toMap()).toJson();
    }

    /* PRIVATE METHODS */

    /**
     * Returns the requested metadata value of the Whiteflag message
     * @return a string with the value of the requested metadata key
     */
    private void setMetadata(Map<String, String> metadata) {
        metadata.forEach(this.metadata::put);
    }

    /* NESTED CLASSES */

    /**
     * Whiteflag nested message creator class
     * 
     * </p> This is a nested builder class to create a Whiteflag message. It
     * calls the core builder to create a message i.a.w. the Whiteflag
     * specification.
     */
    public static class Creator {

        /* CONSTRUCTORS */

        /**
         * Prevents the static class to be instantiated
         */
        private Creator() {
            throw new IllegalStateException("Cannot instantiate static class");
        }

        /* METHODS */

        /**
         * Creates a new empty Whiteflag message object of the specified type
         * @param messageCode the code indicating the message type to be created
         * @return a new {@link WfMessage} Whiteflag message
         */
        public static final WfMessage type(final String messageCode) throws WfException {
            WfMessageCore messageCore;
            try {
                messageCore = new WfMessageCreator().type(WfMessageType.getType(messageCode)).create();
            } catch (WfCoreException e) {
                throw new WfException("Cannot create new message of type " + messageCode + ": " + e.getMessage(), WfException.ErrorType.WF_FORMAT_ERROR);
            }
            return new WfMessage(messageCore.type, messageCore.header, messageCore.body);
        }

        /**
         * Copies a Whiteflag message into new Whiteflag message object
         * @param originalMessage the {@link WfMessage} to be copied
         * @return a {@link WfMessage} Whiteflag message
         */
        public static final WfMessage copy(final WfMessage originalMessage) {
            WfMessage message = new WfMessage(originalMessage.type, new WfMessageSegment(originalMessage.header), new WfMessageSegment(originalMessage.body));
            for (String key : originalMessage.getMetadataKeys()) {
                message.addMetadata(key, originalMessage.getMetadata(key));
            }
            return message;
        }

        /**
         * Creates a new Whiteflag message object from a serialized message
         * @param messageSerialized String with the uncompressed serialized message
         * @return a {@link WfMessage} Whiteflag message
         * @throws WfException if the serialization of the message is invalid
         */
        public static final WfMessage deserialize(final String messageSerialized) throws WfException {
            WfMessageCore messageCore;
            try {
                messageCore = new WfMessageCreator().deserialize(messageSerialized).create();
            } catch (WfCoreException e) {
                throw new WfException("Cannot deserialize message: " + e.getMessage(), WfException.ErrorType.WF_FORMAT_ERROR);
            }
            return new WfMessage(messageCore.type, messageCore.header, messageCore.body);
        }

        /**
         * Creates a new Whiteflag message object from a serialized JSON message
         * @param jsonMessageStr String with the serialized JSON message
         * @return a {@link WfMessage} Whiteflag message
         * @throws WfException if the serialization of the message is invalid
         */
        public static final WfMessage deserializeJson(final String jsonMessageStr) throws WfException {
            // Deserialize JSOM string
            WfJsonMessage jsonMessage;
            try {
                jsonMessage = new WfJsonMessage(jsonMessageStr);
            } catch (JsonException e) {
                throw new WfException("Cannot deserialize JSON message: " + e.getMessage(), WfException.ErrorType.WF_FORMAT_ERROR);
            }
            // Create message core with header and body field name-to-value mappings
            WfMessageCore messageCore;
            try {
                messageCore = new WfMessageCreator().map(jsonMessage.getHeader(), jsonMessage.getBody()).create();
            } catch (WfCoreException e) {
                throw new WfException("Cannot deserialize JSON message: " + e.getMessage(), WfException.ErrorType.WF_FORMAT_ERROR);
            }
            // Create message and add metadata
            WfMessage message = new WfMessage(messageCore.type, messageCore.header, messageCore.body);
            message.setMetadata(jsonMessage.getMetadata());
            return message;
        }

        /**
         * Creates a new Whiteflag message object from an encoded message
         * @param messageEncoded String with hexadecimal representation of the encoded message
         * @return a {@link WfMessage} Whiteflag message
         * @throws WfException if the encoding of the message is invalid
         */
        public static final WfMessage decode(final String messageEncoded) throws WfException {
            WfMessageCore messageCore;
            try {
                messageCore = new WfMessageCreator().decode(messageEncoded).create();
            } catch (WfCoreException e) {
                throw new WfException("Cannot decode message: " + e.getMessage(), WfException.ErrorType.WF_FORMAT_ERROR);
            }
            return new WfMessage(messageCore.type, messageCore.header, messageCore.body);
        }

        /**
         * Creates a new Whiteflag message object from field values
         * @param fieldValues String array with the values for the message fields
         * @return a {@link WfMessage} Whiteflag message
         * @throws WfException if any of the provided values is invalid
         */
        public static final WfMessage compile(final String[] fieldValues) throws WfException {
            WfMessageCore messageCore;
            try {
                messageCore = new WfMessageCreator().compile(fieldValues).create();
            } catch (WfCoreException e) {
                throw new WfException("Cannot compile message: " + e.getMessage(), WfException.ErrorType.WF_FORMAT_ERROR);
            }
            return new WfMessage(messageCore.type, messageCore.header, messageCore.body);
        }
    }
}
