/*
 * Whiteflag Java Library
 */
package org.whiteflagprotocol.java;

import java.util.Set;
import java.util.Map;
import java.util.HashMap;

/* Required Whiteflag core and util classes */
import org.whiteflagprotocol.java.core.*;
import org.whiteflagprotocol.java.util.*;

/* Required error types */
import static org.whiteflagprotocol.java.WfException.ErrorType.WF_FORMAT_ERROR;

/**
 * Whiteflag message class
 * 
 * <p> This is a class representing a Whiteflag message. It contains
 * all methods to handle a Whiteflag message, e.g. to encode, decode, etc. It
 * also provides static factory methods to create Whiteflag messages in
 * different ways from various data.
 * 
 * @wfver v1-draft.6
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
    private WfBinaryBuffer binaryMsg = WfBinaryBuffer.create();
    private String serializedMsg = null;

    /* CONSTRUCTORS */

    /**
     * Creates a Whiteflag message by calling the super constructor
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
     * @return the value of the requested metadata key
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
     * @return the serialized message, i.e. the concatinated string of field values
     * @throws WfException if any of the field does not contain valid data
     */
    @Override
    public String serialize() throws WfException {
        try {
            if (serializedMsg == null) {
                serializedMsg = super.serialize();
            }
        } catch (WfCoreException e) {
            throw new WfException(e.getMessage(), WF_FORMAT_ERROR);
        }
        return serializedMsg;
    }

    /**
     * Returns the cached encoded message, or else it encodes and caches Whiteflag message
     * @return a byte array with the compressed binary encoded message
     * @throws WfException if any field does not contain valid data
     */
    @Override
    public WfBinaryBuffer encode() throws WfException {
        try {
            if (binaryMsg.length() == 0) {
                binaryMsg = super.encode();
            }
        } catch (WfCoreException e) {
            throw new WfException(e.getMessage(), WF_FORMAT_ERROR);
        }
        return binaryMsg;
    }

    /**
     * Returns a byte array with the binary encoded message
     * @return a byte array with the binary encoded message
     * @throws WfException if any field does not contain valid data
     */
    public byte[] toByteArray() throws WfException {
        return encode().toByteArray();
    }

    /**
     * Returns a hexedimal string representation of the binary encoded message
     * @return a hexadecimal string representation of the binary encoded
     * @throws WfException if any field does not contain valid data
     */
    public String toHexString() throws WfException {
        return encode().toHexString();
    }

    /**
     * Returns the serialised JSON representation of the Whiteflag message
     * @return the serialised JSON representation
     */
    public String toJson() throws WfException {
        String jsonMsgStr;
        try {
            jsonMsgStr = new WfJsonMessage(metadata, header.toMap(), body.toMap()).toJson();
        } catch (WfUtilException e) {
            throw new WfException("Cannot serialize message into JSON string: " + e.getMessage(), WF_FORMAT_ERROR);
        }
        return jsonMsgStr;
    }

    /* PRIVATE UTILITY METHODS */

    /**
     * Returns the requested metadata value of the Whiteflag message
     * @return the value of the requested metadata key
     */
    private void setMetadata(final Map<String, String> metadata) {
        metadata.forEach(this.metadata::put);
    }

    /* STATIC FACTORY METHODS */

    /**
     * Creates a new empty Whiteflag message object of the specified type
     * @param messageCode the code indicating the message type to be created
     * @return a new {@link WfMessage} Whiteflag message
     */
    public static WfMessage create(final String messageCode) throws WfException {
        WfMessageCore message;
        try {
            message = new WfMessageCreator().type(WfMessageType.byCode(messageCode)).create();
        } catch (WfCoreException e) {
            throw new WfException("Cannot create new message of type " + messageCode + ": " + e.getMessage(), WF_FORMAT_ERROR);
        }
        return new WfMessage(message.type, message.header, message.body);
    }

    /**
     * Copies a Whiteflag message into new Whiteflag message object, without metadata
     * @param message the message to be copied
     * @return a {@link WfMessage} Whiteflag message
     */
    public static WfMessage copy(final WfMessage message) {
        return new WfMessage(message.type, new WfMessageSegment(message.header), new WfMessageSegment(message.body));
    }

    /**
     * Clones a Whiteflag message into new Whiteflag message object, including metadata
     * @param message the message to be copied
     * @return a {@link WfMessage} Whiteflag message
     */
    public static WfMessage clone(final WfMessage message) {
        WfMessage newMessage = copy(message);
        for (String key : message.getMetadataKeys()) {
            newMessage.addMetadata(key, message.getMetadata(key));
        }
        return newMessage;
    }

    /**
     * Creates a new Whiteflag message object from a serialized message
     * @param serializedMsg the uncompressed serialized message
     * @return a {@link WfMessage} Whiteflag message
     * @throws WfException if the serialization of the message is invalid
     */
    public static WfMessage deserialize(final String serializedMsg) throws WfException {
        WfMessageCore message;
        try {
            message = new WfMessageCreator().deserialize(serializedMsg).create();
        } catch (WfCoreException e) {
            throw new WfException("Cannot deserialize message: " + e.getMessage(), WF_FORMAT_ERROR);
        }
        return new WfMessage(message.type, message.header, message.body);
    }

    /**
     * Creates a new Whiteflag message object from a serialized JSON message
     * @param jsonMessage the serialized JSON message
     * @return a {@link WfMessage} Whiteflag message
     * @throws WfException if the serialization of the message is invalid
     */
    public static WfMessage deserializeJson(final String jsonMessage) throws WfException {
        // Deserialize JSON string
        WfJsonMessage jsonMsg;
        try {
            jsonMsg = WfJsonMessage.create(jsonMessage);
        } catch (WfUtilException e) {
            throw new WfException("Cannot deserialize JSON message: " + e.getMessage(), WF_FORMAT_ERROR);
        }
        // Create message core with header and body fieldname-to-value mappings
        WfMessageCore message;
        try {
            message = new WfMessageCreator().map(jsonMsg.getHeader(), jsonMsg.getBody()).create();
        } catch (WfCoreException e) {
            throw new WfException("Cannot deserialize JSON message: " + e.getMessage(), WF_FORMAT_ERROR);
        }
        // Create message and add metadata
        WfMessage newMessage = new WfMessage(message.type, message.header, message.body);
        newMessage.setMetadata(jsonMsg.getMetadata());
        return newMessage;
    }

    /**
     * Creates a new Whiteflag message object from a hexadecimal string with an encoded message
     * @param hexMessage the hexadecimal string representation of the encoded message
     * @return a {@link WfMessage} Whiteflag message
     * @throws WfException if the encoding of the message is invalid
     */
    public static WfMessage decode(final String hexMessage) throws WfException {
        return decode(WfBinaryBuffer.convertToByteArray(hexMessage));
    }

    /**
     * Creates a new Whiteflag message object from a binary encoded message
     * @param binMessage the binary encoded message
     * @return a {@link WfMessage} Whiteflag message
     * @throws WfException if the encoding of the message is invalid
     */
    public static WfMessage decode(final byte[] binMessage) throws WfException {
        WfMessageCore message;
        try {
            message = new WfMessageCreator().decode(binMessage).create();
        } catch (WfCoreException e) {
            throw new WfException("Cannot decode message: " + e.getMessage(), WF_FORMAT_ERROR);
        }
        return new WfMessage(message.type, message.header, message.body);
    }

    /**
     * Creates a new Whiteflag message object from field values
     * @param fieldValues String array with the values for the message fields
     * @return a {@link WfMessage} Whiteflag message
     * @throws WfException if any of the provided values is invalid
     */
    public static WfMessage compile(final String[] fieldValues) throws WfException {
        WfMessageCore message;
        try {
            message = new WfMessageCreator().compile(fieldValues).create();
        } catch (WfCoreException e) {
            throw new WfException("Cannot compile message: " + e.getMessage(), WF_FORMAT_ERROR);
        }
        return new WfMessage(message.type, message.header, message.body);
    }
}
