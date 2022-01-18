/*
 * Whiteflag Java Library
 */
package org.whiteflagprotocol.java;

import java.util.Set;
import java.util.Map;
import java.util.HashMap;

/* Required Whiteflag core and util classes */
import org.whiteflagprotocol.java.core.WfBinaryBuffer;
import org.whiteflagprotocol.java.core.WfCoreException;
import org.whiteflagprotocol.java.core.WfMessageCore;
import org.whiteflagprotocol.java.core.WfMessageCreator;
import org.whiteflagprotocol.java.core.WfMessageType;
import org.whiteflagprotocol.java.util.WfJsonMessage;
import org.whiteflagprotocol.java.util.WfUtilException;

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
 * 
 * @since 1.0
 */
public class WfMessage extends WfMessageCore {

    /* PROPERTIES */

    /* Constants */
    private final String ADDRESSKEY = "originatorAddress";

    /**
     * Implementation specific message metadata
     */
    private Map<String, String> metadata = new HashMap<>();
    /**
     * The blockchain address that will transmit or has transmitted this message
     */
    private WfBlockchainAddress address;
    /**
     * The binary encoded message
     */
    private WfBinaryBuffer encodedMsg = WfBinaryBuffer.create();
    /**
     * The serialized message cache
     */
    private String cachedSerializedMsg = null;

    /* CONSTRUCTORS */

    /**
     * Creates a Whiteflag message from a core message by calling the super constructor
     * @since 1.1
     * @param coreMsg the {@link WfMessageCore} core message
     */
    private WfMessage(final WfMessageCore coreMsg) {
        super(coreMsg);
    }

    /**
     * Creates a Whiteflag message from a decoded core message by calling the super constructor
     * @since 1.1
     * @param coreMsg the {@link WfMessageCore} core message
     * @param encodedMsg the {@link WfBinaryBuffer} with the source binary encoded message to be preserved
     */
    private WfMessage(final WfMessageCore coreMsg, final WfBinaryBuffer encodedMsg) {
        super(coreMsg);
        this.encodedMsg = encodedMsg.markComplete();
    }

    /**
     * Creates a Whiteflag message from a deserialized core message by calling the super constructor
     * @since 1.1
     * @param coreMsg the {@link WfMessageCore} core message
     * @param serializedMsg the source serialized message to be preserved
     */
    private WfMessage(final WfMessageCore coreMsg, final String serializedMsg) {
        super(coreMsg);
        this.cachedSerializedMsg = serializedMsg;
    }

    /* STATIC FACTORY METHODS */

    /**
     * Creates a new empty Whiteflag message object of the specified type
     * @param messageCode the code indicating the message type to be created
     * @return a new {@link WfMessage} Whiteflag message
     * @throws WfException if the message cannot be created
     */
    public static WfMessage create(final String messageCode) throws WfException {
        WfMessageCore coreMsg;
        try {
            coreMsg = new WfMessageCreator().type(WfMessageType.fromCode(messageCode)).create();
        } catch (WfCoreException e) {
            throw new WfException("Cannot create new message of type " + messageCode + ": " + e.getMessage(), WF_FORMAT_ERROR);
        }
        return new WfMessage(coreMsg);
    }

    /**
     * Copies a Whiteflag message into new Whiteflag message object, without metadata
     * @param message the message to be copied
     * @return a {@link WfMessage} Whiteflag message
     */
    public static WfMessage copy(final WfMessage message) {
        return new WfMessage(message);
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
     * @since 1.1
     * @param serializedMsg the uncompressed serialized message
     * @return a {@link WfMessage} Whiteflag message
     * @throws WfException if the serialization of the message is invalid
     */
    public static WfMessage deserialize(final String serializedMsg) throws WfException {
        WfMessageCore coreMsg;
        try {
            coreMsg = new WfMessageCreator().deserialize(serializedMsg).create();
        } catch (WfCoreException e) {
            throw new WfException("Cannot deserialize message: " + e.getMessage(), WF_FORMAT_ERROR);
        }
        return new WfMessage(coreMsg, serializedMsg);
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
        WfMessageCore coreMsg;
        try {
            coreMsg = new WfMessageCreator().map(jsonMsg.getHeader(), jsonMsg.getBody()).create();
        } catch (WfCoreException e) {
            throw new WfException("Cannot deserialize JSON message: " + e.getMessage(), WF_FORMAT_ERROR);
        }
        // Create message and add metadata
        WfMessage message = new WfMessage(coreMsg);
        message.setMetadata(jsonMsg.getMetadata());
        return message;
    }

    /**
     * Creates a new Whiteflag message from a hexadecimal string represaentation of an encoded message
     * @since 1.1
     * @param hexMessage the hexadecimal string representation of the encoded message
     * @return a new {@link WfMessage} Whiteflag message
     * @throws WfException if the message cannot be decoded
     */
    public static WfMessage decode(final String hexMessage) throws WfException {
        return decode(WfBinaryBuffer.fromHexString(hexMessage));
    }

    /**
     * Creates a new Whiteflag message from a byte array with an binary encoded message
     * @since 1.1
     * @param binMessage the byte array with the binary encoded message
     * @return a new {@link WfMessage} Whiteflag message
     * @throws WfException if the message cannot be decoded
     */
    public static WfMessage decode(final byte[] binMessage) throws WfException {
        return decode(WfBinaryBuffer.fromByteArray(binMessage));
    }

    /**
     * Creates a new Whiteflag message from a binary buffer
     * @since 1.1
     * @param encodedMsg the binary buffer with the encoded message
     * @return a new {@link WfMessage} Whiteflag message
     * @throws WfException if the message cannot be decoded
     */
    public static WfMessage decode(final WfBinaryBuffer encodedMsg) throws WfException {
        WfMessageCore coreMsg;
        try {
            coreMsg = new WfMessageCreator().decode(encodedMsg).create();
        } catch (WfCoreException e) {
            throw new WfException("Cannot decode message: " + e.getMessage(), WF_FORMAT_ERROR);
        }
        return new WfMessage(coreMsg, encodedMsg);
    }

    /**
     * Creates a new Whiteflag message object from field values
     * @param fieldValues String array with the values for the message fields
     * @return a {@link WfMessage} Whiteflag message
     * @throws WfException if any of the provided values is invalid
     */
    public static WfMessage compile(final String[] fieldValues) throws WfException {
        WfMessageCore coreMsg;
        try {
            coreMsg = new WfMessageCreator().compile(fieldValues).create();
        } catch (WfCoreException e) {
            throw new WfException("Cannot compile message: " + e.getMessage(), WF_FORMAT_ERROR);
        }
        return new WfMessage(coreMsg);
    }

    /* PUBLIC METHODS */

    /**
     * Adds metadata to the Whiteflag message if not already existing
     * @param key a string with the metadata key
     * @param value a string with the metadata value
     * @return null if successful, otherwise the value of the already existing key
     */
    public String addMetadata(final String key, final String value) {
        return metadata.putIfAbsent(key, value);
    }

    /**
     * Returns the requested metadata value of the Whiteflag message
     * @param key a string with the metadata key
     * @return the value associated with the requested metadata key
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

    /**
     * Sets the blockchain address used to send this message and adds it to the metadata
     * @param address the {@link WfBlockchainAddress} used to send this message
     * @return null if address newly added to metadata, otherwise the existing value that was replaced
     */
    public String setAddress(WfBlockchainAccount account) {
        this.address = account;
        return metadata.put(ADDRESSKEY, address.getAddressString());
    }

    /**
     * Gets the blockchain address used to send this message
     * @param address the {@link WfBlockchainAddress} used to send this message
     */
    public WfBlockchainAddress getAddress() {
        return this.address;
    }

    /**
     * Returns the cached serialized message, or else it serialzes and caches Whiteflag message
     * @return the serialized message, i.e. the concatinated string of field values
     * @throws WfException if any of the field does not contain valid data
     */
    @Override
    public String serialize() throws WfException {
        if (this.cachedSerializedMsg == null) {
            try {
                this.cachedSerializedMsg = super.serialize();
            } catch (WfCoreException e) {
                throw new WfException(e.getMessage(), WF_FORMAT_ERROR);
            }
        }
        return this.cachedSerializedMsg;
    }

    /**
     * Returns the cached encoded message, or else it encodes and caches Whiteflag message
     * @since 1.1
     * @return a byte array with the compressed binary encoded message
     * @throws WfException if any field does not contain valid data
     */
    @Override
    public WfBinaryBuffer encode() throws WfException {
        if (Boolean.FALSE.equals(encodedMsg.isComplete())) {
            try {
                this.encodedMsg = super.encode().markComplete();
            } catch (WfCoreException e) {
                throw new WfException(e.getMessage(), WF_FORMAT_ERROR);
            }
        }
        return this.encodedMsg;
    }

    /**
     * Returns a byte array with the binary encoded message
     * @since 1.1
     * @return a byte array with the binary encoded message
     * @throws WfException if any field does not contain valid data
     */
    public byte[] toByteArray() throws WfException {
        return this.encode().toByteArray();
    }

    /**
     * Returns a hexedimal string representation of the binary encoded message
     * @since 1.1
     * @return a hexadecimal string representation of the binary encoded
     * @throws WfException if any field does not contain valid data
     */
    public String toHexString() throws WfException {
        return this.encode().toHexString();
    }

    /**
     * Returns the serialised JSON representation of the Whiteflag message
     * @return the serialised JSON representation
     * @throws WfException if the message cannot be serialised
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

    /* PRIVATE METHODS */

    /**
     * Returns the requested metadata value of the Whiteflag message
     * @return the value of the requested metadata key
     */
    private void setMetadata(final Map<String, String> metadata) {
        metadata.forEach(this.metadata::put);
    }
}
