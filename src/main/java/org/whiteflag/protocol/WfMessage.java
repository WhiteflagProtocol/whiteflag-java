/*
 * Whiteflag Java Library
 */
package org.whiteflag.protocol;

import java.util.HashMap;
import org.whiteflag.protocol.core.*;

/**
 * Whiteflag message class
 * 
 * </p> This is a class representing a Whiteflag message. It contains
 * all methods to handle a Whiteflag message, e.g. to encode, decode, etc.
 */
public class WfMessage extends WfMessageCore {

    /* PROPERTIES */

    /**
     * Contains implementation specific message metadata
     */
    private HashMap<String, String> metadata = new HashMap<>();

    /**
     * Contains the cached encoded message
     */
    private String encodedMessage;

    /* CONSTRUCTORS */

    /**
     * Creates a Whiteflag message by calling the super constructor with an array of field values
     * @param header the {@link WfMessageSegment} message header
     * @param body the {@link WfMessageSegment} message body
     */
    public WfMessage(WfMessageSegment header, WfMessageSegment body) {
        super(header, body);
    }

    /* PUBLIC METHODS: Message interface */

    /**
     * Returns the cached encoded message, or else it encodes and caches Whiteflag message without 0x prefix
     * @return hexadecimal string with the encoded Whiteflag message
     * @throws WfCoreException if any field does not contain valid data
     */
    @Override
    public String encode() throws WfException {
        return encode(false);
    }

    /**
     * Returns the cached encoded message, or else it encodes and caches Whiteflag message
     * @param prefix if TRUE, the resulting string gets a 0x prefix (or whatever has been cached)
     * @return hexadecimal string with the encoded Whiteflag message
     * @throws WfCoreException if any field does not contain valid data
     */
    @Override
    public String encode(Boolean prefix) throws WfException {
        try {
            if (encodedMessage == null) {
                encodedMessage = super.encode(prefix);
            }
        } catch (WfCoreException e) {
            throw new WfException(e.getMessage(), WfException.ErrorType.WF_FORMAT_ERROR);
        }
        return encodedMessage;
    }

    /* PUBLIC METHODS: Metadata */

    /**
     * Adds metadata to the Whiteflag message if not already existing
     * @return null if successful, otherwise the value of the already existing key
     */
    public String addMetadata(String key, String value) {
        return metadata.putIfAbsent(key, value);
    }

    /**
     * Returns the requested metadata value of the Whiteflag message
     * @return a string with the value of the requested metadata key
     */
    public String getMetadata(String key) {
        return metadata.get(key);
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

        /* METHODS */

        /**
         * Creates a Whiteflag message object from a serialised message
         * @param serializedMessage String with the uncompressed serialized message
         * @return a {@link WfMessage} Whiteflag message
         * @throws WfException if the provided values are invalid
         */
        public final WfMessage deserialize(String serializedMessage) throws WfException {
            WfMessageCore message;
            try {
                message = new WfMessageCreator().deserialize(serializedMessage);
            } catch (WfCoreException e) {
                throw new WfException(e.getMessage(), WfException.ErrorType.WF_FORMAT_ERROR);
            }
            return new WfMessage(message.header, message.body);
        }

        /**
         * Creates a Whiteflag message object from field values
         * @param fieldValues String array with the values for the message fields
         * @return a {@link WfMessage} Whiteflag message
         * @throws WfCoreException if the provided values are invalid
         */
        public final WfMessage createFromValues(String[] fieldValues) throws WfException {
            WfMessageCore message;
            try {
                message = new WfMessageCreator().createFromValues(fieldValues);
            } catch (WfCoreException e) {
                throw new WfException(e.getMessage(), WfException.ErrorType.WF_FORMAT_ERROR);
            }
            return new WfMessage(message.header, message.body);
        }
    }
}
