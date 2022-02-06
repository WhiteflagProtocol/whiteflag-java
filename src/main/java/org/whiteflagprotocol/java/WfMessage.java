/*
 * Whiteflag Java Library
 */
package org.whiteflagprotocol.java;

import java.util.Set;
import java.util.Map;
import java.util.Arrays;
import java.util.HashMap;
import java.security.interfaces.ECPublicKey;

import javax.security.auth.DestroyFailedException;

/* Required Whiteflag core and util classes */
import org.whiteflagprotocol.java.core.WfBinaryBuffer;
import org.whiteflagprotocol.java.core.WfCoreException;
import org.whiteflagprotocol.java.core.WfBasicMessage;
import org.whiteflagprotocol.java.core.WfMessageCreator;
import org.whiteflagprotocol.java.core.WfMessageSegment;
import org.whiteflagprotocol.java.core.WfMessageType;
import org.whiteflagprotocol.java.crypto.WfCipher;
import org.whiteflagprotocol.java.crypto.WfEncryptionKey;
import org.whiteflagprotocol.java.crypto.WfEncryptionMethod;
import org.whiteflagprotocol.java.crypto.WfCryptoException;
import org.whiteflagprotocol.java.crypto.WfCryptoUtil;
import org.whiteflagprotocol.java.crypto.WfECDHKeyPair;
import org.whiteflagprotocol.java.util.WfJsonMessage;
import org.whiteflagprotocol.java.util.WfUtilException;

/* Required error types */
import static org.whiteflagprotocol.java.WfException.ErrorType.WF_METADATA_ERROR;
import static org.whiteflagprotocol.java.WfException.ErrorType.WF_FORMAT_ERROR;
import static org.whiteflagprotocol.java.WfException.ErrorType.WF_CRYPTO_ERROR;

/**
 * Whiteflag message class
 * 
 * <p> This is a class representing a Whiteflag message. It contains all
 * methods to handle a Whiteflag message, e.g. to encode, decode, etc. It also
 * provides static factory methods to create Whiteflag messages in different
 * ways from various data.
 * 
 * A Whiteflag message is put on a blockchain by embedding it in a transaction
 * in encoded, and possibly also encrypted, form. Information from the message
 * originmator and recipient (if any) is provided through the {@link WfAccount}
 * interface.
 * 
 * @wfver v1-draft.6
 * 
 * @since 1.1
 */
public class WfMessage {

    /* PROPERTIES */

    /* Constants */
    private static final String METAKEY_ORIGINATOR = "originatorAddress";
    private static final String METAKEY_RECIPIENT = "recipientAddress";
    private static final String FIELD_ENCRYPTIONINDICATOR = "EncryptionIndicator";

    /* Base message */
    private final WfBasicMessage base;

    /* Metadata */
    private Map<String, String> metadata = new HashMap<>();

    /* Originator and Recipient */
    private WfAccount originator;
    private WfAccount recipient;

    /* Encryption */
    private byte[] initVector = new byte[0];

    /* Cached messge representations */
    private WfBinaryBuffer cachedMsg = WfBinaryBuffer.create();
    private String cachedMsgStr;
    
    /* CONSTRUCTORS */

    /**
     * Creates a Whiteflag message from a base message
     * @since 1.1
     * @param base the base message
     */
    private WfMessage(final WfBasicMessage base) {
        this.base = base;
    }

    /**
     * Creates a Whiteflag message from a decoded base message
     private String cachedMsgStr = null;
     * @since 1.1
     * @param base the base message
     * @param encodedMsg the buffer with the binary encoded source of the message to be preserved
     */
    private WfMessage(final WfBasicMessage base, final WfBinaryBuffer encodedMsg) {
        this.base = base;
        this.cachedMsg = encodedMsg.markComplete();
    }

    /**
     * Creates a Whiteflag message from a deserialized base message
     * @since 1.1
     * @param base the base message
     * @param serializedMsg the source serialized message to be preserved
     */
    private WfMessage(final WfBasicMessage base, final String serializedMsg) {
        this.base = base;
        this.cachedMsgStr = serializedMsg;
    }

    /* STATIC FACTORY METHODS */

    /**
     * Creates a new empty Whiteflag message object of the specified type
     * @param messageCode a string with the code indicating the message type to be created
     * @return a new Whiteflag message
     * @throws WfException if the message cannot be created
     */
    public static final WfMessage create(final String messageCode) throws WfException {
        WfBasicMessage base;
        try {
            base = new WfMessageCreator().type(WfMessageType.fromCode(messageCode)).create();
        } catch (WfCoreException e) {
            throw new WfException("Cannot create new message of type " + messageCode, e, WF_FORMAT_ERROR);
        }
        return new WfMessage(base);
    }

    /**
     * Creates a new Whiteflag message object from a serialized message
     * @since 1.1
     * @param serializedMsg the uncompressed serialized message
     * @return a new Whiteflag message
     * @throws WfException if the serialization of the message is invalid
     */
    public static final WfMessage deserialize(final String serializedMsg) throws WfException {
        WfBasicMessage base;
        try {
            base = new WfMessageCreator().deserialize(serializedMsg).create();
        } catch (WfCoreException e) {
            throw new WfException("Cannot deserialize message: " + e.getMessage(), e, WF_FORMAT_ERROR);
        }
        return new WfMessage(base, serializedMsg);
    }

    /**
     * Creates a new Whiteflag message object from a serialized JSON message
     * @param jsonMessage the serialized JSON message
     * @return a new Whiteflag message
     * @throws WfException if the serialization of the message is invalid
     */
    public static final WfMessage deserializeJson(final String jsonMessage) throws WfException {
        /* Deserialize JSON string */
        WfJsonMessage jsonMsg;
        try {
            jsonMsg = WfJsonMessage.create(jsonMessage);
        } catch (WfUtilException e) {
            throw new WfException("Cannot deserialize JSON message: " + e.getMessage(), e, WF_FORMAT_ERROR);
        }
        /* Create message base with header and body fieldname-to-value mappings */
        WfBasicMessage base;
        try {
            base = new WfMessageCreator().map(jsonMsg.getHeader(), jsonMsg.getBody()).create();
        } catch (WfCoreException e) {
            throw new WfException("Cannot deserialize JSON message: " + e.getMessage(), e, WF_FORMAT_ERROR);
        }
        /* Create message and add metadata */
        WfMessage message = new WfMessage(base);
        message.setMetadata(jsonMsg.getMetadata());
        return message;
    }

    /**
     * Creates a new Whiteflag message from a hexadecimal string represaentation of an encoded message
     * @since 1.1
     * @param hexMessage a hexadecimal string representation of the encoded message
     * @return a new Whiteflag message
     * @throws WfException if the message cannot be decoded
     */
    public static final WfMessage decode(final String hexMessage) throws WfException {
        return decode(WfBinaryBuffer.fromHexString(hexMessage));
    }

    /**
     * Creates a new Whiteflag message from a byte array with an binary encoded message
     * @since 1.1
     * @param binMessage a byte array with the binary encoded message
     * @return a new Whiteflag message
     * @throws WfException if the message cannot be decoded
     */
    public static final WfMessage decode(final byte[] binMessage) throws WfException {
        return decode(WfBinaryBuffer.fromByteArray(binMessage));
    }

    /**
     * Creates a new Whiteflag message from a binary buffer
     * @since 1.1
     * @param encodedMsg a binary buffer with the encoded message
     * @return a new Whiteflag message
     * @throws WfException if the message cannot be decoded
     */
    public static final WfMessage decode(final WfBinaryBuffer encodedMsg) throws WfException {
        WfBasicMessage base;
        try {
            base = new WfMessageCreator().decode(encodedMsg).create();
        } catch (WfCoreException e) {
            throw new WfException("Cannot decode message: " + e.getMessage(), e, WF_FORMAT_ERROR);
        }
        return new WfMessage(base, encodedMsg);
    }

    /**
     * Creates a new Whiteflag message from an encyrpted binary buffer and metadata
     * @since 1.1
     * @param encryptedMsg a hexadecimal string with the encrypted message
     * @param originator the originator of the message
     * @param recipient the intended recipient of the message
     * @param initVector the initialisation vector used to encrypt the message
     * @return a new Whiteflag message
     * @throws WfException if the message cannot be decrypted or decoded
     */
    public static final WfMessage decrypt(final String encryptedMsg, final WfAccount originator, final WfAccount recipient, final String initVector) throws WfException {
        return decrypt(WfBinaryBuffer.fromHexString(encryptedMsg), originator, recipient, WfBinaryBuffer.convertToByteArray(initVector));
    }


    /**
     * Creates a new Whiteflag message from an encyrpted binary buffer and metadata
     * @since 1.1
     * @param encryptedMsg a hexadecimal string with with the encrypted message
     * @param originator the originator of the message
     * @param recipient the intended recipient of the message
     * @param initVector a byte array with initialisation vector used to encrypt the message
     * @return a new Whiteflag message
     * @throws WfException if the message cannot be decrypted or decoded
     */
    public static final WfMessage decrypt(final byte[] encryptedMsg, final WfAccount originator, final WfAccount recipient, final byte[] initVector) throws WfException {
        return decrypt(WfBinaryBuffer.fromByteArray(encryptedMsg), originator, recipient, initVector);
    }

    /**
     * Creates a new Whiteflag message from an encyrpted binary buffer and metadata
     * @since 1.1
     * @param encryptedMsg a binary buffer with the encrypted message
     * @param originator the originator of the message
     * @param recipient the intended recipient of the message
     * @param initVector a byte array with the initialisation vector used to encrypt the message
     * @return a new Whiteflag message
     * @throws WfException if the message cannot be decrypted or decoded
     */
    public static final WfMessage decrypt(final WfBinaryBuffer encryptedMsg, final WfAccount originator, final WfAccount recipient, final byte[] initVector) throws WfException {
        /* Get the unencrypted header */
        WfMessageSegment header;
        WfMessageCreator creator = new WfMessageCreator();
        try {
            header = creator.getUnencryptedHeader(encryptedMsg);
        } catch (WfCoreException e) {
            throw new WfException("Cannot decode unencrypted message header: " + e.getMessage(), e, WF_FORMAT_ERROR);
        }
        /* Decrypt message */
        WfBinaryBuffer encodedMsg = decrypt(encryptedMsg, header, originator, recipient, initVector);

        /* Decode the decrypted message */
        WfBasicMessage base;
        try {
            base = creator.decode(encodedMsg).create();
        } catch (WfCoreException e) {
            throw new WfException("Cannot decode message: " + e.getMessage(), e, WF_FORMAT_ERROR);
        }
        /* Create a new message and pass meta information */
        WfMessage message = new WfMessage(base, encryptedMsg);
        message.setOriginator(originator);
        message.setRecipient(recipient);
        message.setInitVector(initVector);
        return message;
    }

    /**
     * Creates a new Whiteflag message object from field values
     * @param fieldValues a string array with the values for the message fields
     * @return a new Whiteflag message
     * @throws WfException if any of the provided values is invalid
     */
    public static final WfMessage compile(final String[] fieldValues) throws WfException {
        WfBasicMessage base;
        try {
            base = new WfMessageCreator().compile(fieldValues).create();
        } catch (WfCoreException e) {
            throw new WfException("Cannot compile message: " + e.getMessage(), e, WF_FORMAT_ERROR);
        }
        return new WfMessage(base);
    }

    /* PUBLIC METHODS */

    /**
     * Gets the type of this message
     * @return the message type
     */
    public WfMessageType getType() {
        return base.type;
    }

    /**
     * Checks if this message contains valid data
     * @return TRUE if all message fields contain valid data, else FALSE
     */
    public boolean isValid() {
        return base.isValid();
    }

    /**
     * Checks if the specified message field contains valid data
     * @param fieldname the name of the field
     * @return TRUE if the field contains valid data, else FALSE
     */
    public final boolean isValid(final String fieldname) {
        return base.isValid(fieldname);
    }

    /**
     * Checks if the provided data is valid for the specified message field
     * @param fieldname the name of the field
     * @param data the value to be checked
     * @return TRUE if the field contains valid data, else FALSE
     */
    public final boolean isValid(final String fieldname, final String data) {
        return base.isValid(fieldname, data);
    }

    /**
     * Gets the number of fields in this message
     * @return the number of message fields
     */
    public final int getNoFields() {
        return base.getNoFields();
    }

    /**
     * Gets the field names of this message
     * @return a string set with all field names
     */
    public Set<String> getFieldNames() {
        return base.getFieldNames();
    }

    /**
     * Gets the value of the specified field
     * @param fieldname the name of the requested field
     * @return the field value, or NULL if field does not exist
     */
    public String get(final String fieldname) {
        return base.get(fieldname);
    }

    /**
     * Sets the value of the specified field
     * @param fieldname the name of the field
     * @param data data to be set as the field value
     * @return TRUE if field value is set, FALSE if field does not exits, isalready set, or data is invalid
     */
    public boolean set(final String fieldname, final String data) {
        return base.set(fieldname, data);
    }

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
     * Copies the base of this message into a new message, without any metadata
     * @since 1.1
     * @return a base copy of this Whiteflag message
     */
    public final WfMessage copy() {
        return new WfMessage(this.base);
    }

    /**
     * Sets the originator sending this message and adds its blockchain address to the metadata
     * @since 1.1
     * @param originator the originator information
     * @return null if address newly added to metadata, otherwise the existing value that was replaced
     */
    public final String setOriginator(WfAccount originator) {
        this.originator = originator;
        return metadata.put(METAKEY_ORIGINATOR, originator.getAddress());
    }

    /**
     * Gets the originator of this message by their account
     * @since 1.1
     * @return the account of the originator
     */
    public final WfAccount getOriginator() {
        return this.originator;
    }

    /**
     * Sets the intended recipient of this message (if any) and adds its blockchain address to the metadata
     * @since 1.1
     * @param recipient the account of the recipient of this message
     * @return null if address newly added to metadata, otherwise the existing value that was replaced
     */
    public final String setRecipient(WfAccount recipient) {
        this.recipient = recipient;
        return metadata.put(METAKEY_RECIPIENT, recipient.getAddress());
    }

    /**
     * Gets the recipient of this message by theri account
     * @since 1.1
     * @return the account of the recipient
     */
    public final WfAccount getRecipient() {
        return this.recipient;
    }

    /**
     * Sets the non-secret initialisation vector used for encryption and decryption, if not already set
     * @since 1.1
     * @param initVector a hexadecimal string with the initialisation vector
     * @return a byte array with the initialisation vector; if already set this is the existing initialisation vector
     */
    public final byte[] setInitVector(final String initVector) {
        if (this.initVector.length == 0) {
            this.initVector = WfCryptoUtil.convertToByteArray(initVector);
        }
        return this.initVector;
    }

    /**
     * Sets the non-secret initialisation vector used for encryption and decryption, if not already set
     * @since 1.1
     * @param initVector a byte array with the initialisation vector
     * @return a byte array with the initialisation vector; if already set this is the existing initialisation vector
     */
    public final byte[] setInitVector(final byte[] initVector) {
        if (this.initVector.length == 0) {
            this.initVector = Arrays.copyOf(initVector, initVector.length);
        }
        return this.initVector;
    }

    /**
     * Gets the non-secret initialisation vector used for encryption and decryption
     * @since 1.1
     * @return a byte array with the initialisation vector
     */
    public final byte[] getInitVector() {
        return Arrays.copyOf(this.initVector, this.initVector.length);
    }

    /* PUBLIC METHODS: Transformations */

    /**
     * Returns the message as a concatinated string of field values
     * @return the serialized message
     */
    @Override
    public String toString() {
        return base.toString(); 
    }

    /**
     * Returns the cached serialized message, or else it serialzes and caches Whiteflag message
     * @return the serialized message, i.e. the concatinated string of field values
     * @throws WfException if any of the field does not contain valid data
     */
    public final String serialize() throws WfException {
        if (this.cachedMsgStr == null) {
            try {
                this.cachedMsgStr = base.serialize();
            } catch (WfCoreException e) {
                throw new WfException("Could not serialize message: " + e.getMessage(), e, WF_FORMAT_ERROR);
            }
        }
        return this.cachedMsgStr;
    }

    /**
     * Returns the cached encoded/encrypted message, or else it encodes/encrypts and caches the message
     * @since 1.1
     * @return a binary buffer with the encoded/endypted message
     * @throws WfException if any field does not contain valid data
     */
    public final WfBinaryBuffer encode() throws WfException {
        /* Return encoded/encrypted message if already cached */
        if (Boolean.TRUE.equals(cachedMsg.isComplete())) return this.cachedMsg.copy();

        /* Encode and encrypt message */
        WfBinaryBuffer encodedMsg;
        try {
            encodedMsg = base.encode();
        } catch (WfCoreException e) {
            throw new WfException("Could not encode message: " + e.getMessage(), e, WF_FORMAT_ERROR);
        }
        /* Encrypt, cache and return the result */
        this.cachedMsg = encrypt(encodedMsg).markComplete();
        return this.cachedMsg.copy();
    }

    /**
     * Returns the encoded/encrypted message; identical to encode()
     * @since 1.1
     * @return a byte array with the compressed binary encoded message
     * @throws WfException if any field does not contain valid data
     */
    public final WfBinaryBuffer encrypt() throws WfException {
        return this.encode();
    }

    /**
     * Returns a byte array with the binary encoded message
     * @since 1.1
     * @return a byte array with the binary encoded message
     * @throws WfException if any field does not contain valid data
     */
    public final byte[] toByteArray() throws WfException {
        return this.encode().toByteArray();
    }

    /**
     * Returns a hexadimal string representation of the binary encoded message
     * @since 1.1
     * @return a hexadecimal string representation of the binary encoded
     * @throws WfException if any field does not contain valid data
     */
    public final String toHexString() throws WfException {
        return this.encode().toHexString();
    }

    /**
     * Returns the serialised JSON representation of the Whiteflag message
     * @return the serialised JSON representation
     * @throws WfException if the message cannot be serialised
     */
    public final String toJson() throws WfException {
        String jsonMsgStr;
        try {
            jsonMsgStr = new WfJsonMessage(metadata, base.header.toMap(), base.body.toMap()).toJson();
        } catch (WfUtilException e) {
            throw new WfException("Could not serialize message into JSON string: " + e.getMessage(), e, WF_FORMAT_ERROR);
        }
        return jsonMsgStr;
    }

    /* PROTECTED METHODS */

    /**
     * Sets the metadata
     * @param metadata the key-to-value mapping of the metadata
     */
    protected final void setMetadata(final Map<String, String> metadata) {
        metadata.forEach(this.metadata::put);
    }

    /* PRIVATE METHODS */

    /**
     * Encrypts the message
     * @since 1.1
     * @param encodedMsg a binary buffer with the encoded message to be encrypted
     * @return a binary buffer with the encrypted message
     * @throws WfException if message could not be encrypted
     * @throws IllegalStateException if the originator or recipient of this message are unknown
     */
    private final WfBinaryBuffer encrypt(WfBinaryBuffer encodedMsg) throws WfException {
        /* Determine encryption method */
        WfEncryptionMethod method = getEncryptionMethod(base.header.get(FIELD_ENCRYPTIONINDICATOR));
        if (method == WfEncryptionMethod.NO_ENCRYPTION) return encodedMsg;

        /* Check originator and recipient */
        if (recipient == null) throw new IllegalStateException("Cannot determine encryption key if recipient is unknown");
        if (originator == null) throw new IllegalStateException("Cannot set context if originator is unknown");

        /* Get key and initialize cipher */
        WfCipher cipher = createCipher(method, originator, recipient);
        if (this.initVector.length == 0) {
            try {
                this.initVector = setInitVector(cipher.setInitVector());
            } catch (WfCryptoException e) {
                throw new WfException("Could not create random initialisation vector: " + e.getMessage(), e, WF_CRYPTO_ERROR);
            }
        } else {
            cipher.setInitVector(this.initVector);
        }
        /* Encryption message, except first fields */
        final int unencryptedBitPosition = base.header.bitLength(FIELD_ENCRYPTIONINDICATOR);
        WfBinaryBuffer encryptedMsg = WfBinaryBuffer.create();
        try {
            encryptedMsg.appendBits(encodedMsg.extractBits(0, unencryptedBitPosition));
            encryptedMsg.appendBits(cipher.encrypt(encodedMsg.extractBits(unencryptedBitPosition)));
            cipher.destroy();
        } catch (WfCryptoException e) {
            throw new WfException("Could not encrypt message: " + e.getMessage(), e, WF_CRYPTO_ERROR);
        } catch (DestroyFailedException e) {
            throw new WfException("Could not destroy the cipher: " + e.getMessage(), e, WF_CRYPTO_ERROR);
        }
        return encryptedMsg;
    }

    /* PRIVATE STATIC METHODS */

    /**
     * Decrypts an encrypted message
     * @param encryptedMsg a binary buffer with the encrypted message
     * @param header the unencrypted header fields
     * @param originator the message originator
     * @param recipient the message recipient
     * @param initVector the initialisation vector
     * @return a binary buffer with the decrypted messge
     * @throws WfException if message cannot be decrypted
     */
    private static final WfBinaryBuffer decrypt(WfBinaryBuffer encryptedMsg, WfMessageSegment header, WfAccount originator, WfAccount recipient, byte[] initVector) throws WfException {
        /* Check encryption method */
        WfEncryptionMethod method = getEncryptionMethod(header.get(FIELD_ENCRYPTIONINDICATOR));
        if (method == WfEncryptionMethod.NO_ENCRYPTION) return encryptedMsg;

        /* Create and initialize cipher */
        WfCipher cipher = createCipher(method, originator, recipient);
        cipher.setInitVector(initVector);

        /* Decrypt message, except first fields */
        final int unencryptedBitPosition = header.bitLength(FIELD_ENCRYPTIONINDICATOR);
        WfBinaryBuffer encodedMsg = WfBinaryBuffer.create();
        try {
            encodedMsg.appendBits(encryptedMsg.extractBits(0, unencryptedBitPosition));
            encodedMsg.appendBits(cipher.decrypt(encryptedMsg.extractBits(unencryptedBitPosition)));
            cipher.destroy();
        } catch (WfCryptoException e) {
            throw new WfException("Could not decrypt message: " + e.getMessage(), e, WF_CRYPTO_ERROR);
        } catch (DestroyFailedException e) {
            throw new WfException("Could not destroy the cipher: " + e.getMessage(), e, WF_CRYPTO_ERROR);
        }
        return encodedMsg;
    }

    /**
     * Sets the encryption method based on message field
     * @since 1.1
     * @param encryptionIndicator the value of the EncryptionIndicator field
     * @throws WfException if the encryption method of the message is invalid
     */
    private static final WfEncryptionMethod getEncryptionMethod(final String encryptionIndicator) throws WfException {
        if (encryptionIndicator == null || encryptionIndicator.equals("")) {
            throw new WfException("The " + FIELD_ENCRYPTIONINDICATOR + " message field does not exist or is not set", null, WF_FORMAT_ERROR);
        }
        try {
            return WfEncryptionMethod.fromFieldValue(encryptionIndicator);
        } catch (WfCryptoException e) {
            throw new WfException("No valid encryption method associated with the " + FIELD_ENCRYPTIONINDICATOR + " message field value: " + encryptionIndicator, e, WF_FORMAT_ERROR);
        }
    }

    /**
     * Initializes the cryptographic cipher
     * @since 1.1
     * @param method the Whiteflag encryption method
     * @param originator the originator of the message
     * @param recipient the recipient of the message
     * @return the initialized cipher
     * @throws WfException if the cipher cannot be initialized
     * @throws IllegalStateException if the originator of this message is unknown
     */
    private static final WfCipher createCipher(WfEncryptionMethod method, WfAccount originator, WfAccount recipient) throws WfException {
        /* Get encryption key and create cipher */
        WfCipher cipher;
        try {
            WfEncryptionKey key = getEncryptionKey(method, originator, recipient);
            cipher = WfCipher.fromKey(key);
        } catch (WfCryptoException e) {
            throw new WfException("Could not initialize cipher to encrypt message: " + e.getMessage(), e, WF_CRYPTO_ERROR);
        }
        /* Bind cipher to context */
        byte[] address = originator.getBinaryAddress();
        if (address.length == 0) {
            throw new WfException("No originator blockchain address available", null, WF_METADATA_ERROR);
        }
        return cipher.setContext(address);
    }

    /**
     * Retrieves the encryption key
     * @since 1.1
     * @param method the Whiteflag encryption method
     * @param originator the originator of the message
     * @param recipient the recipient of the message
     * @return the requested encryption key
     * @throws WfException if the encryption key cannot be retrieved
     */
    private static final WfEncryptionKey getEncryptionKey(WfEncryptionMethod method, WfAccount originator, WfAccount recipient) throws WfException {
        switch (method) {
            case AES_256_CTR_ECDH:
                return generateNegotiatedKey(originator, recipient);
            case AES_256_CTR_PSK:
                return getSharedKey(recipient);
            default:
                throw new WfException("Cannot retrieve encryption key for encryption method " + method.fieldValue + "(" + method.cipherName + ")", null, WF_CRYPTO_ERROR);
        }
    }

    /**
     * Retrieves a pre-shared encryption key
     * @since 1.1
     * @param recipient the recipient of the message
     * @return the requested pre-shared encryption key
     * @throws WfException if no key for recipient
     */
    private static final WfEncryptionKey getSharedKey(WfAccount recipient) throws WfException {
        WfEncryptionKey key = recipient.getSharedKey();
        if (key == null) throw new WfException("Missing pre-shared key with recipient", null, WF_METADATA_ERROR);
        return key;
    }

    /**
     * Generates a negotiated encryption key for originator and recipient
     * @since 1.1
     * @param originator the originator of the message
     * @param recipient the recipient of the message
     * @return the requested negotiated encryption key
     * @throws WfException if the negotiated encryption key cannot be generated
     */
    private static final WfEncryptionKey generateNegotiatedKey(WfAccount originator, WfAccount recipient) throws WfException {
        /* Determine whose key pair and public key to use */
        WfECDHKeyPair ecdhKeypair;
        ECPublicKey ecdhPublicKey;
        if (originator.isSelf()) {
            ecdhKeypair = originator.getEcdhKeyPair();
            if (ecdhKeypair == null) throw new WfException("Missing own ECDH key pair", null, WF_METADATA_ERROR);
            ecdhPublicKey = recipient.getEcdhPublicKey();
            if (ecdhPublicKey == null) throw new WfException("Missing recipient's ECDH public key", null, WF_METADATA_ERROR);
        } else if (recipient.isSelf()) {
            ecdhKeypair = recipient.getEcdhKeyPair();
            if (ecdhKeypair == null) throw new WfException("Missing recipients ECDH key pair", null, WF_METADATA_ERROR);
            ecdhPublicKey = originator.getEcdhPublicKey();
            if (ecdhPublicKey == null) throw new WfException("Missing originator's ECDH public key", null, WF_METADATA_ERROR);
        } else {
            throw new WfException("Cannot encrypt or decrypt message if not the originator or recipient", null, WF_CRYPTO_ERROR);
        }
        /* Generate negotiated key */
        try {
            return new WfEncryptionKey(ecdhPublicKey, ecdhKeypair);
        } catch (WfCryptoException e) {
            throw new WfException("Could not generate negotiated encryption key: " + e.getMessage(), e, WF_CRYPTO_ERROR);
        }
    }
}

