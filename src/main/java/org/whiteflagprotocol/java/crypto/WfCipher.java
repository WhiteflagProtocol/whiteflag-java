/*
 * Whiteflag Java Library
 */
package org.whiteflagprotocol.java.crypto;

import java.security.SecureRandom;
import java.util.Arrays;

import javax.crypto.Cipher;
import javax.crypto.SecretKey;
import javax.crypto.spec.IvParameterSpec;
import javax.security.auth.Destroyable;
import javax.security.auth.DestroyFailedException;

/* Static import of cryptographic utility functions */
import static org.whiteflagprotocol.java.crypto.WfCryptoUtil.convertToByteArray;
import static org.whiteflagprotocol.java.crypto.WfCryptoUtil.convertToHexString;

/**
 * Whiteflag cipher class
 * 
 * <p> This class represents a Whiteflag cipher. Instances of this are used
 * to encrypt and decrypt a Whiteflag messages. This class needs to be 
 * instantiated with a Whiteflag encryption key instance.
 * 
 * @wfver v1-draft.6
 * @wfref 5.2.3 Key and Token Derivation
 * 
 * @since 1.1
 */
public final class WfCipher implements Destroyable {

    /* PROPERTIES */

    /* Constants */
    /**
     * The byte length of an initialisation vector
     */
    public static final int IVBYTELENGTH = 16;

    /* Status of the instance */
    private boolean destroyed = false;

    /* The Whiteflag encryption method and keys */
    private final Cipher cipher;
    private WfEncryptionKey key; 
    private SecretKey secretKey;
    private IvParameterSpec iv;
    private byte[] context;

    /* CONSTRUCTOR */

    /**
     * Constructs a new Whiteflag cipher based on the type of the provided Whiteflag encryption key
     * @param key the encryption key
     * @throws WfCryptoException if the cipher could not be created
     */
    private WfCipher(final WfEncryptionKey key) throws WfCryptoException {
        this.key = key;
        try {
            this.cipher = Cipher.getInstance(key.method.cipherName);
        } catch(Exception e) {
            throw new WfCryptoException("Could not instantiate cryptographic cipher: " + key.method.cipherName, e);
        }
    }

    /* STATIC FACTORY METHODS */

    /**
     * Creates a new Whiteflag cipher instance from a Whiteflag encryption key
     * @param key the encryption key
     * @return a new Whiteflag cipher instance
     * @throws IllegalArgumentException if the encryption key is invalid
     * @throws WfCryptoException if the cipher could not be created
     */
    public static final WfCipher fromKey(final WfEncryptionKey key) throws WfCryptoException {
        if (Boolean.TRUE.equals(key.isDestroyed())) {
            throw new IllegalArgumentException("Cannot create Whiteflag cipher from a destroyed key");
        }
        return new WfCipher(key);
    }

    /* PUBLIC METHODS */

    /**
     * Destroys this Whiteflag cipher by clearing the encryption key
     * @throws DestroyFailedException if the destroy operation fails
     * @throws IllegalStateException if the encryption key has already been destroyed
     */
    @Override
    public final void destroy() throws DestroyFailedException {
        try {
            if (this.secretKey != null) this.secretKey.destroy();
        } catch (DestroyFailedException e) {
            /* For some reason, javax.crypto.SecretKey does not actually implement the destroy() method */
        }
        this.key = null;    // Only delete reference
        this.destroyed = true;
    }

    /**
     * Determine if this Whiteflag cipher has been destroyed.
     * @return TRUE if destroyed, else FALSE
     */
    @Override
    public final boolean isDestroyed() {
        return destroyed;
    }

    /**
     * Sets the context to bind the encryption key; in Whiteflag this is usually the blockchain address of the message originator
     * @param context a hexadecimal string with the context specific information required to derive the correct key
     * @return this Whiteflag cipher object
     */
    public final WfCipher setContext(final String context) {
        return setContext(convertToByteArray(context));
    }

    /**
     * Sets the context to bind the encryption key; in Whiteflag this is usually the blockchain address of the message originator
     * @param context a byte array with the context specific information required to derive the correct key
     * @return this Whiteflag cipher object
     */
    public final WfCipher setContext(final byte[] context) {
        checkDestroyed();
        this.context = context;
        this.secretKey = key.getSecretKey(context);
        return this;
    }

    /** 
     * Generates a new random initialisation vector. This should be used when encrypting a new message.
     * @return a byte array with the random initialisation vector
     * @throws WfCryptoException if a random in initialization vector could not be generated
     */
    public final byte[] setInitVector() throws WfCryptoException {
        checkDestroyed();
        byte[] initialisationVector = new byte[IVBYTELENGTH];
        try {
            SecureRandom.getInstanceStrong().nextBytes(initialisationVector);
            this.iv = new IvParameterSpec(initialisationVector);
        } catch (Exception e) {
            throw new WfCryptoException("Could not generate new random initialisation vector", e);
        }
        return initialisationVector;
    }

    /**
     * Sets the initialisation vector. This should only be used to decrypt a message.
     * @param initialisationVector a hexadecimal string with the initialisation vector
     * @return this Whiteflag cipher object
     */
    public final WfCipher setInitVector(final String initialisationVector) {
        return setInitVector(convertToByteArray(initialisationVector));
    }

    /**
     * Sets the initialisation vector. This should only be used to decrypt a message.
     * @param initialisationVector a byte array with the initialisation vector
     * @return this Whiteflag cipher object
     */
    @SuppressWarnings("java:S3329")
    public final WfCipher setInitVector(final byte[] initialisationVector) {
        checkDestroyed();
        this.iv = new IvParameterSpec(initialisationVector, 0, IVBYTELENGTH);
        return this;
    }

    /**
     * Gets the current initialisation vector
     * @return a byte array with the initialisation vector
     */
    public final byte[] getInitVector() {
        if (iv == null) return new byte[0];
        return iv.getIV();
    }

    /**
     * Checks if the cipher has been fully set up for encryption or decryption.
     * @return TRUE if cipher has been fully set up, else FALSE
     */
    public final boolean isSet() {
        if (context == null || context.length == 0) return false;
        if (iv == null || iv.getIV().length != IVBYTELENGTH) return false;
        return !this.destroyed;
    }

    /**
     * Encrypts the provided data
     * @param data a hexadecimal string with the data to be encrypted
     * @return a hexadecimal string with the encrypted data
     * @throws WfCryptoException if data could not be encrypted
     */
    public final String encrypt(final String data) throws WfCryptoException {
        return convertToHexString(encrypt(convertToByteArray(data)));
    }

    /**
     * Encrypts the provided data
     * @param data a byte array the data to be encrypted
     * @return a byte array with the encrypted data
     * @throws IllegalStateException if this cipher has not been fully set up or keys have been destroyed
     * @throws WfCryptoException if data could not be encrypted
     */
    public final byte[] encrypt(final byte[] data) throws WfCryptoException {
        checkDestroyed();
        checkSet();
        try {
            cipher.init(Cipher.ENCRYPT_MODE, secretKey, iv);
            return cipher.doFinal(data);
        } catch(Exception e) {
            throw new WfCryptoException("Could not encrypt data with " + key.method.cipherName + " cipher", e);
        }
    }

    /**
     * Decrypts the provided data
     * @param data a hexadecimal string with the data to be decrypted
     * @return a hexadecimal string with the decrypted data
     * @throws WfCryptoException if data could not be decrypted
     */
    public final String decrypt(final String data) throws WfCryptoException {
        return convertToHexString(decrypt(convertToByteArray(data)));
    }

    /**
     * Decrypts the provided data
     * @param data a byte array the data to be decrypted
     * @return a byte array with the decrypted data
     * @throws IllegalStateException if this cipher has not been fully set up or keys have been destroyed
     * @throws WfCryptoException if data could not be decrypted
     */
    public final byte[] decrypt(final byte[] data) throws WfCryptoException {
        checkDestroyed();
        checkSet();
        try {
            cipher.init(Cipher.DECRYPT_MODE, secretKey, iv);
            return cipher.doFinal(data);
        } catch(Exception e) {
            throw new WfCryptoException("Could not decrypt data with " + key.method.cipherName + " cipher", e);
        }
    }

    /* PROTECTED METHODS */

    /**
     * Gets the current context to which the encryption key is bound
     * @return a byte array with the context, typically the blockchain address of the message originator
     */
    protected final byte[] getContext() {
        return Arrays.copyOf(context, context.length);
    }

    /* PRIVATE METHODS */

    /**
     * Checks and throws exception if this cipher has been destroyed
     * @throws IllegalStateException if this cipher has been destroyed
     */
    private final void checkDestroyed() {
        if (destroyed) {
            throw new IllegalStateException("Cipher has been destroyed");
        }
    }

    /**
     * Checks and throws exception if this cipher has not been fully set up
     * @throws IllegalStateException if this cipher has not been set up
     */
    private void checkSet() {
        if (Boolean.FALSE.equals(isSet())) {
            throw new IllegalStateException("Context and/or initialisation vector have not been set");
        }
    }
}
