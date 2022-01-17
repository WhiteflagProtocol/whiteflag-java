/*
 * Whiteflag Java Library
 */
package org.whiteflagprotocol.java.crypto;

import java.security.SecureRandom;

import javax.crypto.Cipher;
import javax.crypto.SecretKey;
import javax.crypto.spec.IvParameterSpec;
import javax.security.auth.Destroyable;
import javax.security.auth.DestroyFailedException;

/**
 * Whiteflag cipher class
 * 
 * <p> This class represents a Whiteflag cipher. Instances of this are used
 * to encrypt and decrypt a Whiteflag messages. This class needs to instantiated
 * with a Whiteflag encryption key instance.
 * 
 * @wfver v1-draft.6
 * @wfref 5.2.3 Key and Token Derivation
 * 
 * @since 1.1
 */
public class WfCipher implements Destroyable {

    /* PROPERTIES */

    /* Constants */
    /**
     * The byte length of an initialisation vector
     */
    public static final int IVBYTELENGTH = 16;

    /* Random number generator */
    SecureRandom random = new SecureRandom();

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
     * @param key the {@link WfEncryptionKey} encryption key
     * @throws WfCryptoException if the cipher could not be created
     */
    private WfCipher(WfEncryptionKey key) throws WfCryptoException {
        this.key = key;
        try {
            this.cipher = Cipher.getInstance(key.method.getCipher());
        } catch(Exception e) {
            throw new WfCryptoException(e.getMessage());
        }
    }

    /* STATIC FACTORY METHODS */

    /**
     * Creates a new Whiteflag cipher instance from a Whiteflag encryption key
     * @param key the {@link WfEncryptionKey} encryption key
     * @return a new {@link WfCipher} instance
     * @throws IllegalArgumentException if the encryption key is invalid
     * @throws WfCryptoException if the cipher could not be created
     */
    public static WfCipher fromKey(WfEncryptionKey key) throws WfCryptoException {
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
    public void destroy() throws DestroyFailedException {
        secretKey.destroy();    // Destroy derived key; throws exceptions
        this.key = null;        // Only delete reference
        this.destroyed = true;
    }

    /**
     * Determine if this Whiteflag cipher has been destroyed.
     * @return TRUE if destroyed, else FALSE
     */
    @Override
    public boolean isDestroyed() {
        return destroyed;
    }

    /**
     * Sets the context to bind the encryption key; in Whiteflag this is usually the blockchain address of the message originator
     * @param context a hexadecimal string with the context specific information required to derive the correct key
     * @return this Whiteflag cipher object
     */
    public WfCipher setContext(String context) {
        return setContext(WfCryptoUtil.convertToByteArray(context));
    }

    /**
     * Sets the context to bind the encryption key; in Whiteflag this is usually the blockchain address of the message originator
     * @param context a byte array with the context specific information required to derive the correct key
     * @return this Whiteflag cipher object
     */
    public WfCipher setContext(byte[] context) {
        this.context = context;
        this.secretKey = key.getSecretKey(context);
        return this;
    }

    /**
     * Gets the current context to which the encryption key is bound
     * @return a byte array with the context, typically the blockchain address of the message originator
     */
    public byte[] getContext() {
        return context;
    }

    /** 
     * Generates a new random initialisation vector. This should be used when encrypting a new message.
     * @return this Whiteflag cipher object
     */
    public WfCipher setInitVector() {
        byte[] initialisationVector = new byte[IVBYTELENGTH];
        random.nextBytes(initialisationVector);
        this.iv = new IvParameterSpec(initialisationVector);
        return this;
    }

    /**
     * Sets the initialisation vector. This should only be used to decrypt a message.
     * @param initialisationVector a hexadecimal string with the initialisation vector
     * @return this Whiteflag cipher object
     */
    public WfCipher setInitVector(String initialisationVector) {
        return setInitVector(WfCryptoUtil.convertToByteArray(initialisationVector));
    }

    /**
     * Sets the initialisation vector. This should only be used to decrypt a message.
     * @param initialisationVector a byte array with the initialisation vector
     * @return this Whiteflag cipher object
     */
    @SuppressWarnings("java:S3329")
    public WfCipher setInitVector(byte[] initialisationVector) {
        this.iv = new IvParameterSpec(initialisationVector, 0, IVBYTELENGTH);
        return this;
    }

    /**
     * Gets the current initialisation vector
     * @return a byte array with the initialisation vector
     */
    public byte[] getInitVector() {
        return iv.getIV();
    }

    /**
     * Checks if the cipher has been fully set up for encryption or decryption.
     * @return TRUE if cipher has been fully set up, else FALSE
     */
    public Boolean isSet() {
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
    public String encrypt(final String data) throws WfCryptoException {
        return WfCryptoUtil.convertToHexString(encrypt(WfCryptoUtil.convertToByteArray(data)));
    }

    /**
     * Encrypts the provided data
     * @param data a byte array the data to be encrypted
     * @return a byte array with the encrypted data
     * @throws IllegalStateException if this cipher has not been fully set up or keys have been destroyed
     * @throws WfCryptoException if data could not be encrypted
     */
    public byte[] encrypt(final byte[] data) throws WfCryptoException {
        checkState();
        try {
            cipher.init(Cipher.ENCRYPT_MODE, secretKey, iv);
            return cipher.doFinal(data);
        } catch(Exception e) {
            throw new WfCryptoException(e.getMessage());
        }
    }

    /**
     * Decrypts the provided data
     * @param data a hexadecimal string with the data to be decrypted
     * @return a hexadecimal string with the decrypted data
     * @throws WfCryptoException if data could not be decrypted
     */
    public String decrypt(final String data) throws WfCryptoException {
        return WfCryptoUtil.convertToHexString(decrypt(WfCryptoUtil.convertToByteArray(data)));
    }

    /**
     * Decrypts the provided data
     * @param data a byte array the data to be decrypted
     * @return a byte array with the decrypted data
     * @throws IllegalStateException if this cipher has not been fully set up or keys have been destroyed
     * @throws WfCryptoException if data could not be decrypted
     */
    public byte[] decrypt(final byte[] data) throws WfCryptoException {
        checkState();
        try {
            cipher.init(Cipher.DECRYPT_MODE, secretKey, iv);
            return cipher.doFinal(data);
        } catch(Exception e) {
            throw new WfCryptoException(e.getMessage());
        }
    }

    /* PRIVATE METHODS */

    /**
     * Checks the state of this cipher
     * @throws IllegalStateException if in an illegal state 
     */
    private void checkState() {
        if (destroyed) {
            throw new IllegalStateException("Cipher has been destroyed");
        }
        if (Boolean.FALSE.equals(isSet())) {
            throw new IllegalStateException("Cipher has not been fully set up to perform encryption or decryption");
        }
    }
}
