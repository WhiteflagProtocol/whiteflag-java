/*
 * Whiteflag Java Library
 */
package org.whiteflagprotocol.java.crypto;

import javax.crypto.Cipher;
import javax.crypto.SecretKey;
import javax.crypto.spec.IvParameterSpec;

/* Whiteflag encryption methods */ 
import static org.whiteflagprotocol.java.crypto.WfEncryptionMethod.*;

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
public class WfCipher {

    /* PROPERTIES */

    /* The Whiteflag encryption method and keys */
    private Cipher cipher;
    private WfEncryptionKey key; 
    private SecretKey secretKey;
    private IvParameterSpec iv;
    private byte[] context;

    /* CONSTRUCTOR */

    /**
     * Constructs a new Whiteflag cipher based on the type of the provided Whiteflag encryption key
     * @param key the {@link WfEncryptionKey} encryption key
     */
    public WfCipher(WfEncryptionKey key) throws WfCryptoException {
        try {
            this.key = key;
            this.cipher = Cipher.getInstance(key.encryptionMethod.getCipher());
        } catch(Exception e) {
            throw new WfCryptoException(e.getMessage());
        }
    }

    /* PUBLIC METHODS */

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
     * Sets the initialisation vector, which is required by certain Whiteflag encryption methods
     * @param initialisationVector a hexadecimal string with the initialisation vector
     * @return this Whiteflag cipher object
     */
    public WfCipher setInitVector(String initialisationVector) {
        return setInitVector(WfCryptoUtil.convertToByteArray(initialisationVector));
    }

    /**
     * Sets the initialisation vector, which is required by certain Whiteflag encryption methods
     * @param initialisationVector a byte array with the initialisation vector
     * @return this Whiteflag cipher object
     */
    public WfCipher setInitVector(byte[] initialisationVector) {
        this.iv = new IvParameterSpec(initialisationVector);
        return this;
    }

    /**
     * Gets the current initialisation vector
     * @return a byte array with the initialisation vector
     */
    public byte[] getInitVector() {
        return context;
    }

    /**
     * Encrypts the provided data
     * @param data a hexadecimal string with the data to be encrypted
     * @return a hexadecimal string with the encrypted data
     */
    public String encrypt(final String data) throws WfCryptoException {
        return WfCryptoUtil.convertToHexString(encrypt(WfCryptoUtil.convertToByteArray(data)));
    }

    /**
     * Encrypts the provided data
     * @param data a byte array the data to be encrypted
     * @return a byte array with the encrypted data
     */
    public byte[] encrypt(final byte[] data) throws WfCryptoException {
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
     */
    public String decrypt(final String data) throws WfCryptoException {
        return WfCryptoUtil.convertToHexString(decrypt(WfCryptoUtil.convertToByteArray(data)));
    }

    /**
     * Decrypts the provided data
     * @param data a byte array the data to be decrypted
     * @return a byte array with the decrypted data
     */
    public byte[] decrypt(final byte[] data) throws WfCryptoException {
        try {
            cipher.init(Cipher.DECRYPT_MODE, secretKey, iv);
            return cipher.doFinal(data);
        } catch(Exception e) {
            throw new WfCryptoException(e.getMessage());
        }
    }
}
