/*
 * Whiteflag Java Library
 */
package org.whiteflagprotocol.java.crypto;

import javax.crypto.Cipher;
import javax.crypto.SecretKey;
import javax.crypto.spec.SecretKeySpec;
import javax.crypto.spec.IvParameterSpec;

/* Whiteflag encryption methods */ 
import static org.whiteflagprotocol.java.crypto.WfEncryptionMethod.*;

/**
 * Whiteflag cipher class
 * 
 * <p> This class represents a Whiteflag cipher. Instances of this are used
 * to encrypt and decrypt a Whiteflag messgae. This class needs to instantiated
 * with a Whiteflag encryption key instance.
 * 
 * @wfver v1-draft.6
 * @wfref 5.2.3 Key and Token Derivation
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
     * Constructs a new Whiteflag cipher based 
     * @param cipher a hexadecimal string with the raw pre-shared encryption key
     * @param key
     */
    public WfCipher(WfEncryptionKey key) throws WfCryptoException {
        try {
            this.cipher = Cipher.getInstance(key.encryptionMethod.getCipher());
            this.key = key;
        } catch(Exception e) {
            throw new WfCryptoException(e.getMessage());
        }
    }

    /* SETTER AND GETTER METHODS */

    /**
     * Sets the context to bind the encryption key; in Whiteflag this is usually the blockchain address of the message originator
     * @param context information to derive a key which is bound to a specific context
     * @return this Whiteflag cipher object
     */
    public final WfCipher setContext(byte[] context) {
        this.context = context;
        secretKey = new SecretKeySpec(key.getSecretKey(context), key.encryptionMethod.getAlgorithm());
        return this;
    }

    /**
     * Gets the current context to which the encryption key is bound
     * @return a byte array with the context, typically the blockchain address of the message originator
     */
    public final byte[] getContext() {
        return context;
    }

    /**
     * Sets the initialisation vector, which is required by certain Whiteflag encryption methods
     * @param initialisationVector a hexadecimal sting with the initialisation vector
     * @return this Whiteflag cipher object
     */
    public final WfCipher setInitVector(String data) {
        return setInitVector(WfCryptoUtil.parseHexString(data));
    }

    /**
     * Sets the initialisation vector, which is required by certain Whiteflag encryption methods
     * @param initialisationVector a byte array with the initialisation vector
     * @return this Whiteflag cipher object
     */
    public final WfCipher setInitVector(byte[] initialisationVector) {
        this.iv = new IvParameterSpec(initialisationVector);
        return this;
    }

    /**
     * Gets the current initialisation vector
     * @return a byte array with the initialisation vector
     */
    public final byte[] getInitVector() {
        return context;
    }

    /* ENCRYPTION AND DECRYPTION METHODS */

    /**
     * Encrypts the provided data
     * @param data a hexadecimal string with the data to be encrypted
     * @return hexadecimal string with the encrypted data
     */
    public final String encrypt(final String data) throws WfCryptoException {
        return WfCryptoUtil.toHexString((WfCryptoUtil.parseHexString(data)));
    }

    /**
     * Encrypts the provided data
     * @param data a byte array the data to be encrypted
     * @return byte array with the encrypted data
     */
    public final byte[] encrypt(final byte[] data) throws WfCryptoException {
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
     * @return hexadecimal string with the decrypted data
     */
    public final String decrypt(final String data) throws WfCryptoException {
        return  WfCryptoUtil.toHexString(decrypt(WfCryptoUtil.parseHexString(data)));
    }

    /**
     * Decrypts the provided data
     * @param data a byte array the data to be decrypted
     * @return byte array with the decrypted data
     */
    public final byte[] decrypt(final byte[] data) throws WfCryptoException {
        try {
            cipher.init(Cipher.DECRYPT_MODE, secretKey, iv);
            return cipher.doFinal(data);
        } catch(Exception e) {
            throw new WfCryptoException(e.getMessage());
        }
    }
}
