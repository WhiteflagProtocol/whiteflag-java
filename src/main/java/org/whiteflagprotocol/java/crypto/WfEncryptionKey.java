/*
 * Whiteflag Java Library
 */
package org.whiteflagprotocol.java.crypto;

import javax.crypto.SecretKey;
import javax.crypto.spec.SecretKeySpec;
import javax.security.auth.Destroyable;

/* Whiteflag encryption methods */ 
import static org.whiteflagprotocol.java.crypto.WfEncryptionMethod.*;

import java.security.GeneralSecurityException;

/**
 * Whiteflag encryption key class
 * 
 * <p> This class represents a Whiteflag encryption key. Instances of this
 * class represent the raw key, either pre-shared or negotiated, from which
 * the actual key material for encryption methods 1 and 2 is created.
 * 
 * @wfver v1-draft.6
 * @wfref 5.2.3 Key and Token Derivation
 * 
 * @since 1.1
 */
public class WfEncryptionKey implements Destroyable {

    /* PROPERTIES */

    /* Status of the instance */
    private boolean destroyed = false;

    /* The encryption method and keys */
    public final WfEncryptionMethod encryptionMethod;
    private final byte[] encryptionKey;
    private final byte[] pseudoRandomKey;

    /* CONSTRUCTORS */

    /**
     * Constructs a new Whiteflag encryption key from a raw pre-shared key
     * @param preSharedKey a hexadecimal string with the raw pre-shared encryption key
     */
    public WfEncryptionKey(String preSharedKey) {
        this(WfCryptoUtil.convertToByteArray(preSharedKey));
    }

    /**
     * Constructs a new Whiteflag encryption key from a raw pre-shared key
     * @param preSharedKey a byte array with the raw pre-shared encryption key
     */
    public WfEncryptionKey(byte[] preSharedKey) {
        this.encryptionKey = preSharedKey;
        this.encryptionMethod = AES_256_CTR_PSK;
        this.pseudoRandomKey = WfCryptoUtil.hkdfExtract(encryptionKey, encryptionMethod.getSalt());
    }

    /**
     * Constructs a new Whiteflag encryption key through ECDH key negotiation
     * @param originatorPublicKey a hexadecimal string with an originator's raw ECDH public key
     * @param ecdhKeyPair the own ECDH key pair object
     */
    public WfEncryptionKey(String originatorPublicKey, WfECDHKeyPair ecdhKeyPair) throws GeneralSecurityException {
        this(WfCryptoUtil.convertToByteArray(originatorPublicKey), ecdhKeyPair);
    }

    /**
     * Constructs a new Whiteflag encryption key through ECDH key negotiation
     * @param originatorPublicKey a byte array with an originator's raw ECDH public key
     * @param ecdhKeyPair the own ECDH key pair object
     */
    public WfEncryptionKey(byte[] originatorPublicKey, WfECDHKeyPair ecdhKeyPair) throws GeneralSecurityException {
        this.encryptionKey = ecdhKeyPair.getSharedKey(originatorPublicKey);
        this.encryptionMethod = AES_256_CTR_ECDH;
        this.pseudoRandomKey = WfCryptoUtil.hkdfExtract(encryptionKey, encryptionMethod.getSalt());
    }

    /* PUBLIC METHODS */

    /**
     * Destroys this Whiteflag cipher by clearing the encryption key
     */
    @Override
    public void destroy() {
        WfCryptoUtil.zeroise(encryptionKey);
        WfCryptoUtil.zeroise(pseudoRandomKey);
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
     * Returns the encryption method
     * @return a string with the encryption method indicator
     */
    public WfEncryptionMethod getEncryptionMethod() {
        return encryptionMethod;
    }

    /**
     * Derive the secret cryptographic key from this Whiteflag encryption key
     * @param contextInfo information to bind the derived key to the intended context
     * @return a java SecretKey object with the secret cryptographic key
     * @throws IllegalArgumentException if this Whiteflag encryption key has been destroyed 
     */
    public SecretKey getSecretKey(byte[] contextInfo) {
        if (destroyed) {
            throw new IllegalArgumentException("Cannot create a secret key from a destroyed Whiteflag encryption key");
        }
        return new SecretKeySpec(
            WfCryptoUtil.hkdfExpand(pseudoRandomKey, contextInfo, encryptionMethod.getKeyLength()),
            encryptionMethod.getAlgorithm()
        );
    }
}
