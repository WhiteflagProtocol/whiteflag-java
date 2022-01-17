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
import java.security.interfaces.ECPublicKey;

/**
 * Whiteflag encryption key class
 * 
 * <p> This class represents a Whiteflag encryption key. Instances of this
 * class represent the raw key, either pre-shared or negotiated, from which
 * the actual key material for encryption methods 1 and 2 is created.
 * 
 * @wfver v1-draft.6
 * @wfref 5.2.3 Key and Token Derivation
 * @wfref 5.2.4 Message Encryption
 * 
 * @since 1.1
 */
public class WfEncryptionKey implements Destroyable {

    /* PROPERTIES */

    /* Status of the instance */
    private boolean destroyed = false;

    /* The encryption method and keys */
    /**
     * The {@link WfEncryptionMethod} for which this key is valid
     */
    public final WfEncryptionMethod method;

    /* The raw key materials */
    private final byte[] rawkey;
    private final byte[] prk;

    /* CONSTRUCTORS */

    /**
     * Constructs a new Whiteflag encryption key from a raw pre-shared key
     * @param rawSharedKey a hexadecimal string with the raw pre-shared encryption key
     */
    public WfEncryptionKey(String rawSharedKey) {
        this(WfCryptoUtil.convertToByteArray(rawSharedKey));
    }

    /**
     * Constructs a new Whiteflag encryption key from a raw pre-shared key
     * @param rawSharedKey a byte array with the raw pre-shared encryption key
     */
    public WfEncryptionKey(byte[] rawSharedKey) {
        this.rawkey = rawSharedKey;
        this.method = AES_256_CTR_PSK;
        this.prk = WfCryptoUtil.hkdfExtract(rawkey, method.getSalt());
    }

    /**
     * Constructs a new Whiteflag encryption key through ECDH key negotiation
     * @param rawPublicKey a hexadecimal string with an originator's raw 264-bit compressed public ECDH key
     * @param ecdhKeyPair the own ECDH key pair object
     * @throws GeneralSecurityException if the encryption key cannot be created
     */
    public WfEncryptionKey(String rawPublicKey, WfECDHKeyPair ecdhKeyPair) throws GeneralSecurityException {
        this(WfCryptoUtil.convertToByteArray(rawPublicKey), ecdhKeyPair);
    }

    /**
     * Constructs a new Whiteflag encryption key through ECDH key negotiation
     * @param rawPublicKey a byte array with an originator's raw 264-bit compressed public ECDH key
     * @param ecdhKeyPair the own ECDH key pair object
     * @throws GeneralSecurityException if the encryption key cannot be created
     */
    public WfEncryptionKey(byte[] rawPublicKey, WfECDHKeyPair ecdhKeyPair) throws GeneralSecurityException {
        this(WfECDHKeyPair.createPublicKey(rawPublicKey), ecdhKeyPair);
    }

        /**
     * Constructs a new Whiteflag encryption key through ECDH key negotiation
     * @param ecPublicKey a {@link java.security.interfaces.ECPublicKey} ECDH public key
     * @param ecdhKeyPair the own ECDH key pair object
     * @throws GeneralSecurityException if the encryption key cannot be created
     */
    public WfEncryptionKey(ECPublicKey ecPublicKey, WfECDHKeyPair ecdhKeyPair) throws GeneralSecurityException {
        this.rawkey = ecdhKeyPair.getSharedKey(ecPublicKey);
        this.method = AES_256_CTR_ECDH;
        this.prk = WfCryptoUtil.hkdfExtract(rawkey, method.getSalt());
    }

    /* PUBLIC METHODS */

    /**
     * Destroys this Whiteflag cipher by clearing the encryption key
     */
    @Override
    public void destroy() {
        WfCryptoUtil.zeroise(rawkey);
        WfCryptoUtil.zeroise(prk);
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
        return method;
    }

    /**
     * Derive the secret cryptographic key from this Whiteflag encryption key
     * @param context byte array with information to bind the derived key to the intended context
     * @return a java SecretKey object with the secret cryptographic key
     * @throws IllegalArgumentException if this Whiteflag encryption key has been destroyed 
     */
    public SecretKey getSecretKey(byte[] context) {
        if (destroyed) {
            throw new IllegalArgumentException("Cannot create a secret key from a destroyed Whiteflag encryption key");
        }
        return new SecretKeySpec(
            WfCryptoUtil.hkdfExpand(prk, context, method.getKeyLength()),
            method.getAlgorithm()
        );
    }
}
