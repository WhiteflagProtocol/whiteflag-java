/*
 * Whiteflag Java Library
 */
package org.whiteflagprotocol.java.crypto;

import java.security.interfaces.ECPublicKey;

import javax.crypto.SecretKey;
import javax.crypto.spec.SecretKeySpec;
import javax.security.auth.Destroyable;

/* Whiteflag encryption methods */ 
import static org.whiteflagprotocol.java.crypto.WfEncryptionMethod.*;

/* Static import of cryptographic utility functions */
import static org.whiteflagprotocol.java.crypto.WfCryptoUtil.convertToByteArray;

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
public final class WfEncryptionKey implements Destroyable {

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
     * @param rawPreSharedKey a hexadecimal string with the raw pre-shared encryption key
     */
    public WfEncryptionKey(String rawPreSharedKey) {
        this(convertToByteArray(rawPreSharedKey));
    }

    /**
     * Constructs a new Whiteflag encryption key from a raw pre-shared key
     * @param rawPreSharedKey a byte array with the raw pre-shared encryption key
     */
    public WfEncryptionKey(byte[] rawPreSharedKey) {
        this.rawkey = rawPreSharedKey;
        this.method = AES_256_CTR_PSK;
        this.prk = WfCryptoUtil.hkdfExtract(rawkey, method.hkdfSalt);
    }

    /**
     * Constructs a new Whiteflag encryption key through ECDH key negotiation
     * @param rawPublicKey a hexadecimal string with an originator's raw 264-bit compressed public ECDH key
     * @param ecdhKeyPair the own ECDH key pair object
     * @throws WfCryptoException if the encryption key cannot be created
     * @throws IllegalStateException if the key pair has been destroyed
     */
    public WfEncryptionKey(String rawPublicKey, WfECDHKeyPair ecdhKeyPair) throws WfCryptoException {
        this(convertToByteArray(rawPublicKey), ecdhKeyPair);
    }

    /**
     * Constructs a new Whiteflag encryption key through ECDH key negotiation
     * @param rawPublicKey a byte array with an originator's raw 264-bit compressed public ECDH key
     * @param ecdhKeyPair the own ECDH key pair object
     * @throws WfCryptoException if the encryption key cannot be created
     * @throws IllegalStateException if the key pair has been destroyed
     */
    public WfEncryptionKey(byte[] rawPublicKey, WfECDHKeyPair ecdhKeyPair) throws WfCryptoException {
        this.rawkey = ecdhKeyPair.negotiateKey(rawPublicKey);
        this.method = AES_256_CTR_ECDH;
        this.prk = WfCryptoUtil.hkdfExtract(rawkey, method.hkdfSalt);
    }

    /**
     * Constructs a new Whiteflag encryption key through ECDH key negotiation
     * @param ecPublicKey a {@link java.security.interfaces.ECPublicKey} ECDH public key
     * @param ecdhKeyPair the own ECDH key pair object
     * @throws WfCryptoException if the encryption key cannot be created
     * @throws IllegalStateException if the key pair has been destroyed
     */
    public WfEncryptionKey(ECPublicKey ecPublicKey, WfECDHKeyPair ecdhKeyPair) throws WfCryptoException {
        this.rawkey = ecdhKeyPair.negotiateKey(ecPublicKey);
        this.method = AES_256_CTR_ECDH;
        this.prk = WfCryptoUtil.hkdfExtract(rawkey, method.hkdfSalt);
    }

    /* PUBLIC METHODS */

    /**
     * Destroys this Whiteflag cipher by clearing the encryption key
     */
    @Override
    public final void destroy() {
        WfCryptoUtil.zeroise(rawkey);
        WfCryptoUtil.zeroise(prk);
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
     * Returns the encryption method
     * @return a string with the encryption method indicator
     */
    public final WfEncryptionMethod getEncryptionMethod() {
        return method;
    }

    /**
     * Derive the secret cryptographic key from this Whiteflag encryption key
     * @param context a hexadecimal string with information to bind the derived key to the intended context
     * @return a java SecretKey object with the secret cryptographic key
     * @throws IllegalArgumentException if the encryption key has been destroyed 
     */
    public final SecretKey getSecretKey(String context) {
        return getSecretKey(convertToByteArray(context));
    }

    /**
     * Derive the secret cryptographic key from this Whiteflag encryption key
     * @param context a byte array with information to bind the derived key to the intended context
     * @return a java SecretKey object with the secret cryptographic key
     * @throws IllegalArgumentException if the encryption key has been destroyed
     */
    public final SecretKey getSecretKey(byte[] context) {
        checkState();
        return new SecretKeySpec(
            WfCryptoUtil.hkdfExpand(prk, context, method.keyLength),
            method.algorithmName
        );
    }

    /* PRIVATE METHODS */

    /**
     * Checks the state of this encryption key
     * @throws IllegalStateException if in an illegal state
     */
    private final void checkState() {
        if (destroyed) throw new IllegalStateException("Encryption key has been destroyed");
    }
}
