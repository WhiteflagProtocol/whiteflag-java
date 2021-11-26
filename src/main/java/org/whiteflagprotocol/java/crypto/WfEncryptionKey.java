/*
 * Whiteflag Java Library
 */
package org.whiteflagprotocol.java.crypto;

import javax.crypto.Mac;
import javax.crypto.SecretKey;

/* Whiteflag encryption methods */ 
import static org.whiteflagprotocol.java.crypto.WfEncryptionMethod.*;

/**
 * Whiteflag encryption key class
 * 
 * <p> This class represents a Whiteflag encryption key. Instances of this
 * class represent the raw key, either pre-shared or negotiated, from which
 * the actual key material for encryption methods 1 and 2 is created.
 * 
 * @wfver v1-draft.6
 * @wfref 5.2.3 Key and Token Derivation
 */
public class WfEncryptionKey {

    /* PROPERTIES */

    /* The encryption method and keys */
    public final WfEncryptionMethod encryptionMethod;
    private final byte[] encryptionKey;
    private final byte[] pseudoRandomKey;

    /* CONSTRUCTOR */

    /**
     * Constructs a new Whiteflag encryption key from a pre-shared key
     * @param preSharedKey a hexadecimal string with the raw pre-shared encryption key
     */
    public WfEncryptionKey(String preSharedKey) {
        this(WfCryptoUtil.parseHexString(preSharedKey));
    }

    /**
     * Constructs a new Whiteflag encryption key from a pre-shared key
     * @param preSharedKey a byte array with the raw pre-shared encryption key
     */
    public WfEncryptionKey(byte[] preSharedKey) {
        this.encryptionKey = preSharedKey;
        this.encryptionMethod = AES_256_CTR_PSK;
        this.pseudoRandomKey = WfCryptoUtil.hkdfExtract(encryptionKey, WfCryptoUtil.parseHexString(encryptionMethod.getSalt()));
    }

    /**
     * Constructs a new Whiteflag encryption key through ECDH key negotiation
     * @param originatorPublicKey a hexadecimal string with an originator's ECDH public key
     * @param ecdhKeyPair the own ECDH key pair object
     */
    public WfEncryptionKey(String originatorPublicKey, WfECDHKeyPair ecdhKeyPair) {
        this(WfCryptoUtil.parseHexString(originatorPublicKey), ecdhKeyPair);
    }

    /**
     * Constructs a new Whiteflag encryption key through ECDH key negotiation
     * @param originatorPublicKey a byte array with an originator's ECDH public key
     * @param ecdhKeyPair the own ECDH key pair object
     */
    public WfEncryptionKey(byte[] originatorPublicKey, WfECDHKeyPair ecdhKeyPair) {
        this.encryptionKey = ecdhKeyPair.getSharedKey(originatorPublicKey);
        this.encryptionMethod = AES_256_CTR_ECDH;
        this.pseudoRandomKey = WfCryptoUtil.hkdfExtract(encryptionKey, WfCryptoUtil.parseHexString(encryptionMethod.getSalt()));
    }

    /* PUBLIC METHODS */
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
     * @return a byte array with the secret cryptographic key
     */
    public byte[] getSecretKey(byte[] contextInfo) {
        return WfCryptoUtil.hkdfExpand(pseudoRandomKey, contextInfo, encryptionMethod.getKeyLength());
    }
}
