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
    public final WfEncryptionMethod method;
    private final byte[] rawKey;

    /* CONSTRUCTOR */

    /**
     * Constructs a new Whiteflag encryption key from a pre-shared key
     * @param rawKey a hexadecimal string with the raw pre-shared encryption key
     */
    public WfEncryptionKey(String rawKey) {
        this(WfCryptoUtil.parseHexString(rawKey));
    }

    /**
     * Constructs a new Whiteflag encryption key from a pre-shared key
     * @param rawKey a byte array with the raw pre-shared encryption key
     */
    public WfEncryptionKey(byte[] rawKey) {
        this.rawKey = rawKey;
        this.method = AES_PRESHARED;
    }

    /**
     * Constructs a new Whiteflag encryption key through ECDH key negotiation
     * @param originatorPublicKey a hexadecimal string with the originator's ECDH public key
     * @param ownKeyPair the own ECDH key pair object
     */
    public WfEncryptionKey(String originatorPublicKey, WfECDHKeyPair ownKeyPair) {
        this(WfCryptoUtil.parseHexString(originatorPublicKey), ownKeyPair);
    }

    /**
     * Constructs a new Whiteflag encryption key through ECDH key negotiation
     * @param originatorPublicKey a byte array with the originator's ECDH public key
     * @param ownKeyPair the own ECDH key pair object
     */
    public WfEncryptionKey(byte[] originatorPublicKey, WfECDHKeyPair ownKeyPair) {
        this.rawKey = ownKeyPair.getSharedKey(originatorPublicKey);
        this.method = AES_NEGOTIATED;
    }

    /* PUBLIC METHODS */

    /**
     * Constructs a new Whiteflag encryption key from a pre-shared key
     * @param blockchainAddress the blockchain address
     * @return a byte array with the cryptographic key
     */
    public byte[] getEncryptionKey(byte[] blockchainAddress) {
        return WfCryptoUtil.hkdf(
            rawKey,
            WfCryptoUtil.parseHexString(method.getHkdfSalt()),
            blockchainAddress,
            method.getKeyLength()
        );
    }
}
