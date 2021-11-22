/*
 * Whiteflag Java Library
 */
package org.whiteflagprotocol.java.crypto;

import javax.crypto.Mac;
import javax.crypto.SecretKey;

/* Required encryption utilities */
import org.whiteflagprotocol.java.crypto.WfCryptoUtil;

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
    public final int method;
    private final byte[] rawKey;

    /* Constants */
    private static final int METHOD_PRESHARED = 1;
    private static final int METHOD_NEGOTIATED = 2;

    /**
     * HKDF salts array
     * 
     * <p> This array contains the HKDF salts, with the elements corresponding
     * to the valid encryption methods for which this key class may be used.
     *
     * @wfver v1-draft.6
     * @wfref 5.2.3 Key and Token Derivation
     */
    public static final String[] hkdfSalts = {
        "",   // no encryption
        "8ddb03085a2c15e69c35c224bce2952dca7878770724741cbce5a135328be0c0",
        "c4d028bd45c876135e80ef7889835822a6f19a31835557d5854d1334e8497b56"
    };

    /**
     * HKDF key lengths array
     * 
     * <p> This array contains the output key lengths, with the elements
     * corresponding to the valid encryption methods for which this key class
     * may be used.
     * 
     * @wfref 5.2.3 Key and Token Derivation
     */
    public static final int[] keyLengths = {
        0,   // no encryption
        32,
        32
    };

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
        this.method = METHOD_PRESHARED;
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
        this.method = METHOD_NEGOTIATED;
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
            WfCryptoUtil.parseHexString(hkdfSalts[method]),
            blockchainAddress,
            keyLengths[method]
        );
    }
}
