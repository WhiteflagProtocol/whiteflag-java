/*
 * Whiteflag Java Library
 */
package org.whiteflagprotocol.java.crypto;

/* Static import of cryptographic utility functions */
import static org.whiteflagprotocol.java.crypto.WfCryptoUtil.convertToByteArray;

/**
 * Whiteflag encryption parameters enum class
 *
 * <p> This is a non-instantiatable enum class that holds all
 * encryption parameters in accordance with the Whiteflag specification.
 * No implementation specific properties and methods are defined by this class.
 * 
 * @wfver v1-draft.6
 * @wfref 5.2.3 Key and Token Derivation
 * 
 * @since 1.1
 */
public enum WfEncryptionMethod {
    /**
     * Encryption Method 1: AES-256-CTR with negotiated key
     */
    AES_256_CTR_ECDH("1", "AES", "CTR", "NoPadding", 32, "8ddb03085a2c15e69c35c224bce2952dca7878770724741cbce5a135328be0c0"),

    /**
     * Encryption Method : AES-256-CTR with pre-shared key
     */
    AES_256_CTR_PSK("2", "AES", "CTR", "NoPadding", 32, "c4d028bd45c876135e80ef7889835822a6f19a31835557d5854d1334e8497b56");


    /* PROPERTIES */

    /* The valid regex charset of an unencoded field value */
    /**
     * The value used in a Whiteflag message to indicate the encryption method
     */
    public final String indicatorValue;
    /**
     * The name of the algorithm for this encryption method, i.a.w. Java Cryptography Standard Algorithm Names
     */
    public final String algorithmName;
    /**
     * The mode of operation for this encryption method, i.a.w. Java Cryptography Standard Algorithm Names
     */
    public final String operationMode;
    /**
     * The padding scheme for this encryption method, i.a.w. Java Cryptography Standard Algorithm Names
     */
    public final String paddingScheme;
    /**
     * The cipher name for this encryption method i.a.w. Java Cryptography Standard Algorithm Names
     */
    public final String cipherName;
    /**
     * The byte length of the encryption key for this encryption method
     */
    public final int keyLength;
    /**
     * The salt used by this encryption method in the HKDF function to derive the encryption key
     */
    public final byte[] hkdfSalt;

    /* METHODS */

    /* Constructor */
    /**
     * Sets the properties of the encryption methods
     * @param indicatorValue the value used in a Whiteflag message to indicate the encryption method
     * @param algorithmName the name of the encryption algorithm, i.a.w. Java Cryptography Standard Algorithm Names
     * @param operationMode the encryption mode of operation, i.a.w. Java Cryptography Standard Algorithm Names
     * @param paddingScheme the padding scheme, i.a.w. Java Cryptography Standard Algorithm Names
     * @param keyLength the length of the encryption key in bytes
     * @param hkdfSalt the salt used in the HKDF function to derive the encryption key
     */
    private WfEncryptionMethod(
        final String indicatorValue,
        final String algorithmName,
        final String operationMode,
        final String paddingScheme,
        final int keyLength,
        final String hkdfSalt
    ) {
        this.indicatorValue = indicatorValue;
        this.algorithmName = algorithmName;
        this.operationMode = operationMode;
        this.paddingScheme = paddingScheme;
        this.cipherName = algorithmName + "/" + operationMode + "/" + paddingScheme;
        this.keyLength = keyLength;
        this.hkdfSalt = convertToByteArray(hkdfSalt);
    }
}
