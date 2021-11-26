/*
 * Whiteflag Java Library
 */
package org.whiteflagprotocol.java.crypto;

/**
 * Whiteflag encryption parameters enum class
 *
 * <p> This is a non-instantiatable enum class that holds all
 * encryption parameters in accordance with the Whiteflag specification.
 * No implementation specific properties and methods are defined by this class.
 * 
 * @wfver v1-draft.6
 * @wfref 5.2.3 Key and Token Derivation
 */
public enum WfEncryptionMethod {
    /**
     * Encryption Method 1: AES-256-CTR with pre-shared key
     */
    AES_256_CTR_PSK("1", "AES", "CTR", "NoPadding", 32, "8ddb03085a2c15e69c35c224bce2952dca7878770724741cbce5a135328be0c0"),

    /**
     * Encryption Method 2: AES-256-CTR with negotiated key
     */
    AES_256_CTR_ECDH("2", "AES", "CTR", "NoPadding", 32, "c4d028bd45c876135e80ef7889835822a6f19a31835557d5854d1334e8497b56");

    /* PROPERTIES */

    /* The valid regex charset of an unencoded field value */
    private final String indicator;
    private final String algorithm;
    private final String mode;
    private final String padding;
    private final int keyLength;
    private final String hkdfSalt;

    /* METHODS */

    /* Constructor */
    /**
     * @param indicator string with value used in a Whiteflag message to indicate the encryption method
     * @param algorithm the encryption algorithm, i.a.w. Java Cryptography Standard Algorithm Names
     * @param mode the encryption mode, i.a.w. Java Cryptography Standard Algorithm Names
     * @param padding the padding, i.a.w. Java Cryptography Standard Algorithm Names
     * @param keyLength the length of the encryption key in byes
     * @param hkdfSalt the salt used in the HKDF function to derive the encryption key
     */
    private WfEncryptionMethod(
        final String indicator,
        final String algorithm,
        final String mode,
        final String padding,
        final int keyLength,
        final String hkdfSalt
    ) {
        this.indicator = indicator;
        this.algorithm = algorithm;
        this.mode = mode;
        this.padding = padding;
        this.keyLength = keyLength;
        this.hkdfSalt = hkdfSalt;
    }

    /**
     * Returns the indicator for the Whiteflag encryption method
     * @return string with value used in a Whiteflag message to indicate the Whiteflag encryption method
     */
    public final String getIndicator() {
        return indicator;
    }

    /**
     * Returns the cipher for the Whiteflag encryption method i.a.w. Java Cryptography Standard Algorithm Names
     * @return the cipher parameters with algorithm, mode and padding
     */
    public final String getCipher() {
        return algorithm + "/" + mode + "/" + padding;
    }

    /**
     * Returns the cipher algorithm for the Whiteflag encryption method, i.a.w. Java Cryptography Standard Algorithm Names
     * @return the encryption algorithm
     */
    public final String getAlgorithm() {
        return algorithm;
    }

        /**
     * Returns the cipher mode for the Whiteflag encryption method, i.a.w. Java Cryptography Standard Algorithm Names
     * @return the encryption mode
     */
    public final String getMode() {
        return mode;
    }

    /**
     * Returns the cipher padding for the Whiteflag encryption method, i.a.w. Java Cryptography Standard Algorithm Names
     * @return the encryption padding
     */
    public final String getPadding() {
        return algorithm;
    }

    /**
     * Returns the encryption key length for the Whiteflag encryption method
     * @return the length of the encryption key in byes
     */
    public final int getKeyLength() {
        return keyLength;
    }

    /**
     * Returns the salt for key derivation for the Whiteflag encryption method
     * @return the salt used in the HKDF function to derive the encryption key
     */
    public final String getSalt() {
        return hkdfSalt;
    }
}
