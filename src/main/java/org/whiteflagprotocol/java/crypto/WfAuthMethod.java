/*
 * Whiteflag Java Library
 */
package org.whiteflagprotocol.java.crypto;

/* Static import of cryptographic utility functions */
import static org.whiteflagprotocol.java.crypto.WfCryptoUtil.convertToByteArray;

/**
 * Whiteflag authentication parameters enum class
 *
 * <p> This is a non-instantiatable enum class that holds all
 * authentication parameters in accordance with the Whiteflag specification.
 * No implementation specific properties and methods are defined by this class.
 * 
 * @wfver v1-draft.6
 * @wfref 5.2.3 Key and Token Derivation
 * 
 * @since 1.1
 */
public enum WfAuthMethod {
    /**
     * Authentication Method 2: shared token
     */
    TOKEN_PRESHARED("2", 32, "420abc48f5d69328c457d61725d3fd7af2883cad8460976167e375b9f2c14081");

    /* PROPERTIES */
    /**
     * The indicator used in a Whiteflag messgae for this authentication method
     */
    public final String indicatorValue;
    /**
     * The byte length of the validation token for this authentication method
     */
    public final int tokenLength;
    /**
     * The salt used by this authentication method in the HKDF function to derive the validation data
     */
    public final byte[] hkdfSalt;

    /* METHODS */

    /* Constructor */
    /**
     * @param indicatorValue value used in a Whiteflag message to indicate the authentication method
     * @param tokenLength the length of the validation token sent in an authentication message
     * @param hkdfSalt a hexadecimal string with the salt used in the HKDF function to derive the validation token
     */
    private WfAuthMethod(final String indicatorValue, final int tokenLength, final String hkdfSalt) {
        this.indicatorValue = indicatorValue;
        this.tokenLength = tokenLength;
        this.hkdfSalt = convertToByteArray(hkdfSalt);
    }
}
