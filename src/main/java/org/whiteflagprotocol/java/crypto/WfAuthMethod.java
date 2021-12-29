/*
 * Whiteflag Java Library
 */
package org.whiteflagprotocol.java.crypto;

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

    /* The valid regex charset of an unencoded field value */
    private final String indicator;
    private final int tokenLength;
    private final String hkdfSalt;

    /* METHODS */

    /* Constructor */
    /**
     * @param indicator string with value used in a Whiteflag message to indicate the authentication method
     * @param tokenLength the length of the validation token sent in an authentication message
     * @param hkdfSalt the salt used in the HKDF function to derive the validation token
     */
    private WfAuthMethod(final String indicator, final int tokenLength, final String hkdfSalt) {
        this.indicator = indicator;
        this.tokenLength = tokenLength;
        this.hkdfSalt = hkdfSalt;
    }

    /**
     * Returns the Whiteflag indicator for the authentication method
     * @return string with value used in a Whiteflag message to indicate the authentication method
     */
    public final String getIndicator() {
        return indicator;
    }

    /**
     * Returns the validation token length for the authentication method
     * @return the length of the validation token sent in an authentication message
     */
    public final int getTokenLength() {
        return tokenLength;
    }

    /**
     * Returns the salt for validation token derivation for the authentication method
     * @return the salt used in the HKDF function to derive the validation token
     */
    public final String getSalt() {
        return hkdfSalt;
    }
}
