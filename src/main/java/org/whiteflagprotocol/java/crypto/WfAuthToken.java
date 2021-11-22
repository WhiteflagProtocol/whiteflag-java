/*
 * Whiteflag Java Library
 */
package org.whiteflagprotocol.java.crypto;

/**
 * Whiteflag authentication token class
 * 
 * <p> This class represents a Whiteflag shared secret authentication token.
 * Instances of this class represent the shared secret, and validation data
 * for authentication method 2 can be created.
 * 
 * @wfver v1-draft.6
 * 
 * @wfref 5.1.2.2 Method 2: Shared Token Validation
 * @wfref 5.2.3 Key and Token Derivation
 */
public class WfAuthToken {

    /* PROPERTIES */

    /* The secret authentication token */
    public final int method;
    private final byte[] token;

    /* Constants */
    private static final int METHOD_AUTHTOKEN = 2;

    /**
     * HKDF salts array
     * 
     * <p> This array contains the HKDF salts, with the elements corresponding
     * to the valid authentication methods for which this token class may be used.
     * 
     * @wfver v1-draft.6
     * @wfref 5.2.3 Key and Token Derivation
     */
    public static final String[] hkdfSalts = {
        "",   // non-existing authentication method
        "",   // this authentication method does not use token
        "420abc48f5d69328c457d61725d3fd7af2883cad8460976167e375b9f2c14081"
    };

    /**
     * HKDF validation token lengths array
     * 
     * <p> This array contains the output key lengths, with the elements
     * corresponding to the valid encryption methods for which this key class
     * may be used.
     * 
     * @wfref 5.2.3 Key and Token Derivation
     */
    public static final int[] tokenLengths = {
        0,
        0,
        32
    };

    /* CONSTRUCTOR */

    /**
     * Constructs a new Whiteflag authentication token
     * @param token the shared secret authentication token
     */
    public WfAuthToken(String token) {
        this(WfCryptoUtil.parseHexString(token));
    }

    /**
     * Constructs a new Whiteflag authentication token
     * @param token the shared secret authentication token
     */
    public WfAuthToken(byte[] token) {
        this.token = token;
        this.method = METHOD_AUTHTOKEN;
    }

    /* PUBLIC METHODS */

    /**
     * Generated the Whiteflag verification data from the shared token
     * @param blockchainAddress the blockchain address
     * @return a byte array with the verification data
     */
    public byte[] getVerificationData(byte[] blockchainAddress) {
        return WfCryptoUtil.hkdf(
            token,
            WfCryptoUtil.parseHexString(hkdfSalts[method]),
            blockchainAddress,
            tokenLengths[method]
        );
    }
}
