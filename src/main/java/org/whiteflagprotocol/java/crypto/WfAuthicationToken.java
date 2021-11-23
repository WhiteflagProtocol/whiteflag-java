/*
 * Whiteflag Java Library
 */
package org.whiteflagprotocol.java.crypto;

/* Whiteflag authentication methods */
import static org.whiteflagprotocol.java.crypto.WfAuthenticationMethod.*;

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
public class WfAuthicationToken {

    /* PROPERTIES */

    /* The secret authentication token */
    public final WfAuthenticationMethod method;
    private final byte[] token;

    /* CONSTRUCTOR */

    /**
     * Constructs a new Whiteflag authentication token
     * @param token the shared secret authentication token
     */
    public WfAuthicationToken(String token) {
        this(WfCryptoUtil.parseHexString(token));
    }

    /**
     * Constructs a new Whiteflag authentication token
     * @param token the shared secret authentication token
     */
    public WfAuthicationToken(byte[] token) {
        this.token = token;
        this.method = TOKEN_PRESHARED;
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
            WfCryptoUtil.parseHexString(method.getHkdfSalt()),
            blockchainAddress,
            method.getTokenLength()
        );
    }
}
