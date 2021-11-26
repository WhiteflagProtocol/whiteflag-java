/*
 * Whiteflag Java Library
 */
package org.whiteflagprotocol.java.crypto;

/* Whiteflag authentication methods */
import static org.whiteflagprotocol.java.crypto.WfAuthenticationMethod.*;

/**
 * Whiteflag authentication token class
 * 
 * <p> This class represents a Whiteflag shared secret authentication token
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
    public final WfAuthenticationMethod authMethod;
    private final byte[] authToken;

    /* CONSTRUCTOR */

    /**
     * Constructs a new Whiteflag authentication token
     * @param authToken the shared secret authentication token
     */
    public WfAuthicationToken(String authToken) {
        this(WfCryptoUtil.parseHexString(authToken));
    }

    /**
     * Constructs a new Whiteflag authentication token
     * @param authToken the shared secret authentication token
     */
    public WfAuthicationToken(byte[] authToken) {
        this.authToken = authToken;
        this.authMethod = TOKEN_PRESHARED;
    }

    /* PUBLIC METHODS */

    /**
     * Generated the Whiteflag verification data from the shared token
     * @param contextInfo information to bind the derived key to the intended context
     * @return a byte array with the verification data
     */
    public byte[] getVerificationData(byte[] contextInfo) {
        return WfCryptoUtil.hkdf(
            authToken,
            WfCryptoUtil.parseHexString(authMethod.getSalt()),
            contextInfo,
            authMethod.getTokenLength()
        );
    }
}
