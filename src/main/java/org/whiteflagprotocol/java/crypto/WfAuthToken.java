/*
 * Whiteflag Java Library
 */
package org.whiteflagprotocol.java.crypto;

import javax.security.auth.Destroyable;

/* Whiteflag authentication methods */
import static org.whiteflagprotocol.java.crypto.WfAuthMethod.*;

/* Static import of cryptographic utility functions */
import static org.whiteflagprotocol.java.crypto.WfCryptoUtil.convertToByteArray;
import static org.whiteflagprotocol.java.crypto.WfCryptoUtil.convertToHexString;

import java.util.Arrays;

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
 * 
 * @since 1.1
 */
public final class WfAuthToken implements Destroyable {

    /* PROPERTIES */
    /**
     * The {@link WfAuthMethod} for which this token is valid
     */
    public final WfAuthMethod method;

    /* Status of the instance */
    private boolean destroyed = false;

    /* The secret authentication token */
    private final byte[] token;

    /* CONSTRUCTOR */

    /**
     * Constructs a new Whiteflag authentication token
     * @param secret a hexadecimal string with the shared secret used as an authentication token
     */
    public WfAuthToken(final String secret) {
        this(convertToByteArray(secret));
    }

    /**
     * Constructs a new Whiteflag authentication token
     * @param secret a byte array with the shared secret used as an authentication token
     */
    public WfAuthToken(final byte[] secret) {
        this.token = Arrays.copyOf(secret, secret.length);
        this.method = TOKEN_PRESHARED;
    }

    /* PUBLIC METHODS */

    /**
     * Destroys this Whiteflag authentication token by clearing the shared secret
     */
    @Override
    public final void destroy() {
        WfCryptoUtil.zeroise(token);
        this.destroyed = true;
    }

    /**
     * Determine if this Whiteflag cipher has been destroyed.
     * @return TRUE if destroyed, else FALSE
     */
    @Override
    public final boolean isDestroyed() {
        return destroyed;
    }

    /**
     * Generates the Whiteflag verification data to prove possession of the token
     * @param context a hexadecimal string with information to bind the derived key to the intended context
     * @return a hexadecimal string with the verification data
     * @throws IllegalArgumentException if the authentication token has been destroyed
     */
    public final String getVerificationData(String context) {
        byte[] verificationData = getVerificationData(convertToByteArray(context));
        return convertToHexString(verificationData);
    }

    /**
     * Generates the Whiteflag verification data to prove possession of the token
     * @param context a byte array with information to bind the derived key to the intended context
     * @return a byte array with the verification data
     * @throws IllegalArgumentException if the authentication token has been destroyed
     */
    public final byte[] getVerificationData(byte[] context) {
        checkState();
        return WfCryptoUtil.hkdf(token, method.hkdfSalt, context, method.tokenLength);
    }

    /* PRIVATE METHODS */

    /**
     * Checks the state of this authentication token
     * @throws IllegalStateException if in an illegal state
     */
    private final void checkState() {
        if (destroyed) throw new IllegalStateException("Authentication token has been destroyed");
    }
}
