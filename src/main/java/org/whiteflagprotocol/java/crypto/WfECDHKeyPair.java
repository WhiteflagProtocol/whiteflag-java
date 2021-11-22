/*
 * Whiteflag Java Library
 */
package org.whiteflagprotocol.java.crypto;

/**
 * Whiteflag ECDH Key Pair class
 *
 * <p> This class represents an Elleptic Curve Diffie-Hellmann key pair
 * used by Whiteflag for cryptographic key negotiation.
 * 
 * @wfver v1-draft.6
 * @wfref 5.2.2 Key Agreement
 */
public class WfECDHKeyPair {

    /* PROPERTIES */

    /* CONSTRUCTOR */

    /**
     * Constructs a new Whiteflag ECDH key pair
     */
    public WfECDHKeyPair() {
        // WIP
    }

    /* PUBLIC METHODS */

    /**
     * Returns the public key of the ECDH key pair
     * @return a byte array with the ECDH public key
     */
    public final byte[] getPublicKey() {
        return new byte[0];  // WIP
    }

    /**
     * Calculates the shared secret with an originator
     * @param originatorPublicKey the originator's public key
     * @return a byte array with the shared secret
     */
    public final byte[] getSharedKey(final byte[] originatorPublicKey) {
        return new byte[0];  // WIP
    }
}