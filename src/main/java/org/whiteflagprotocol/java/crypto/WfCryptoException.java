/*
 * Whiteflag Java Library
 */
package org.whiteflagprotocol.java.crypto;

/**
 * Whiteflag protocol cryptographic exception class
 */
public class WfCryptoException extends Exception {

    /* PROPERTIES */

    /**
     *  Serial version UID
     */
    private static final long serialVersionUID = 1L;

    /* CONSTRUCTORS */

    /**
     * Creates a Whiteflag crypto exception
     * @param errorMessage a human readible error message
     */
    public WfCryptoException(final String errorMessage) {
        super(errorMessage);
    }
}
