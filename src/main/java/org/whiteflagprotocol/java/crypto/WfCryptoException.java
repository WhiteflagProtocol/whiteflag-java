/*
 * Whiteflag Java Library
 */
package org.whiteflagprotocol.java.crypto;

/**
 * Whiteflag cryptographic exception class
 * 
 * @since 1.1
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
     * @param message a human readible error message
     * @param cause the causing error, or null
     */
    public WfCryptoException(final String message, final Throwable cause) {
        super(message, cause);
    }
}
