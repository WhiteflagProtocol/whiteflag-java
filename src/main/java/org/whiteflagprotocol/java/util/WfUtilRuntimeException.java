/*
 * Whiteflag Java Library
 */
package org.whiteflagprotocol.java.util;

/**
 * Whiteflag utilities runtime exception class
 * 
 * <p> Runtime exception for unrecoverable states, typically when a required
 * utility resource, such as the Whiteflag message schema, cannot be loaded.
 * 
 * @since 1.2
 */
public class WfUtilRuntimeException extends RuntimeException {

    /* CONSTRUCTORS */

    /**
     * Creates a Whiteflag utility runtime exception
     * @param message String containing the human readible error message
     * @param cause the underlying error
     */
    public WfUtilRuntimeException(final String message, final Throwable cause) {
        super(message, cause);
    }
}
