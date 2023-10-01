/*
 * Whiteflag Java Library
 */
package org.whiteflagprotocol.java.util;

/**
 * Whiteflag utility runtime exception class
 * 
 * <p> Runtime exception to throw if no Whiteflag message schema available,
 * which is unrecoverbale.
 * 
 * @since 1.2
 */
public class WfMissingSchemaException extends RuntimeException {

    /* CONSTRUCTORS */

    /**
     * Creates a Whiteflag utility runtime exception
     * @param message String containing the human readible error message
     * @param type the error type
     */
    public WfMissingSchemaException(final String message, final Throwable cause) {
        super(message, cause);
    }
}
