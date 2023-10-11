/*
 * Whiteflag Java Library
 */
package org.whiteflagprotocol.java.core;

/**
 * Whiteflag core exception class
 * 
 * @since 1.0
 */
public class WfCoreException extends Exception {

    /* PROPERTIES */

    /**
     *  Serial version UID
     */
    private static final long serialVersionUID = 1L;

    /* CONSTRUCTORS */

    /**
     * Creates a generic Whiteflag exception
     * @param message a human readible error message
     * @param cause the causing error, or null
     */
    public WfCoreException(final String message, final Throwable cause) {
        super(message, cause);
    }
}
