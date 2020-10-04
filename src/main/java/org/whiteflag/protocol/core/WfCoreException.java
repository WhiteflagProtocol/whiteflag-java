/*
 * Whiteflag Java Library
 */
package org.whiteflag.protocol.core;

/**
 * Whiteflag protocol core exception class
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
     * @param errorMessage String containing the human readible error message
     */
    public WfCoreException(final String errorMessage) {
        super(errorMessage);
    }
}
