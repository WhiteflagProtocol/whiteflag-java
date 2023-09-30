/*
 * Whiteflag Java Library
 */
package org.whiteflagprotocol.java.util;

/**
 * Whiteflag protocol utility exception class
 * 
 * @since 1.0
 */
public class WfUtilException extends Exception {

    /* PROPERTIES */

    /**
     *  Serial version UID
     */
    private static final long serialVersionUID = 1L;

    /**
     * Whiteflag util error category as defined by {@link ErrorType}
     */
    public final ErrorType errorType;

    /** 
     * Whiteflag utility error categories
     * 
     * <p> These error categories are used by the {@link WfUtilException} class to
     * specify the type of error.
     */
    public enum ErrorType {

        /**
         * Generic Whiteflag utility error
         */
        WF_UTIL_ERROR,

        /** 
         * Errors when processing (parsing, generating) JSON content
         */
        WF_JSON_ERROR
    }

    /* CONSTRUCTORS */

    /**
     * Creates a Whiteflag utility exception with a causing error and specific utility error type
     * @param message String containing the human readible error message
     * @param cause the causing error, or null
     * @param type the error type
     */
    public WfUtilException(final String message, final Throwable cause, final ErrorType type) {
        super(message, cause);
        this.errorType = type;
    }

    /**
     * Creates a Whiteflag utility exception with a specific utility error type
     * @param message String containing the human readible error message
     * @param type the error type
     */
    public WfUtilException(final String message, final ErrorType type) {
        this.errorType = type;
    }
}
