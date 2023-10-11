/*
 * Whiteflag Java Library
 */
package org.whiteflagprotocol.java;

/**
 * Whiteflag protocol exception class
 * 
 * @since 1.0
 */
public class WfException extends Exception {

    /* PROPERTIES */

    /**
     *  Serial version UID
     */
    private static final long serialVersionUID = 1L;

    /**
     * Whiteflag error category as defined by {@link ErrorType}
     */
    public final ErrorType errorType;

    /** 
     * Whiteflag error categories
     * 
     * <p> These error categories are used by the {@link WfException} class to
     * specify the type of error.
     */
    public enum ErrorType {

        /**
         * Generic Whiteflag protocol error
         */
        WF_GENERIC_ERROR,

        /** 
         * Incorrect or missing Whiteflag message metadata
         */
        WF_METADATA_ERROR,

        /** 
         * Whiteflag message format or field value error
         */
        WF_FORMAT_ERROR,

        /** 
         * Whiteflag message reference error
         */
        WF_REFERENCE_ERROR,

        /**
         * Whiteflag authentication error
         */
        WF_AUTH_ERROR,

        /**
         * Whiteflag digital signature error
         */
        WF_SIGN_ERROR,

        /**
         * Whiteflag cryptographic error
         */
        WF_CRYPTO_ERROR
    }

    /* CONSTRUCTORS */

    /**
     * Creates a Whiteflag exception with a specific error type
     * @param message a human readible error message
     * @param cause the causing error, or null
     * @param type the Whiteflag error type
     */
    public WfException(final String message, final Throwable cause, final ErrorType type) {
        super(message, cause);
        this.errorType = type;
    }
}