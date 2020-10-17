/*
 * Whiteflag Java Library
 */
package org.whiteflag.protocol.util;

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
     * Whiteflag protocol error categories
     * 
     * </p> These error categories are used by the {@link WfUtilException} class to
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
     * Creates a Whiteflag exception with a specific Whiteflag error type
     * @param errorMessage String containing the human readible error message
     * @param wfUtilErrorType The Whiteflag {@link ErrorType} 
     */
    public WfUtilException(final String errorMessage, final ErrorType wfUtilErrorType) {
        super(errorMessage);
        this.errorType = wfUtilErrorType;
    }
}