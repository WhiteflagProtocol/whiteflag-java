/*
 * Whiteflag Java Library
 */
package org.whiteflag.protocol;

/* Required Whiteflag core exception class to extend */
import org.whiteflag.protocol.core.WfCoreException;

public class WfException extends WfCoreException {

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
     * Whiteflag protocol error categories
     * 
     * </p> These error categories are used by the {@link WfCoreException} class to
     * specify the type of error.
     */
    public enum ErrorType {

        /**
         * Generic Whiteflag protocol error
         */
        WF_PROTOCOL_ERROR,

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
         * Whiteflag message authentication error
         */
        WF_AUTH_ERROR,

        /**
         * Whiteflag digital signature error
         */
        WF_SIGN_ERROR,

        /**
         * Whiteflag encryption error
         */
        WF_ENCRYPTION_ERROR
    }

    /* CONSTRUCTORS */

    /**
     * Creates a Whiteflag exception with a specific Whiteflag error type
     * @param errorMessage String containing the human readible error message
     * @param wfErrorType The Whiteflag {@link ErrorType} 
     */
    public WfException(final String errorMessage, final ErrorType wfErrorType) {
        super(errorMessage);
        this.errorType = wfErrorType;
    }
}