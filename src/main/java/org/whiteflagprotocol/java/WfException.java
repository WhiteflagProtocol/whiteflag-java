/*
 * Whiteflag Java Library
 */
package org.whiteflagprotocol.java;

/* Required Whiteflag core exception class to extend */
import org.whiteflagprotocol.java.core.WfCoreException;

/**
 * Whiteflag protocol exception class
 * 
 * @since 1.0
 */
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
     * @param errorMessage a human readible error message
     * @param wfErrorType the Whiteflag error type
     */
    public WfException(final String errorMessage, final ErrorType wfErrorType) {
        super(errorMessage);
        this.errorType = wfErrorType;
    }
}