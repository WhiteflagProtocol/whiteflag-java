/*
 * Whiteflag Java Library
 */
package org.whiteflagprotocol.java;

import org.whiteflagprotocol.java.util.WfMessageSchema;

/**
 * Whiteflag message validator class
 *
 * <p> This unitlity class is used to validate Whiteflag messages. It uses the
 * Whiteflag message schema to validate against.
 * 
 * WIP: develop validation logic
 * 
 * @since 1.2
 */
public final class WfValidator {

    /* PROPERTIES */

    /**
     *  The Whiteflag message to be validated
     */
    private WfMessage message;

    /* CONSTRUCTOR */

    /** 
     * Creates a new validator for a message
     * @param message the message to be validated
     */
    public WfValidator(WfMessage message) {
        this.message = message;
    }
}
