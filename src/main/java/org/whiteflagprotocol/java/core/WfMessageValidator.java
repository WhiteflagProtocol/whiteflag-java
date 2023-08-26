/*
 * Whiteflag Java Library
 */
package org.whiteflagprotocol.java.core;

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
public final class WfMessageValidator {

    /* PROPERTIES */

    /**
     *  The Whiteflag message to be validated
     */
    private WfBasicMessage message;

    /* CONSTRUCTOR */

    /** 
     * Prevents this utility class to be instantiated
     */
    public WfMessageValidator(WfBasicMessage message) {
        this.message = message;
    }
}
