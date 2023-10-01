/*
 * Whiteflag Java Library
 */
package org.whiteflagprotocol.java;

import org.whiteflagprotocol.java.util.WfMessageSchema;

/**
 * Whiteflag message validator class
 *
 * <p> This unitlity class is used to validate Whiteflag messages. It uses
 * static factory methods to create a validator. This allows to create
 * validators for specific situations, e.g. from validating a single message
 * to validating a chain of related messages.
 * 
 * <p> The validator uses the Whiteflag message schema to validate against.
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
    private WfValidator(WfMessage message) {
        this.message = message;
    }

    /* STATIC FACTORY METHODS */

    /**
     * Creates a new Whiteflag validator object for validation of the provided Whiteflag message
     * @param message the Whiteflag message to be validated
     * @return the Whiteflag validator object
     */
    public static WfValidator create(WfMessage message) {
        return new WfValidator(message);
    }

    /* PUBLIC METHODS */

    /**
     * Gets the type of this message
     * @return the message type
     */
    public boolean validate() {
        /* Check internal validity */
        if (!message.isValid()) return false;

        /*
         * TODO: Check validity by holding jeson representation aginst schema
         * https://github.com/networknt/json-schema-validator
         */

        return true;
    }

}
