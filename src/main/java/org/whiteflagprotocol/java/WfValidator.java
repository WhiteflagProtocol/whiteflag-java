/*
 * Whiteflag Java Library
 */
package org.whiteflagprotocol.java;

import org.whiteflagprotocol.java.util.WfJsonValidator;

/**
 * Whiteflag message validator class
 *
 * <p> This class is used to validate Whiteflag messages. It uses a static
 * factory methods to create a validator. This allows to create different
 * validators for specific situations, e.g. from validating a single message
 * to validating a chain of related messages.
 * 
 * <p> Use cases:
 * - validate an individual message
 * - validate the JSON representation against the JSON message schema
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
     * Validates the Whiteflag message
     * @return TRUE if provided data is valid, else FALSE
     */
    public boolean validate() {
        return message.isValid();
    }

    /**
     * Validates the JSON representation of the Whiteflag message
     * @return TRUE if provided data is valid, else FALSE
     */
    public boolean validateJson() {
        return WfJsonValidator.validateMessage(message.toJsonMessage());
    }
}
