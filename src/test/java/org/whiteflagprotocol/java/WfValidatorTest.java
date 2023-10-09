/*
 * Whiteflag Java Library tests
 */
package org.whiteflagprotocol.java;

import org.junit.Test;
import static org.junit.Assert.*;

/**
 * Whiteflag message validator test class
 */
public class WfValidatorTest {
    /**
     * Tests for correctly constructed cryptographic message with header and body object
     */
    @Test
    public void testCryptoMessageValidation() throws WfException {
        /* Setup */
        final String[] fieldValues = { "WF", "1", "0", "0", "K", "0", "0000000000000000000000000000000000000000000000000000000000000000",
                                       "11", "d426bbe111221675e333f30ef608b1aa6e60a47080dd33cb49e96395894ef42f"
                                    };
        WfMessage message = WfMessage.compile(fieldValues);
        WfValidator validator = WfValidator.create(message);

        /* Verify */
        assertTrue("The JSON representation of the message should be valid", validator.validateJson());
    }
    /**
     * Tests for correctly constructed authentication message with header and body object
     */
    @Test
    public void testAuthMessageValidation() throws WfException {
        /* Setup */
        final String[] fieldValues = { "WF", "1", "0", "0", "A", "0", "3efb4e0cfa83122b242634254c1920a769d615dfcc4c670bb53eb6f12843c3ae",
                                       "1", "b01218a30dd3c23d050af254bfcce31a715fecdff6a23fd59609612e6e0ef263"
                                    };
        WfMessage message = WfMessage.compile(fieldValues);
        WfValidator validator = WfValidator.create(message);

        /* Verify */
        assertTrue("The JSON representation of the message should be valid", validator.validateJson());
    }
}