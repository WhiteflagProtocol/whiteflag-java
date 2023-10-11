/*
 * Whiteflag Java Library tests
 */
package org.whiteflagprotocol.java.core;

import org.junit.Test;
import static org.junit.Assert.*;

import org.whiteflagprotocol.java.core.WfMessageType;
import org.whiteflagprotocol.java.util.WfJsonSchema;

/**
 * Whiteflag binary buffer test class
 */
public class WfMessageTypeTest {

    /**
     * Tests if message type definition contains only existing message types
     */
    @Test
    public void testMessageCodes() {
        for (WfMessageType messageType : WfMessageType.values()) {
            String messageCode = messageType.getCode();
            if (!messageCode.equals("")) {
                assertTrue("Should be a valid message type: "+ messageCode, WfJsonSchema.isValidMessageType(messageCode));
            }
        }
    }
}