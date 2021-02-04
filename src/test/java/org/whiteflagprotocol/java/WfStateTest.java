/*
 * Whiteflag Java Library tests
 */
package org.whiteflagprotocol.java;

import org.junit.Test;
import static org.junit.Assert.*;

/**
 * Whiteflag state test class
 */
public class WfStateTest {
    /**
     * Tests for correctly constructed state object
     */
    @Test
    public void testStateIsValid() {
        /* Setup */
        WfState wfState = new WfState();

        /* Verify */
        assertTrue("Whiteflag state should be valid", wfState.isValid());
    }
}
