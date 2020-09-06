package org.whiteflag.protocol;

/**
 * Whiteflag state test class
 */
import org.junit.Test;
import static org.junit.Assert.*;

public class WfStateTest {
    @Test public void testStateIsValid() {
        WfState wfState = new WfState();
        assertTrue("Whiteflag state should be valid", wfState.isValid());
    }
}
