/*
 * Whiteflag Java Library tests
 */
package org.whiteflagprotocol.java.crypto;

import org.junit.Test;
import static org.junit.Assert.*;

/* Whiteflag authentication methods */
import static org.whiteflagprotocol.java.crypto.WfAuthMethod.*;

/**
 * Whiteflag authentication token test class
 */
public class WfAuthTokenTest {

    /**
     * Tests authentication token
     */
    @Test
    public void testAuthToken1() {
        /* Setup */
        final String secret = "000102030405060708090a0b0c";
        final String context = "6fdb25dc394d5a437d88f15b459406ac6db8b386a49dbfc38c";
        final String verificationData = "a951cb35881ee7f78b05f8476a2193de4556455d48ffcfebcfc8938f4a37a70f";
        final WfAuthToken token = new WfAuthToken(secret);

        /* Verify */
        assertEquals("Authentication token should have the correct authentication indicator", TOKEN_PRESHARED.indicatorValue, token.method.indicatorValue);
        assertEquals("Authentication token should give the correct verification data", verificationData, token.getVerificationData(context));
    }
    /**
     * Tests a
     */
    @Test
    public void testDestroyAuthToken() {
        /* Setup */
        final String secret = "000102030405060708090a0b0c";
        final String context = "6fdb25dc394d5a437d88f15b459406ac6db8b386a49dbfc38c";
        final WfAuthToken token = new WfAuthToken(secret);

        /* Verify */
        token.destroy();
        assertTrue("Authentication token should indicate it is destroyed", token.isDestroyed());
        try {
            token.getVerificationData(context);
            fail("Expected a IllegalStateException to be thrown");
        } catch (Exception e) {
            assertTrue(e instanceof IllegalStateException);
        }
    }
}
