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
     * Tests Hexadecimal String to Byte Array parser
     */
    @Test
    public void testAuthToken1() {
        /* Setup */
        final String secret = "000102030405060708090a0b0c";
        final String context = "6fdb25dc394d5a437d88f15b459406ac6db8b386a49dbfc38c";
        final String verificationData = "a951cb35881ee7f78b05f8476a2193de4556455d48ffcfebcfc8938f4a37a70f";

        /* Verify */
        final WfAuthToken token = new WfAuthToken(secret);
        assertEquals("Authentication token should have the correct authentication indicator", TOKEN_PRESHARED.getIndicator(), token.method.getIndicator());
        assertEquals("Authentication token should give the correct verification data", verificationData, token.getVerificationData(context));
    }
}
